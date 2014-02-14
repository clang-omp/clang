//===--- ParseOpenMP.cpp - OpenMP directives parsing ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file implements parsing of all OpenMP directives and clauses.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Sema/Scope.h"
#include "llvm/ADT/PointerIntPair.h"
#include "RAIIObjectsForParser.h"
using namespace clang;

//===----------------------------------------------------------------------===//
// OpenMP declarative directives.
//===----------------------------------------------------------------------===//

/// \brief Parsing of declarative OpenMP directives.
///
///       threadprivate-directive:
///         annot_pragma_openmp 'threadprivate' simple-variable-list
///         annot_pragma_openmp_end
///
Parser::DeclGroupPtrTy Parser::ParseOpenMPDeclarativeDirective(AccessSpecifier AS) {
  assert(Tok.is(tok::annot_pragma_openmp) && "Not an OpenMP directive!");
  ParenBraceBracketBalancer BalancerRAIIObj(*this);

  SourceLocation Loc = ConsumeAnyToken();
  SmallVector<Expr *, 4> Identifiers;
  OpenMPDirectiveKind DKind = Tok.isAnnotation() ?
                                  OMPD_unknown :
                                  getOpenMPDirectiveKind(PP.getSpelling(Tok));

  switch (DKind) {
  case OMPD_declare: {
    Token SavedToken = PP.LookAhead(0);
    if (!SavedToken.isAnnotation()) {
      StringRef Spelling = PP.getSpelling(SavedToken);
      if (Spelling == "reduction") {
        DKind = OMPD_declare_reduction;
        ConsumeAnyToken();
      }
      else if (Spelling == "simd") {
        DKind = OMPD_declare_simd;
        ConsumeAnyToken();
      }
    }
    }
    break;
  case OMPD_for: {
    // This is to get correct directive name in the error message below.
    // This whole switch actually should be extracted into a helper routine
    // and reused in ParseOpenMPDeclarativeOrExecutableDirective below.
    Token SavedToken = PP.LookAhead(0);
    if (!SavedToken.isAnnotation()) {
      StringRef Spelling = PP.getSpelling(SavedToken);
      if (Spelling == "simd") {
        DKind = OMPD_for_simd;
        ConsumeAnyToken();
      }
    }
    }
    break;
  default:
    break;
  }

  switch (DKind) {
  case OMPD_threadprivate:
    ConsumeAnyToken();
    if (!ParseOpenMPSimpleVarList(OMPD_threadprivate, Identifiers, true)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_threadprivate);
        while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
      }
      // Skip the last annot_pragma_openmp_end.
      ConsumeAnyToken();
      return Actions.ActOnOpenMPThreadprivateDirective(Loc,
                                                       Identifiers);
    }
    break;
  case OMPD_declare_simd: {
      // The syntax is:
      // #pragma omp declare simd
      // [ #pragma omp declare simd
      // ... ]
      // <function-declaration-or-definition>
      //
      SmallVector<OmpDeclareSimdVariantInfo, 4> TI;             // tempopary varlists.
      SmallVector<SourceRange, 4> SrcRanges;  // directives' source ranges.
      SmallVector<unsigned, 4> BeginIdx;      // first clause index in CL.
      SmallVector<unsigned, 4> EndIdx;        // end of clauses index in CL.
      SmallVector<OMPClause *, 4> CL;        // all the clauses.

      for(;;) {
        unsigned CurBegin = CL.size();
        SmallVector<llvm::PointerIntPair<OMPClause *, 1, bool>,
            4> FirstClauses(NUM_OPENMP_CLAUSES);
        if (Tok.isNot(tok::annot_pragma_openmp_end))
          ConsumeAnyToken();

        // Read the clauses of the current simd variant.
        while (Tok.isNot(tok::annot_pragma_openmp_end)) {
          OpenMPClauseKind CKind = Tok.isAnnotation() ?
                                      OMPC_unknown :
                                      getOpenMPClauseKind(PP.getSpelling(Tok));
          if (CKind == OMPC_uniform || CKind == OMPC_aligned ||
              CKind == OMPC_linear) {
            TI.push_back(OmpDeclareSimdVariantInfo(CKind, -1));
            bool HadError = ParseOpenMPDeclarativeVarListClause(
                DKind, CKind,
                TI.back().NameInfos,      // Parsed VarNames.
                TI.back().StartLoc,       // Source loc start.
                TI.back().EndLoc,         // Source loc end.
                TI.back().TailExpr,       // The expr after ':'
                TI.back().TailLoc);       // Source location of the tail expr.
            if (!HadError) {
              TI.back().Idx = CL.size();
              CL.push_back(0);
            }
            else {
              TI.pop_back(); // Revert due to error.
            }
          }
          else {
            OMPClause *Clause = ParseOpenMPClause(DKind, CKind,
                !FirstClauses[CKind].getInt());
            FirstClauses[CKind].setInt(true);
            if (Clause) {
              FirstClauses[CKind].setPointer(Clause);
              CL.push_back(Clause);
            }
          }

          // Skip ',' if any.
          if (Tok.is(tok::comma))
            ConsumeToken();
        }

        // Here we are at the end of current simd variant.
        if (Tok.isNot(tok::annot_pragma_openmp_end)) {
          Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
            << getOpenMPDirectiveName(OMPD_declare_simd);
          while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch)) ;
        }
        // Skip the last annot_pragma_openmp_end.
        ConsumeToken();

        // Save the current simd variant's info.
        {
          SrcRanges.push_back(SourceRange());
          BeginIdx.push_back(CurBegin);
          EndIdx.push_back(CL.size());
        }

        // Check if we have more variants here.
        // If not -- go ahead with parsing the function declaration.
        if (!Tok.is(tok::annot_pragma_openmp))
          break;
        ConsumeToken(); // eat the annotation token
        OpenMPDirectiveKind DKind2 = Tok.isAnnotation() ?
                    OMPD_unknown :
                    getOpenMPDirectiveKind(PP.getSpelling(Tok));
        switch (DKind2) {
        case OMPD_declare: {
          Token SavedToken = PP.LookAhead(0);
          if (!SavedToken.isAnnotation()) {
            StringRef Spelling = PP.getSpelling(SavedToken);
            if (Spelling == "simd") {
              DKind2 = OMPD_declare_simd;
              ConsumeToken();
            }
          }
        }
          break;
        default:
          break;
        }
        if (DKind2 != OMPD_declare_simd) {
          Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
            << getOpenMPDirectiveName(OMPD_declare_simd);
          while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch)) ;
          // Skip the last annot_pragma_openmp_end.
          ConsumeToken();
          break;
        }
      }
      // Here we expect to see some function declaration.
      // TODO What if not?
      ParsedAttributesWithRange attrs(AttrFactory);
      ParsingDeclSpec PDS(*this);
      //DeclGroupPtrTy Ptr = ParseDeclarationOrFunctionDefinition(attrs);
      DeclGroupPtrTy Ptr = ParseExternalDeclaration(attrs, &PDS);
      if (!Ptr || !Ptr.get().isSingleDecl()) return Ptr;
      Decl *FuncDecl = dyn_cast<Decl>(Ptr.get().getSingleDecl());
      // Here we need to convert the saved name-lists to corresponding clauses.
      // This is for 'linear', 'aligned' and 'uniform' clauses only (the
      // rest kinds of clauses are already done in CL array).
      for (unsigned I = 0; I < TI.size(); ++I) {
        assert(CL[TI[I].Idx] == 0);
        CL[TI[I].Idx] = Actions.ActOnOpenMPDeclarativeVarListClause(
                                  TI[I].CKind,
                                  TI[I].NameInfos,
                                  TI[I].StartLoc,
                                  TI[I].EndLoc,
                                  TI[I].TailExpr,
                                  TI[I].TailLoc,
                                  FuncDecl);
      }
      return Actions.ActOnOpenMPDeclareSimdDirective(Loc,
                FuncDecl, SrcRanges, BeginIdx, EndIdx, CL);
    }
    break;
  case OMPD_declare_reduction: {
    SmallVector<QualType, 4> Types;
    SmallVector<SourceRange, 4> TyRanges;
    SmallVector<Expr *, 4> Combiners;
    SmallVector<Expr *, 4> Inits;
    ConsumeAnyToken();
    if (Decl *D = ParseOpenMPDeclareReduction(Types, TyRanges, Combiners,
                                              Inits, AS)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_declare_reduction);
        while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
      }
      // Skip the last annot_pragma_openmp_end.
      ConsumeAnyToken();
      return Actions.ActOnOpenMPDeclareReductionDirective(D, Types, TyRanges,
                                                          Combiners, Inits);
    }
    }
    break;
  case OMPD_unknown:
    Diag(Tok, diag::err_omp_unknown_directive);
    break;
  default:
    Diag(Tok, diag::err_omp_unexpected_directive)
      << getOpenMPDirectiveName(DKind);
    break;
  }
  while (!SkipUntil(tok::annot_pragma_openmp_end)) ;
  return DeclGroupPtrTy();
}

/// \brief Late parsing of declarative OpenMP directives.
///
///       threadprivate-directive:
///         annot_pragma_openmp 'threadprivate' simple-variable-list
///         annot_pragma_openmp_end
///
void Parser::LateParseOpenMPDeclarativeDirective(AccessSpecifier AS) {
  assert(Tok.is(tok::annot_pragma_openmp) && "Not an OpenMP directive!");
  LateParsedOpenMPDeclaration *Decl =
    new LateParsedOpenMPDeclaration(this, AS);
  getCurrentClass().LateParsedDeclarations.push_back(Decl);
  while (Tok.isNot(tok::annot_pragma_openmp_end) &&
         Tok.isNot(tok::eof)) {
    Decl->Tokens.push_back(Tok);
    ConsumeAnyToken();
  }
  Decl->Tokens.push_back(Tok);
  ConsumeAnyToken();

  if (Decl->Tokens.size() > 3) {
    Token SavedToken = Decl->Tokens[1];
    if (!SavedToken.isAnnotation()) {
      StringRef Spelling = PP.getSpelling(SavedToken);
      if (Spelling == "declare") {
        SavedToken = Decl->Tokens[2];
        if (!SavedToken.isAnnotation()) {
          Spelling = PP.getSpelling(SavedToken);
          if (Spelling == "simd") {
            if (Tok.isNot(tok::annot_pragma_openmp)) {
              LexTemplateFunctionForLateParsing(Decl->Tokens);
            }
          }
        }
      }
    }
  }
}

/// \brief Actual parsing of late OpenMP declaration.
void Parser::LateParsedOpenMPDeclaration::ParseLexedMethodDeclarations() {
  // Save the current token position.
  SourceLocation origLoc = Self->Tok.getLocation();

  assert(!Tokens.empty() && "Empty body!");
  // Append the current token at the end of the new token stream so that it
  // doesn't get lost.
  Tokens.push_back(Self->Tok);
  Self->PP.EnterTokenStream(Tokens.data(), Tokens.size(), true, false);

  // Consume the previously pushed token.
  Self->ConsumeAnyToken(/*ConsumeCodeCompletionTok=*/true);

  Self->ParseOpenMPDeclarativeDirective(this->AS);

  if (Self->Tok.getLocation() != origLoc) {
    // Due to parsing error, we either went over the cached tokens or
    // there are still cached tokens left. If it's the latter case skip the
    // leftover tokens.
    // Since this is an uncommon situation that should be avoided, use the
    // expensive isBeforeInTranslationUnit call.
    if (Self->PP.getSourceManager().isBeforeInTranslationUnit(
                                                      Self->Tok.getLocation(),
                                                      origLoc))
      while (Self->Tok.getLocation() != origLoc && Self->Tok.isNot(tok::eof))
        Self->ConsumeAnyToken();
  }

}

/// \brief Parsing of declarative or executable OpenMP directives.
///
///       threadprivate-directive:
///         annot_pragma_openmp 'threadprivate' simple-variable-list
///         annot_pragma_openmp_end
///
///       parallel-directive:
///         annot_pragma_openmp 'parallel' {clause} annot_pragma_openmp_end
///
///       for-directive:
///         annot_pragma_openmp 'for' {clause} annot_pragma_openmp_end
///
///       simd-directive:
///         annot_pragma_openmp 'simd' {clause} annot_pragma_openmp_end
///
///       for-simd-directive:
///         annot_pragma_openmp 'for simd' {clause} annot_pragma_openmp_end
///
///       sections-directive:
///         annot_pragma_openmp 'sections' {clause} annot_pragma_openmp_end
///
///       section-directive:
///         annot_pragma_openmp 'section' annot_pragma_openmp_end
///
///       single-directive:
///         annot_pragma_openmp 'single' {clause} annot_pragma_openmp_end
///
///       task-directive:
///         annot_pragma_openmp 'task' {clause} annot_pragma_openmp_end
///
///       taskyield-directive:
///         annot_pragma_openmp 'taskyield' annot_pragma_openmp_end
///
///       master-directive:
///         annot_pragma_openmp 'master' annot_pragma_openmp_end
///
///       critical-directive:
///         annot_pragma_openmp 'critical' [ '(' <name> ')' ]
///         annot_pragma_openmp_end
///
///       barrier-directive:
///         annot_pragma_openmp 'barrier' annot_pragma_openmp_end
///
///       taskwait-directive:
///         annot_pragma_openmp 'taskwait' annot_pragma_openmp_end
///
///       taskgroup-directive:
///         annot_pragma_openmp 'taskgroup' annot_pragma_openmp_end
///
///       atomic-directive:
///         annot_pragma_openmp 'atomic' [clause] [clause]
///         annot_pragma_openmp_end
///
///       flush-directive:
///         annot_pragma_openmp 'flush' [ '(' list ')' ]
///         annot_pragma_openmp_end
///
///       ordered-directive:
///         annot_pragma_openmp 'ordered' annot_pragma_openmp_end
///
StmtResult Parser::ParseOpenMPDeclarativeOrExecutableDirective(
                                                   bool StandAloneAllowed) {
  assert(Tok.is(tok::annot_pragma_openmp) && "Not an OpenMP directive!");
  ParenBraceBracketBalancer BalancerRAIIObj(*this);
  const unsigned ScopeFlags = Scope::FnScope | Scope::OpenMPDirectiveScope |
                              Scope::DeclScope;
  SmallVector<Expr *, 4> Identifiers;
  SmallVector<OMPClause *, 4> Clauses;
  SmallVector<llvm::PointerIntPair<OMPClause *, 1, bool>, 4>
                                             FirstClauses(NUM_OPENMP_CLAUSES);
  Token FirstToken = Tok;
  SourceLocation Loc = ConsumeAnyToken(), EndLoc;
  OpenMPDirectiveKind DKind = Tok.isAnnotation() ?
                                  OMPD_unknown :
                                  getOpenMPDirectiveKind(PP.getSpelling(Tok));
  StmtResult Directive = StmtError();
  DeclarationNameInfo DirName;
  SmallVector<OMPClause *, 4> LocalSavedClauses;
  Token SavedToken;

  switch (DKind) {
  case OMPD_declare: {
    Token SavedToken = PP.LookAhead(0);
    if (!SavedToken.isAnnotation()) {
      StringRef Spelling = PP.getSpelling(SavedToken);
      if (Spelling == "reduction") {
        DKind = OMPD_declare_reduction;
        ConsumeAnyToken();
      }
      else if (Spelling == "simd") {
        DKind = OMPD_declare_simd;
        ConsumeAnyToken();
      }
    }
    }
    break;
  case OMPD_for: {
    Token SavedToken = PP.LookAhead(0);
    if (!SavedToken.isAnnotation()) {
      StringRef Spelling = PP.getSpelling(SavedToken);
      if (Spelling == "simd") {
        DKind = OMPD_for_simd;
        ConsumeAnyToken();
      }
    }
    }
  default:
    break;
  }

  switch (DKind) {
  case OMPD_threadprivate:
    ConsumeAnyToken();
    if (!ParseOpenMPSimpleVarList(OMPD_threadprivate, Identifiers, false)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_threadprivate);
        while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
      }
      DeclGroupPtrTy Res =
        Actions.ActOnOpenMPThreadprivateDirective(Loc,
                                                  Identifiers);
      Directive = Actions.ActOnDeclStmt(Res, Loc, Tok.getLocation());
    }
    while (!SkipUntil(tok::annot_pragma_openmp_end));
    break;
  case OMPD_declare_reduction: {
    SmallVector<QualType, 4> Types;
    SmallVector<SourceRange, 4> TyRanges;
    SmallVector<Expr *, 4> Combiners;
    SmallVector<Expr *, 4> Inits;
    ConsumeAnyToken();
    if (Decl *D = ParseOpenMPDeclareReduction(Types, TyRanges, Combiners,
                                              Inits, AS_none)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_declare_reduction);
        while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
      }
      // Skip the last annot_pragma_openmp_end.
      DeclGroupPtrTy Res =
        Actions.ActOnOpenMPDeclareReductionDirective(D, Types, TyRanges,
                                                     Combiners, Inits);
      Directive = Actions.ActOnDeclStmt(Res, Loc, Tok.getLocation());
    }
    while (!SkipUntil(tok::annot_pragma_openmp_end));
    }
    break;
  case OMPD_critical:
    // Parse name of critical if any.
    if (PP.LookAhead(0).is(tok::l_paren)) {
      // Consume '('.
      ConsumeAnyToken();
      SourceLocation LOpen = Tok.getLocation();
      // Parse <name>.
      ConsumeAnyToken();
      if (!Tok.isAnyIdentifier()) {
        Diag(Tok, diag::err_expected_ident);
      } else {
        DirName = DeclarationNameInfo(Tok.getIdentifierInfo(),
                                      Tok.getLocation());
        ConsumeAnyToken();
      }
      // Parse ')'.
      if (Tok.isNot(tok::r_paren)) {
        Diag(Tok, diag::err_expected_rparen);
        Diag(LOpen, diag::note_matching) << "(";
      }
    }
    StandAloneAllowed = true;
  case OMPD_taskyield:
  case OMPD_barrier:
  case OMPD_taskwait:
    if (!StandAloneAllowed) {
      Diag(Tok, diag::err_omp_immediate_directive)
        << getOpenMPDirectiveName(DKind);
    }
  case OMPD_parallel:
    if (DKind == OMPD_parallel) {
      SavedToken = PP.LookAhead(0);
      if (!SavedToken.isAnnotation()) {
        OpenMPDirectiveKind SDKind =
           getOpenMPDirectiveKind(PP.getSpelling(SavedToken));
        if (SDKind == OMPD_for) {
          DKind = OMPD_parallel_for;
          ConsumeAnyToken();
        } else if (SDKind == OMPD_sections) {
          DKind = OMPD_parallel_sections;
          ConsumeAnyToken();
        }
      }
    }
  case OMPD_for:
  case OMPD_simd:
  case OMPD_for_simd:
  case OMPD_sections:
  case OMPD_section:
  case OMPD_single:
  case OMPD_task:
  case OMPD_master:
  case OMPD_taskgroup:
  case OMPD_atomic:
  case OMPD_ordered: {
    // Do not read token if the end of directive or flush directive.
    if (Tok.isNot(tok::annot_pragma_openmp_end))
      ConsumeAnyToken();
    OpenMPDirectiveKind NewDKind = DKind;
    if (FirstToken.getAnnotationValue()) {
      if (DKind == OMPD_for)
        NewDKind = OMPD_parallel_for;
      else if (DKind == OMPD_sections)
        NewDKind = OMPD_parallel_sections;
    } else if (DKind == OMPD_parallel_for || DKind == OMPD_parallel_sections)
      NewDKind = OMPD_parallel;
    ParseScope OMPDirectiveScope(this, ScopeFlags);
    Actions.StartOpenMPDSABlock(NewDKind, DirName, Actions.getCurScope());
    while (Tok.isNot(tok::annot_pragma_openmp_end)) {
      OpenMPClauseKind CKind = Tok.isAnnotation() ?
                                  OMPC_unknown :
                                  getOpenMPClauseKind(PP.getSpelling(Tok));
      OMPClause *Clause = ParseOpenMPClause(DKind, CKind,
                                            !FirstClauses[CKind].getInt());
      FirstClauses[CKind].setInt(true);
      if (Clause) {
        FirstClauses[CKind].setPointer(Clause);
        if ((DKind == OMPD_parallel_for && isAllowedClauseForDirective(OMPD_for, CKind)) ||
            (DKind == OMPD_parallel_sections && isAllowedClauseForDirective(OMPD_sections, CKind))) {
          LocalSavedClauses.push_back(Clause);
          if (CKind == OMPC_firstprivate)
            Clauses.push_back(Clause);
        }
        else
          Clauses.push_back(Clause);
      }

      // Skip ',' if any.
      if (Tok.is(tok::comma))
        ConsumeAnyToken();
    }
    // End location of the directive.
    EndLoc = Tok.getLocation();
    if (DKind == OMPD_parallel_for || DKind == OMPD_parallel_sections) {
      // Create fake #pragma omp for or sections pragma.
      Token *Toks = new Token[3];
      Toks[0].startToken();
      Toks[0].setKind(tok::annot_pragma_openmp);
      Toks[0].setLocation(Loc);
      Toks[0].setAnnotationValue(&LocalSavedClauses);
      Toks[1] = SavedToken;
      Toks[2] = Tok;
      PP.EnterTokenStream(Toks, 3, true, true);
      DKind = OMPD_parallel;
    } else if (FirstToken.getAnnotationValue()) {
      assert(Clauses.empty() &&
             "There are saved clauses for non-empty clauses list.");
      Clauses = *static_cast<SmallVector<OMPClause *, 4> *>(
                                        FirstToken.getAnnotationValue());
      for (SmallVectorImpl<OMPClause *>::iterator I = Clauses.begin(),
                                                  E = Clauses.end();
           I != E; ++I) {
        FirstClauses[(*I)->getClauseKind()].setInt(true);
        FirstClauses[(*I)->getClauseKind()].setPointer(*I);
      }
      if (DKind == OMPD_for)
        DKind = OMPD_parallel_for;
      else if (DKind == OMPD_sections)
        DKind = OMPD_parallel_sections;
    }
    // Consume final annot_pragma_openmp_end.
    ConsumeAnyToken();
    llvm::SmallVector<Stmt *, 4> AdditionalStmts(Actions.AdditionalOpenMPStmt);
    Actions.AdditionalOpenMPStmt.clear();

    StmtResult AssociatedStmt;
    bool CreateDirective = true;
    if (DKind != OMPD_taskyield && DKind != OMPD_barrier &&
        DKind != OMPD_taskwait) {
      // Parse statement
      // The body is a block scope like in Lambdas and Blocks.
      Sema::CompoundScopeRAII CompoundScope(Actions);
      // Simd has two additional args -- integer index and boolean last_iter.
      int NumArgs = (DKind == OMPD_simd || DKind == OMPD_for_simd) ? 3 : 1;
      Actions.ActOnCapturedRegionStart(Loc, getCurScope(), CR_OpenMP, NumArgs);
      Actions.ActOnStartOfCompoundStmt();
      AssociatedStmt = ParseStatement();
      Actions.ActOnFinishOfCompoundStmt();
      if (!AssociatedStmt.isUsable()) {
        Actions.ActOnCapturedRegionError();
        CreateDirective = false;
      } else {
        Actions.MarkOpenMPClauses(Clauses);
        AssociatedStmt = Actions.ActOnCapturedRegionEnd(AssociatedStmt.take());
        CreateDirective = AssociatedStmt.isUsable();
      }
    }
    if (CreateDirective) {
      Directive = Actions.ActOnOpenMPExecutableDirective(DKind, DirName,
                                                         Clauses,
                                                         AssociatedStmt.take(),
                                                         Loc, EndLoc);
    }

    // Exit scope.
    Actions.EndOpenMPDSABlock(Directive.get());
    OMPDirectiveScope.Exit();
    if (Directive.isUsable() && !AdditionalStmts.empty()) {
      AdditionalStmts.push_back(Directive.take());
      Directive = Actions.ActOnCompoundStmt(Loc, EndLoc, AdditionalStmts, false);
      Actions.AdditionalOpenMPStmt.clear();
    }
    }
    break;
  case OMPD_flush: {
    if (!StandAloneAllowed) {
      Diag(Tok, diag::err_omp_immediate_directive)
        << getOpenMPDirectiveName(DKind);
    }
    if (PP.LookAhead(0).is(tok::l_paren)) {
      // For flush directive set clause kind to pseudo flush clause.
      OMPClause *Clause = ParseOpenMPVarListClause(OMPC_flush);
      if (Clause)
        Clauses.push_back(Clause);
    } else {
      // Consume directive name.
      ConsumeAnyToken();
    }
    if (Tok.isNot(tok::annot_pragma_openmp_end))
      ParseOpenMPClause(DKind, OMPC_unknown, true);
    ParseScope OMPDirectiveScope(this, ScopeFlags);
    Actions.StartOpenMPDSABlock(DKind, DirName, Actions.getCurScope());
    Directive = Actions.ActOnOpenMPExecutableDirective(DKind, DirName,
                                                       Clauses, 0,
                                                       Loc,
                                                       Tok.getLocation());
    // Exit scope.
    Actions.EndOpenMPDSABlock(Directive.get());
    // Consume final annot_pragma_openmp_end.
    ConsumeAnyToken();
    }
    break;
  case OMPD_unknown:
    Diag(Tok, diag::err_omp_unknown_directive);
    while (!SkipUntil(tok::annot_pragma_openmp_end));
    break;
  default:
    Diag(Tok, diag::err_omp_unexpected_directive)
      << getOpenMPDirectiveName(DKind);
    while (!SkipUntil(tok::annot_pragma_openmp_end));
    break;
  }
  return Directive;
}

/// \brief Parses list of simple variables for '#pragma omp threadprivate'
/// directive.
///
///   simple-variable-list:
///         '(' id-expression {',' id-expression} ')'
///
bool Parser::ParseOpenMPSimpleVarList(OpenMPDirectiveKind Kind,
                                      SmallVectorImpl<Expr *> &VarList,
                                      bool AllowScopeSpecifier) {
  VarList.clear();
  // Parse '('.
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_openmp_end);
  bool LParen = !T.expectAndConsume(diag::err_expected_lparen_after,
                                    getOpenMPDirectiveName(Kind));
  bool IsCorrect = LParen;
  bool NoIdentIsFound = true;

  // Read tokens while ')' or annot_pragma_openmp_end is not found.
  while (Tok.isNot(tok::r_paren) && Tok.isNot(tok::annot_pragma_openmp_end)) {
    CXXScopeSpec SS;
    SourceLocation TemplateKWLoc;
    UnqualifiedId Name;
    // Read var name.
    Token PrevTok = Tok;
    NoIdentIsFound = false;

    if (AllowScopeSpecifier && getLangOpts().CPlusPlus &&
        ParseOptionalCXXScopeSpecifier(SS, ParsedType(), false)) {
      IsCorrect = false;
      while (!SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                        StopBeforeMatch)) ;
    } else if (ParseUnqualifiedId(SS, false, false, false, ParsedType(),
                                  TemplateKWLoc, Name)) {
      IsCorrect = false;
      while (!SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                        StopBeforeMatch)) ;
    } else if (Tok.isNot(tok::comma) && Tok.isNot(tok::r_paren) &&
               Tok.isNot(tok::annot_pragma_openmp_end)) {
      IsCorrect = false;
      Diag(PrevTok.getLocation(), diag::err_expected_ident)
        << SourceRange(PrevTok.getLocation(), PrevTokLocation);
      while (!SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                        StopBeforeMatch)) ;
    } else {
      DeclarationNameInfo NameInfo = Actions.GetNameFromUnqualifiedId(Name);
      ExprResult Res = Actions.ActOnOpenMPIdExpression(getCurScope(), SS,
                                                       NameInfo);
      if (Res.isUsable())
        VarList.push_back(Res.take());
    }
    // Consume ','.
    if (Tok.is(tok::comma)) {
      ConsumeAnyToken();
    }
  }

  if (NoIdentIsFound) {
    Diag(Tok, diag::err_expected_ident);
    IsCorrect = false;
  }

  // Parse ')'.
  IsCorrect = ((LParen || Tok.is(tok::r_paren)) && !T.consumeClose())
              && IsCorrect;

  return !IsCorrect && VarList.empty();
}

/// \brief Parsing of OpenMP declare reduction.
///
///    declare_reduction:
///       '(' <identifier> ':' <typename> {',' <typename>} ':' <expr> ')'
///       ['initializer' '(' 'omp_priv' [ '=' ] <expr> ')']
///
Decl *Parser::ParseOpenMPDeclareReduction(SmallVectorImpl<QualType> &Types,
                                          SmallVectorImpl<SourceRange> &TyRanges,
                                          SmallVectorImpl<Expr *> &Combiners,
                                          SmallVectorImpl<Expr *> &Inits,
                                          AccessSpecifier AS) {
  SourceLocation Loc = Tok.getLocation();
  CXXScopeSpec SS;
  SourceLocation TemplateKWLoc;
  UnqualifiedId UI;
  DeclarationName Name;
  Decl *D = 0;

  // Parse '('.
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_openmp_end);
  bool LParen = !T.expectAndConsume(diag::err_expected_lparen_after,
                                    getOpenMPDirectiveName(OMPD_declare_reduction));
  bool IsCorrect = LParen;

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  switch (Tok.getKind()) {
  case tok::plus:      // '+'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("+"));
    ConsumeAnyToken();
    break;
  case tok::minus:     // '-'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("-"));
    ConsumeAnyToken();
    break;
  case tok::star:      // '*'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("*"));
    ConsumeAnyToken();
    break;
  case tok::amp:       // '&'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("&"));
    ConsumeAnyToken();
    break;
  case tok::pipe:      // '|'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("|"));
    ConsumeAnyToken();
    break;
  case tok::caret:     // '^'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("^"));
    ConsumeAnyToken();
    break;
  case tok::ampamp:    // '&&'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("&&"));
    ConsumeAnyToken();
    break;
  case tok::pipepipe:  // '||'
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(
             &Actions.Context.Idents.get("||"));
    ConsumeAnyToken();
    break;
  case tok::identifier:  // identifier
    Name = Actions.getASTContext().DeclarationNames.getIdentifier(Tok.getIdentifierInfo());
    ConsumeAnyToken();
    break;
  default:
    IsCorrect = false;
    Diag(Tok.getLocation(), diag::err_omp_expected_reduction_identifier);
    while (!SkipUntil(tok::colon, tok::r_paren, tok::annot_pragma_openmp_end,
                      StopBeforeMatch)) ;
    break;
  }

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  // Consume ':'.
  if (Tok.is(tok::colon)) {
    ConsumeAnyToken();
  }
  else {
    Diag(Tok.getLocation(), diag::err_expected_colon);
    IsCorrect = false;
  }

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  if (Tok.is(tok::colon) || Tok.is(tok::annot_pragma_openmp_end)) {
    Diag(Tok.getLocation(), diag::err_expected_type);
    IsCorrect = false;
  }

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  bool IsCommaFound = false;
  bool FunctionsCorrect = true;
  while (Tok.isNot(tok::colon) && Tok.isNot(tok::annot_pragma_openmp_end)) {
    ColonProtectionRAIIObject ColonRAII(*this);
    IsCommaFound = false;
    SourceRange Range;
    TypeResult TR = ParseTypeName(&Range, Declarator::PrototypeContext);
    if (TR.isUsable()) {
      QualType QTy = Sema::GetTypeFromParser(TR.take());
      if (!QTy.isNull() &&
          Actions.IsOMPDeclareReductionTypeAllowed(Range, QTy,
                                                   Types, TyRanges)) {
        Types.push_back(QTy);
        TyRanges.push_back(Range);
      } else {
        FunctionsCorrect = false;
      }
    } else {
      while (!SkipUntil(tok::comma, tok::colon,
                        tok::annot_pragma_openmp_end,
                        StopBeforeMatch)) ;
      FunctionsCorrect = false;
    }

    // Consume ','.
    if (Tok.is(tok::comma)) {
      ConsumeAnyToken();
      IsCommaFound = true;
    }
    else if (Tok.isNot(tok::colon) && Tok.isNot(tok::annot_pragma_openmp_end)) {
      Diag(Tok.getLocation(), diag::err_expected_comma);
      IsCorrect = false;
    }
  }

  if (IsCommaFound) {
    Diag(Tok.getLocation(), diag::err_expected_type);
    IsCorrect = false;
    if (Tok.is(tok::annot_pragma_openmp_end)) return 0;
  }

  if (Types.empty()) {
    while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
    return 0;
  }

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  // Consume ':'.
  if (Tok.is(tok::colon)) {
    ConsumeAnyToken();
  }
  else {
    Diag(Tok.getLocation(), diag::err_expected_colon);
    IsCorrect = false;
  }

  if (Tok.is(tok::annot_pragma_openmp_end)) {
    Diag(Tok.getLocation(), diag::err_expected_expression);
    return 0;
  }

  Sema::OMPDeclareReductionRAII RAII(Actions, Actions.CurScope,
                                     Actions.CurContext, Loc,
                                     Name, Types.size(), AS);

  ParseScope OMPDRScope(this, Scope::FnScope|Scope::DeclScope);

  // Parse expression and make pseudo functions.
  for (SmallVectorImpl<QualType>::iterator I = Types.begin(), E = Types.end();
       I != E; ++I) {
    TentativeParsingAction TPA(*this);
    ParseScope FnScope(this, Scope::FnScope|Scope::DeclScope);
    Sema::OMPDeclareReductionFunctionScope Scope(Actions, Loc, Name, *I);
    ExprResult ER = ParseAssignmentExpression();
    if (ER.isInvalid() && Tok.isNot(tok::r_paren) &&
        Tok.isNot(tok::annot_pragma_openmp_end)) {
      TPA.Commit();
      IsCorrect = false;
      break;
    }
    IsCorrect = IsCorrect && !ER.isInvalid();
    Scope.setBody(ER.take());
    Combiners.push_back(Scope.getCombiner());
    if (I + 1 != E) {
      TPA.Revert();
    }
    else {
      TPA.Commit();
    }
  }

  if (!IsCorrect && Tok.is(tok::annot_pragma_openmp_end)) return 0;

  D = RAII.getDecl();

  // Parse ')'.
  IsCorrect = ((LParen || Tok.is(tok::r_paren)) && !T.consumeClose())
              && IsCorrect;

  if (Tok.isAnyIdentifier() && Tok.getIdentifierInfo()->isStr("initializer")) {
    ConsumeAnyToken();
    BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_openmp_end);
    LParen = !T.expectAndConsume(diag::err_expected_lparen_after, "initializer");
    IsCorrect = IsCorrect && LParen;

    bool IsInit = false;
    SourceLocation OmpPrivLoc;
    if (Tok.isAnyIdentifier() && Tok.getIdentifierInfo()->isStr("omp_priv")) {
      IsInit = true;
      OmpPrivLoc = ConsumeAnyToken();
      if (!getLangOpts().CPlusPlus) {
        // Expect '='
        if (Tok.isNot(tok::equal)) {
          Diag(Tok, diag::err_expected_equal_after) << "'omp_priv'";
          IsCorrect = false;
        } else
          ConsumeAnyToken();
      }
    }

    // Parse expression and make pseudo functions.
    for (SmallVectorImpl<QualType>::iterator I = Types.begin(), E = Types.end();
         I != E; ++I) {
      TentativeParsingAction TPA(*this);
      ParseScope FnScope(this, Scope::FnScope|Scope::DeclScope);
      Sema::OMPDeclareReductionInitFunctionScope Scope(Actions, Loc, Name, *I,
                                                       OmpPrivLoc, IsInit);
      ExprResult ER = ParseAssignmentExpression();
      if (ER.isInvalid() && Tok.isNot(tok::r_paren) &&
          Tok.isNot(tok::annot_pragma_openmp_end)) {
        TPA.Commit();
        IsCorrect = false;
        break;
      }
      IsCorrect = IsCorrect && !ER.isInvalid();
      Scope.setInit(ER.take());
      Inits.push_back(Scope.getInitializer());
      if (I + 1 != E) {
        TPA.Revert();
      } else {
        TPA.Commit();
      }
    }

    IsCorrect = ((LParen || Tok.is(tok::r_paren)) && !T.consumeClose())
                && IsCorrect;
  } else if (IsCorrect && FunctionsCorrect) {
    // Parse expression and make pseudo functions.
    for (SmallVectorImpl<QualType>::iterator I = Types.begin(), E = Types.end();
         I != E; ++I) {
      ParseScope FnScope(this, Scope::FnScope|Scope::DeclScope);
      Sema::OMPDeclareReductionInitFunctionScope Scope(Actions, Loc, Name, *I,
                                                       SourceLocation(), true);
      Scope.setInit();
      Inits.push_back(Scope.getInitializer());
    }
  }

  if (!IsCorrect || !FunctionsCorrect)
    D->setInvalidDecl();
  return (IsCorrect && FunctionsCorrect) ? D : 0;
}

/// \brief Parsing of OpenMP clauses.
///
///    clause:
///       if-clause | num_threads-clause | default-clause | proc_bind-clause |
///       private-clause | firstprivate-clause | shared-clause |
///       copyin-clause | reduction-clause | lastprivate-clause |
///       schedule-clause | collapse-clause | ordered-clause | nowait-clause |
///       copyprivate-clause | flush-clause | safelen-clause | linear-clause |
///       aligned-clause | simdlen-clause | num_teams-clause |
///       thread_limit-clause | uniform-clause | inbranch-clause |
///       notinbranch-clause | dist_schedule-clause
///
OMPClause *Parser::ParseOpenMPClause(OpenMPDirectiveKind DKind,
                                     OpenMPClauseKind CKind,
                                     bool FirstClause) {
  OMPClause *Clause = 0;
  bool ErrorFound = false;
  // Check if clause is allowed for the given directive.
  if (CKind != OMPC_unknown && !isAllowedClauseForDirective(DKind, CKind)) {
    Diag(Tok, diag::err_omp_unexpected_clause)
      << getOpenMPClauseName(CKind) << getOpenMPDirectiveName(DKind);
    ErrorFound = true;
  }

  switch (CKind) {
  case OMPC_if:
  case OMPC_num_threads:
  case OMPC_collapse:
  case OMPC_final:
  case OMPC_safelen:
  case OMPC_simdlen:
  case OMPC_num_teams:
  case OMPC_thread_limit:
    // OpenMP [2.5, Restrictions, p.3]
    //  At most one if clause can appear on the directive.
    // OpenMP [2.5, Restrictions, p.5]
    //  At most one num_threads clause can appear on the directive.
    // OpenMP [2.7.1, Restrictions, p. 4]
    //  Only one collapse clause can appear on a loop directive.
    // OpenMP [2.11.1, Restrictions, p. 4]
    //  At most one final clause can appear on the directive.
    // OpenMP [2.8.1, Restrictions, p. 6]
    //  Only one safelen clause can appear on a simd directive.
    // OpenMP [2.8.2, Restrictions, p. 2]
    //  At most one simdlen clause can appear in a declare simd directive.
    // OpenMP [2.9.5, Restrictions, p. 4]
    //  At most one num_teams clause can appear on the directive.
    // OpenMP [2.9.5, Restrictions, p. 3]
    //  At most one thread_limit clause can appear on the directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }

    Clause = ParseOpenMPSingleExprClause(CKind);
    break;
  case OMPC_default:
  case OMPC_proc_bind:
    // OpenMP [2.14.3.1, Restrictions]
    //  Only a single default clause may be specified on a parallel, task
    //  or teams directive.
    // OpenMP [2.5, Restrictions, p. 4]
    //  At most one proc_bind clause can appear on the directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }

    Clause = ParseOpenMPSimpleClause(CKind);
    break;
  case OMPC_ordered:
  case OMPC_nowait:
  case OMPC_untied:
  case OMPC_mergeable:
  case OMPC_read:
  case OMPC_write:
  case OMPC_update:
  case OMPC_capture:
  case OMPC_seq_cst:
    // OpenMP [2.7.1, Restrictions, p. 9]
    //  Only one ordered clause can appear on a loop directive.
    // OpenMP [2.7.1, Restrictions, C/C++, p. 4]
    //  Only one nowait clause can appear on a loop directive.
    // OpenMP [2.7.2, Restrictions, p. 3]
    //  Only one nowait clause can appear on a sections directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }
    // Fall-through...
    // There is no restriction to have only one inbranch/only one
    // notinbranch, only a restriction to not have them both on the
    // same clause.
  case OMPC_inbranch:
  case OMPC_notinbranch:
    Clause = ParseOpenMPClause(CKind);
    break;
  case OMPC_schedule:
  case OMPC_dist_schedule:
    // OpenMP [2.7.1, Restrictions, p. 3]
    //  Only one schedule clause can appear on a loop directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }

    Clause = ParseOpenMPSingleExprWithTypeClause(CKind);
    break;
  case OMPC_private:
  case OMPC_lastprivate:
  case OMPC_firstprivate:
  case OMPC_shared:
  case OMPC_copyin:
  case OMPC_copyprivate:
  case OMPC_reduction:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_uniform:
    Clause = ParseOpenMPVarListClause(CKind);
    break;
  case OMPC_flush:
  case OMPC_unknown:
    Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
      << getOpenMPDirectiveName(DKind);
    while (!SkipUntil(tok::annot_pragma_openmp_end, StopBeforeMatch));
    break;
  default:
    Diag(Tok, diag::err_omp_unexpected_clause)
      << getOpenMPClauseName(CKind) << getOpenMPDirectiveName(DKind);
    while (!SkipUntil(tok::comma, tok::annot_pragma_openmp_end, StopBeforeMatch));
    break;
  }
  return ErrorFound ? 0 : Clause;
}

/// \brief Parsing of OpenMP clauses with single expressions like 'if',
/// 'collapse', 'safelen', 'num_threads', 'simdlen', 'num_teams' or
/// 'thread_limit'.
///
///    if-clause:
///      'if' '(' expression ')'
///
///    num_threads-clause:
///      'num_threads' '(' expression ')'
///
///    collapse-clause:
///      'collapse' '(' expression ')'
///
///    safelen-clause:
///      'safelen' '(' expression ')'
///
///    simdlen-clause:
///      'simdlen' '(' expression ')'
///
///    num_teams-clause:
///      'num_teams' '(' expression ')'
///
///    thread_limit-clause:
///      'thread_limit' '(' expression ')'
///
OMPClause *Parser::ParseOpenMPSingleExprClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after) << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  ExprResult LHS(ParseCastExpression(false, false, NotTypeCast));
  ExprResult Val(ParseRHSOfBinaryExpression(LHS, prec::Conditional));

  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    while (!SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
                      StopBeforeMatch));
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  if (Val.isInvalid())
    return 0;

  return Actions.ActOnOpenMPSingleExprClause(Kind, Val.take(), Loc,
                                             Tok.getLocation());
}

/// \brief Parsing of OpenMP clauses with single expressions and some additional
/// argument like 'schedule' or 'dist_schedule'.
///
///    schedule-clause:
///      'schedule' '(' kind [',' expression ] ')'
///
///    dist_schedule-clause:
///      'dist_schedule' '(' kind [',' expression] ')'
///
OMPClause *Parser::ParseOpenMPSingleExprWithTypeClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Type = Tok.isAnnotation() ?
                     (unsigned)OMPC_SCHEDULE_unknown :
                     getOpenMPSimpleClauseType(Kind, PP.getSpelling(Tok));
  SourceLocation TypeLoc = Tok.getLocation();
  ExprResult Val = ExprError();
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::comma) &&
      Tok.isNot(tok::annot_pragma_openmp_end))
    ConsumeAnyToken();
  if (Tok.is(tok::comma)) {
    ConsumeAnyToken();
    ExprResult LHS(ParseCastExpression(false, false, NotTypeCast));
    Val = ParseRHSOfBinaryExpression(LHS, prec::Conditional);
  }
  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    while (!SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
                      StopBeforeMatch));
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  return Actions.ActOnOpenMPSingleExprWithTypeClause(Kind, Type, TypeLoc,
                                                     Val.take(), Loc,
                                                     Tok.getLocation());
}

/// \brief Parsing of simple OpenMP clauses like 'default' or 'proc_bind'.
///
///    default-clause:
///         'default' '(' 'none' | 'shared' ')'
///
///    proc_bind-clause:
///         'proc_bind' '(' 'master' | 'close' | 'spread' ')'
///
OMPClause *Parser::ParseOpenMPSimpleClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Type = Tok.isAnnotation() ?
                      ((Kind == OMPC_default) ?
                        (unsigned)OMPC_DEFAULT_unknown :
                        (unsigned)OMPC_PROC_BIND_unknown) :
                     getOpenMPSimpleClauseType(Kind, PP.getSpelling(Tok));
  SourceLocation TypeLoc = Tok.getLocation();
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::comma) &&
      Tok.isNot(tok::annot_pragma_openmp_end))
    ConsumeAnyToken();

  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    while (!SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
                      StopBeforeMatch));
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  return Actions.ActOnOpenMPSimpleClause(Kind, Type, TypeLoc, Loc,
                                         Tok.getLocation());
}

/// \brief Parsing of OpenMP clauses like 'ordered' or 'nowait'.
///
///    ordered-clause:
///         'ordered'
///
///    nowait-clause:
///         'nowait'
///
OMPClause *Parser::ParseOpenMPClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  ConsumeAnyToken();

  return Actions.ActOnOpenMPClause(Kind, Loc, Tok.getLocation());
}

/// \brief Parsing of OpenMP clause 'private', 'firstprivate',
/// 'lastprivate', 'shared', 'copyin', 'reduction', 'flush',
/// 'linear', 'aligned'.
///
///    private-clause:
///       'private' '(' list ')'
///
///    lastprivate-clause:
///       'lastprivate' '(' list ')'
///
///    firstprivate-clause:
///       'firstprivate' '(' list ')'
///
///    shared-clause:
///       'shared' '(' list ')'
///
///    copyin-clause:
///       'copyin' '(' list ')'
///
///    copyprivate-clause:
///       'copyprivate' '(' list ')'
///
///    reduction-clause:
///       'reduction' '(' reduction-identifier ':' list ')'
///
///    flush-clause:
///       '(' list ')'
///
///    linear-clause:
///       'linear' '(' list [ ':' linear-step ] ')'
///
///    aligned-clause:
///       'aligned' '(' list [ ':' alignment ] ')'
///
OMPClause *Parser::ParseOpenMPVarListClause(OpenMPClauseKind Kind) {
  assert(Kind != OMPC_uniform);
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeAnyToken();
  bool LParen = true;
  CXXScopeSpec SS;
  UnqualifiedId OpName;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Op = OMPC_REDUCTION_unknown;
  // Parsing "reduction-identifier ':'" for reduction clause.
  if (Kind == OMPC_reduction) {
    Op = Tok.isAnnotation() ?
               (unsigned)OMPC_REDUCTION_unknown :
               getOpenMPSimpleClauseType(Kind, PP.getSpelling(Tok));
    switch (Op) {
    case OMPC_REDUCTION_add:
    case OMPC_REDUCTION_mult:
    case OMPC_REDUCTION_sub:
    case OMPC_REDUCTION_bitand:
    case OMPC_REDUCTION_bitor:
    case OMPC_REDUCTION_bitxor:
    case OMPC_REDUCTION_and:
    case OMPC_REDUCTION_or:
    case OMPC_REDUCTION_min:
    case OMPC_REDUCTION_max:
      OpName.setIdentifier(
        &Actions.Context.Idents.get(getOpenMPSimpleClauseTypeName(Kind, Op)),
        Tok.getLocation());
      if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::annot_pragma_openmp_end)) {
        ConsumeAnyToken();
      }
      break;
    case OMPC_REDUCTION_unknown: {
      if (getLangOpts().CPlusPlus) {
        ParseOptionalCXXScopeSpecifier(SS, ParsedType(), false);
      }
      SourceLocation TemplateKWLoc;
      if (!ParseUnqualifiedId(SS, false, false, false, ParsedType(),
                              TemplateKWLoc, OpName)) {
        Op = OMPC_REDUCTION_custom;
      }
      }
      break;
    case OMPC_REDUCTION_custom:
      llvm_unreachable("'custom' reduction kind cannot be generated directly.");
    case NUM_OPENMP_REDUCTION_OPERATORS:
      llvm_unreachable("unexpected reduction kind.");
    }

    if (Tok.isNot(tok::colon))
      Diag(Tok, diag::err_omp_expected_colon) << getOpenMPClauseName(Kind);
    else
      ConsumeAnyToken();
  }

  SmallVector<Expr *, 4> Vars;
  bool IsComma = Kind != OMPC_reduction || Op != OMPC_REDUCTION_unknown;
  bool MayHaveTail = (Kind == OMPC_linear) || (Kind == OMPC_aligned);
  while (IsComma || (Tok.isNot(tok::r_paren) &&
                     Tok.isNot(tok::annot_pragma_openmp_end) &&
                     Tok.isNot(tok::colon))) {
    // Parse variable
    ExprResult VarExpr = ParseAssignmentExpression();
    if (VarExpr.isUsable()) {
      Vars.push_back(VarExpr.take());
    } else {
      while (!SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                        StopBeforeMatch));
    }
    // Skip ',' if any
    IsComma = Tok.is(tok::comma);
    if (IsComma) {
      ConsumeAnyToken();
    } else if (Tok.isNot(tok::r_paren) &&
               Tok.isNot(tok::annot_pragma_openmp_end) &&
               (!MayHaveTail || Tok.isNot(tok::colon))) {
      Diag(Tok, diag::err_omp_expected_punc)
        << 1 << getOpenMPClauseName(Kind);
    }
  }

  bool MustHaveTail = false;
  Expr *TailExpr = 0;
  SourceLocation TailLoc;
  if (MayHaveTail) {
    // Parse "':' linear-step" or "':' alignment"
    if (Tok.is(tok::colon)) {
      MustHaveTail = true;
      ConsumeAnyToken();
      ColonProtectionRAIIObject ColonRAII(*this);
      TailLoc = Tok.getLocation();
      ExprResult Tail = ParseAssignmentExpression();
      if (Tail.isUsable()) {
        TailExpr = Tail.take();
      }
      else {
        while (!SkipUntil(tok::r_paren, tok::annot_pragma_openmp_end,
                          StopBeforeMatch)) ;
      }
    }
  }

  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    while (!SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
                      StopBeforeMatch)) ;
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  if (Vars.empty() || (Kind == OMPC_reduction && Op == OMPC_REDUCTION_unknown))
    return 0;

  if (MustHaveTail && !TailExpr) {
    // The error ('expected expression') was already emitted.
    return 0;
  }

  return Actions.ActOnOpenMPVarListClause(Kind, Vars,
                    Loc, Tok.getLocation(), Op, TailExpr, SS, OpName,
                    (TailExpr ? TailLoc : SourceLocation()));
}

/// \brief Parsing of OpenMP clause 'linear', 'aligned' or 'uniform' for
/// the '#pragma omp declare simd'.
///
///    linear-clause:
///       'linear' '(' list [ ':' linear-step ] ')'
///
///    aligned-clause:
///       'aligned' '(' list [ ':' alignment ] ')'
///
///    uniform-clause:
///       'uniform' '(' list ')'
///
bool Parser::ParseOpenMPDeclarativeVarListClause(
               OpenMPDirectiveKind DKind,
               OpenMPClauseKind CKind,
               DeclarationNameInfoList &NameInfos,
               SourceLocation &StartLoc,
               SourceLocation &EndLoc,
               Expr *&TailExpr,
               SourceLocation &TailLoc) {
  bool IsCorrect = true;
  // Check if clause is allowed for the given directive.
  if (CKind != OMPC_unknown && !isAllowedClauseForDirective(DKind, CKind)) {
    Diag(Tok, diag::err_omp_unexpected_clause)
      << getOpenMPClauseName(CKind) << getOpenMPDirectiveName(DKind);
    IsCorrect = false;
  }

  // The following constraint should be enforced by directive-clause
  // checks before calling this routine.
  assert(CKind == OMPC_linear || CKind == OMPC_aligned ||
         CKind == OMPC_uniform);

  NameInfos.clear();

  // Read the source location of the clause.
  StartLoc = Tok.getLocation();
  ConsumeToken();

  // Eat '('.
  BalancedDelimiterTracker T(*this, tok::l_paren, tok::annot_pragma_openmp_end);
  bool LParen = !T.expectAndConsume(diag::err_expected_lparen_after,
                                    getOpenMPClauseName(CKind));
  IsCorrect &= LParen;
  bool NoIdentIsFound = true;

  // Parse the comma-separated identifiers list.
  bool IsComma = true;
  while (IsComma || (Tok.isNot(tok::r_paren) &&
                     Tok.isNot(tok::annot_pragma_openmp_end) &&
                     Tok.isNot(tok::colon))) {
    CXXScopeSpec SS;
    SourceLocation TemplateKWLoc;
    UnqualifiedId Name;
    if (ParseUnqualifiedId(SS, false, false, false, ParsedType(),
                           TemplateKWLoc, Name)) {
      IsCorrect = false;
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                StopBeforeMatch);
    }
    else {
      DeclarationNameInfo NameInfo = Actions.GetNameFromUnqualifiedId(Name);
      NameInfos.push_back(NameInfo);
      NoIdentIsFound = false;
    }
    // Consume ','.
    IsComma = Tok.is(tok::comma);
    if (IsComma) {
      ConsumeToken();
    }
  }
  bool MayHaveTail = (CKind == OMPC_linear) || (CKind == OMPC_aligned);
  bool MustHaveTail = false;
  TailExpr = 0;
  if (MayHaveTail) {
    // Parse "':' linear-step" or "':' alignment"
    if (Tok.is(tok::colon)) {
      MustHaveTail = true;
      ConsumeAnyToken();
      ColonProtectionRAIIObject ColonRAII(*this);
      TailLoc = Tok.getLocation();
      ExprResult Tail = ParseAssignmentExpression();
      if (Tail.isUsable()) {
        TailExpr = Tail.take();
      }
      else {
        SkipUntil(tok::r_paren, tok::annot_pragma_openmp_end, StopBeforeMatch);
      }
    }
  }
  if (NoIdentIsFound) {
    Diag(Tok, diag::err_expected_ident);
    IsCorrect = false;
  }

  EndLoc = Tok.getLocation();

  // Eat ')'.
  IsCorrect = ((LParen || Tok.is(tok::r_paren)) && !T.consumeClose())
              && IsCorrect;

  return !IsCorrect;
}

