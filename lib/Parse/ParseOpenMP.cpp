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
Parser::DeclGroupPtrTy Parser::ParseOpenMPDeclarativeDirective() {
  assert(Tok.is(tok::annot_pragma_openmp) && "Not an OpenMP directive!");

  SourceLocation Loc = ConsumeToken();
  SmallVector<Expr *, 5> Identifiers;
  OpenMPDirectiveKind DKind = Tok.isAnnotation() ?
                                  OMPD_unknown :
                                  getOpenMPDirectiveKind(PP.getSpelling(Tok));

  switch (DKind) {
  case OMPD_threadprivate:
    ConsumeToken();
    if (!ParseOpenMPSimpleVarList(OMPD_threadprivate, Identifiers, true)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_threadprivate);
        SkipUntil(tok::annot_pragma_openmp_end, false, true);
      }
      // Skip the last annot_pragma_openmp_end.
      ConsumeToken();
      return Actions.ActOnOpenMPThreadprivateDirective(Loc,
                                                       Identifiers);
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
  SkipUntil(tok::annot_pragma_openmp_end, false);
  return DeclGroupPtrTy();
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
  const unsigned ScopeFlags = Scope::FnScope | Scope::OpenMPDirectiveScope |
                              Scope::DeclScope;
  SmallVector<Expr *, 5> Identifiers;
  SmallVector<OMPClause *, 5> Clauses;
  SmallVector<llvm::PointerIntPair<OMPClause *, 1, bool>, NUM_OPENMP_CLAUSES>
                                             FirstClauses(NUM_OPENMP_CLAUSES);
  Token FirstToken = Tok;
  SourceLocation Loc = ConsumeToken(), EndLoc;
  OpenMPDirectiveKind DKind = Tok.isAnnotation() ?
                                  OMPD_unknown :
                                  getOpenMPDirectiveKind(PP.getSpelling(Tok));
  StmtResult Directive = StmtError();
  DeclarationNameInfo DirName;
  SmallVector<OMPClause *, 5> LocalSavedClauses;
  Token SavedToken;

  switch (DKind) {
  case OMPD_threadprivate:
    ConsumeToken();
    if (!ParseOpenMPSimpleVarList(OMPD_threadprivate, Identifiers, false)) {
      // The last seen token is annot_pragma_openmp_end - need to check for
      // extra tokens.
      if (Tok.isNot(tok::annot_pragma_openmp_end)) {
        Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
          << getOpenMPDirectiveName(OMPD_threadprivate);
        SkipUntil(tok::annot_pragma_openmp_end, false, true);
      }
      DeclGroupPtrTy Res =
        Actions.ActOnOpenMPThreadprivateDirective(Loc,
                                                  Identifiers);
      Directive = Actions.ActOnDeclStmt(Res, Loc, Tok.getLocation());
    }
    SkipUntil(tok::annot_pragma_openmp_end, false);
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
          ConsumeToken();
        } else if (SDKind == OMPD_sections) {
          DKind = OMPD_parallel_sections;
          ConsumeToken();
        }
      }
    }
  case OMPD_for:
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
    Actions.StartOpenMPDSABlock(NewDKind, DirName, Actions.getCurScope());
    while (Tok.isNot(tok::annot_pragma_openmp_end)) {
      OpenMPClauseKind CKind = Tok.isAnnotation() ?
                                  OMPC_unknown :
                                  getOpenMPClauseKind(PP.getSpelling(Tok));
      // For flush directive set clause kind to pseudo flush clause.
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
        ConsumeToken();
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
      Clauses = *static_cast<SmallVector<OMPClause *, 5> *>(
                                        FirstToken.getAnnotationValue());
      for (SmallVector<OMPClause *, 5>::iterator I = Clauses.begin(),
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
    ConsumeToken();
    llvm::SmallVector<Stmt *, 64> AdditionalStmts(Actions.AdditionalOpenMPStmt);
    Actions.AdditionalOpenMPStmt.clear();

    StmtResult AssociatedStmt;
    bool CreateDirective = true;
    ParseScope OMPDirectiveScope(this, ScopeFlags);
    if (DKind != OMPD_taskyield && DKind != OMPD_barrier &&
        DKind != OMPD_taskwait) {
      // The body is a block scope like in Lambdas and Blocks.
      // OpenMP [2.6.1, Loop construct, Description]
      //  The collapse clause may be used to specify how many loops are
      //  associated with the loop construct.
      // Parse statement
      Sema::CompoundScopeRAII CompoundScope(Actions);
      Actions.ActOnCapturedRegionStart(Loc, getCurScope(), CR_Default, 1);
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
    ConsumeToken();
    }
    break;
  case OMPD_unknown:
    Diag(Tok, diag::err_omp_unknown_directive);
    SkipUntil(tok::annot_pragma_openmp_end, false);
    break;
  default:
    Diag(Tok, diag::err_omp_unexpected_directive)
      << getOpenMPDirectiveName(DKind);
    SkipUntil(tok::annot_pragma_openmp_end, false);
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
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                false, true);
    } else if (ParseUnqualifiedId(SS, false, false, false, ParsedType(),
                                  TemplateKWLoc, Name)) {
      IsCorrect = false;
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                false, true);
    } else if (Tok.isNot(tok::comma) && Tok.isNot(tok::r_paren) &&
               Tok.isNot(tok::annot_pragma_openmp_end)) {
      IsCorrect = false;
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                false, true);
      Diag(PrevTok.getLocation(), diag::err_expected_ident)
        << SourceRange(PrevTok.getLocation(), PrevTokLocation);
    } else {
      DeclarationNameInfo NameInfo = Actions.GetNameFromUnqualifiedId(Name);
      ExprResult Res = Actions.ActOnOpenMPIdExpression(getCurScope(), SS,
                                                       NameInfo);
      if (Res.isUsable())
        VarList.push_back(Res.take());
    }
    // Consume ','.
    if (Tok.is(tok::comma)) {
      ConsumeToken();
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

/// \brief Parsing of OpenMP clauses.
///
///    clause:
///       if-clause|num_threads-clause|default-clause|private-clause|
///       firstprivate-clause|shared-clause|copyin-clause|reduction-clause|
///       lastprivate-clause | schedule-clause | collapse-close |
///       ordered-clause | nowait-clause | copyprivate-clause | flush_clause
///
OMPClause *Parser::ParseOpenMPClause(OpenMPDirectiveKind DKind,
                                     OpenMPClauseKind CKind, bool FirstClause) {
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
    // OpenMP [2.4, Restrictions, p.3]
    //  At most one if clause can appear on the directive.
    // OpenMP [2.4, Restrictions, p.4]
    //  At most one num_threads clause can appear on the directive.
    // OpenMP [2.5.1, Restrictions, p. 4]
    //  Only one collapse clause can appear on a loop directive.
    // OpenMP [2.9.1, Restrictions, p. 4]
    //  Only one final clause can appear on the directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }

    Clause = ParseOpenMPSingleExprClause(CKind);
    break;
  case OMPC_default:
    // OpenMP [2.9.3.1, Restrictions]
    //  Only a single default clause may be specified on a parallel or task
    //  directive.
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
    // OpenMP [2.5.1, Restrictions, p. 9]
    //  Only one ordered clause can appear on a loop directive.
    // OpenMP [2.5.1, Restrictions, C/C++, p. 4]
    //  Only one nowait clause can appear on a loop directive.
    // OpenMP [2.6.2, Restrictions, p. 3]
    //  Only one nowait clause can appear on a sections directive.
    if (!FirstClause) {
      Diag(Tok, diag::err_omp_more_one_clause)
           << getOpenMPDirectiveName(DKind) << getOpenMPClauseName(CKind);
    }
    Clause = ParseOpenMPClause(CKind);
    break;
  case OMPC_schedule:
    // OpenMP [2.5.1, Restrictions, p. 3]
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
    Clause = ParseOpenMPVarListClause(CKind);
    break;
  case OMPC_flush:
  case OMPC_unknown:
    Diag(Tok, diag::warn_omp_extra_tokens_at_eol)
      << getOpenMPDirectiveName(DKind);
    SkipUntil(tok::annot_pragma_openmp_end, false, true);
    break;
  default:
    Diag(Tok, diag::err_omp_unexpected_clause)
      << getOpenMPClauseName(CKind) << getOpenMPDirectiveName(DKind);
    SkipUntil(tok::comma, tok::annot_pragma_openmp_end, false, true);
    break;
  }
  return ErrorFound ? 0 : Clause;
}

/// \brief Parsing of OpenMP clauses with single expressions like 'if',
/// 'collapse' or 'num_threads'.
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
OMPClause *Parser::ParseOpenMPSingleExprClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeToken();
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
    SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
              false, true);
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  if (Val.isInvalid())
    return 0;

  return Actions.ActOnOpenMPSingleExprClause(Kind, Val.take(), Loc,
                                             Tok.getLocation());
}

/// \brief Parsing of OpenMP clauses with single expressions and some additional
/// argument like 'schedule'.
///
///    schedule-clause:
///      'schedule' '(' kind [',' expression ] ')'
///
OMPClause *Parser::ParseOpenMPSingleExprWithTypeClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Type = Tok.isAnnotation() ?
                     OMPC_SCHEDULE_unknown :
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
    SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
              false, true);
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  return Actions.ActOnOpenMPSingleExprWithTypeClause(Kind, Type, TypeLoc,
                                                     Val.take(), Loc,
                                                     Tok.getLocation());
}

/// \brief Parsing of simple OpenMP clauses like 'default'.
///
///    default-clause:
///         'default' '(' 'none' | 'shared' ')'
///
OMPClause *Parser::ParseOpenMPSimpleClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Type = Tok.isAnnotation() ?
                     OMPC_DEFAULT_unknown :
                     getOpenMPSimpleClauseType(Kind, PP.getSpelling(Tok));
  SourceLocation TypeLoc = Tok.getLocation();
  if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::comma) &&
      Tok.isNot(tok::annot_pragma_openmp_end))
    ConsumeAnyToken();

  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
              false, true);
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
  ConsumeToken();

  return Actions.ActOnOpenMPClause(Kind, Loc, Tok.getLocation());
}

/// \brief Parsing of OpenMP clause 'private', 'firstprivate',
/// 'lastprivate', 'shared', 'copyin', 'reduction' or 'flush'.
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
OMPClause *Parser::ParseOpenMPVarListClause(OpenMPClauseKind Kind) {
  SourceLocation Loc = Tok.getLocation();
  SourceLocation LOpen = ConsumeToken();
  bool LParen = true;
  if (Tok.isNot(tok::l_paren)) {
    Diag(Tok, diag::err_expected_lparen_after)
      << getOpenMPClauseName(Kind);
    LParen = false;
  }
  else
    ConsumeAnyToken();

  unsigned Op = OMPC_REDUCTION_unknown;
  SourceLocation OpLoc;
  // Parsing "reduction-identifier ':'" for reduction clause.
  if (Kind == OMPC_reduction) {
    Op = Tok.isAnnotation() ?
               OMPC_REDUCTION_unknown :
               getOpenMPSimpleClauseType(Kind, PP.getSpelling(Tok));
    OpLoc = Tok.getLocation();
    if (Op == OMPC_REDUCTION_unknown) {
      Diag(Tok, diag::err_omp_unknown_reduction_op);
    }

    if (Tok.isNot(tok::r_paren) && Tok.isNot(tok::annot_pragma_openmp_end)) {
      ConsumeAnyToken();
      if (Tok.isNot(tok::colon))
        Diag(Tok, diag::err_omp_expected_colon) << getOpenMPClauseName(Kind);
      else
        ConsumeAnyToken();
    }
  }

  SmallVector<Expr *, 5> Vars;
  bool IsComma = Kind != OMPC_reduction || Op != OMPC_REDUCTION_unknown;
  while (IsComma || (Tok.isNot(tok::r_paren) &&
                     Tok.isNot(tok::annot_pragma_openmp_end))) {
    // Parse variable
    ExprResult VarExpr = ParseAssignmentExpression();
    if (VarExpr.isUsable()) {
      Vars.push_back(VarExpr.take());
    } else {
      SkipUntil(tok::comma, tok::r_paren, tok::annot_pragma_openmp_end,
                false, true);
    }
    // Skip ',' if any
    IsComma = Tok.is(tok::comma);
    if (IsComma) {
      ConsumeToken();
    }
  }

  if (LParen && Tok.isNot(tok::r_paren)) {
    Diag(Tok, diag::err_expected_rparen);
    Diag(LOpen, diag::note_matching) << "(";
    SkipUntil(tok::r_paren, tok::comma, tok::annot_pragma_openmp_end,
              false, true);
  }
  if (Tok.is(tok::r_paren))
    ConsumeAnyToken();

  if (Vars.empty() || (Kind == OMPC_reduction && Op == OMPC_REDUCTION_unknown))
    return 0;

  return Actions.ActOnOpenMPVarListClause(Kind, Vars,
                                          Loc, Tok.getLocation(), Op, OpLoc);
}
