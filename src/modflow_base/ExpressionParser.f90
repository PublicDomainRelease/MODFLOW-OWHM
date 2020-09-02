! CODE DEVELOPED BY SCOTT E BOYCE
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE ExpressionParser
!  DESIGNED TO TAKE IN A LIST OF VARIABLE VALUES AND EVALUTE
!
!  *** MULTI-DIMENSIONAL SUPPORT, CAN SOLVE FOR EXPRESSION OF SCALARS, VECTORS, OR 2D ARRAYS ***
!
!    --MAJOR LIMITATION:
!                        EXPRESSION PARSER CAN NOT HANDLE NEGATIVE NUMBER INPUTS eg: "A+B*(-3+C)"  A, B, C CAN CONTAIN NEGATIVE NUMBERS, BUT "-3" DOES NOT WORK
!                        USE NEG FUNCTION TO GET NEGATIVE NUMBERS IF NEEDED      eg: "A+B*(NEG(3)+C)"    => "NEG(3)" YIELDS "-3"
!
!  VERSION 2.0 [6/15/2015] RESTRUCTURED MAIN VARIABLE "EQN" TO BE DERIVED DATA TYPE
!                          ADDED ABILITY TO HAVE CONDITIONAL EXPRESSION OF THE FORMAT "IF[CONDITION, TRUE, FALSE]"
!                               WHERE "CONDITION" IS AN EXPRESSION WITH "<", ">", "<=", OR ">=", THERE ALSO CAN BE MULTIPLE CONDITIONS WITH & (AND) or | (OR) OPERATORS.
!                                                 --NOTE & IS HIGHER PRECEDANCE THAN | SO IT IS ALWAYS EVALUATED FIRST. PARENTHESIS ARE NOT ALLOWED TO SPAN ACROSS THE INEQUALITY!
!                                     "TRUE"  IS THE RESULT THAT IS RETURNED IF "CONDITION" IS TRUE,  IT CAN BE AN EXPRESSION AS WELL
!                                     "FALSE" IS THE RESULT THAT IS RETURNED IF "CONDITION" IS FALSE, IT CAN BE AN EXPRESSION AS WELL  --THIS IS OPTIONAL
!                                             WHEN "FALSE" IS NOT PRESENT AND THE CONDITIONAL IS FALSE THEN THE RESULT IS SET TO NaN
!                                             THIS ALLOWS FOR OUTSIDE PROGRAMS TO CASECASE CONDITIONAL EXPRESSIONS BY CHECKING FOR NaN
!
!                               EXAMPLE:  IF[A<B, NEG(1), EXP(C*D)]
!                               EXAMPLE:  IF[(A+B*C)<B & A<C | B>C, NEG(1), EXP(C*D)]
!                               CONDITIONAL EXPRESSION CAN BE APART OF ANY EXPRESSION OR WITHIN ANOTHER CONDITINAL EXPRESSION, eg: A + IF[A<B, IF[B<C,1,0], IF[A<C,1,0]]
!                          ADDED ABILITY TO SOLVE FOR MIN OR MAX OF A SEQUENCE OF EXPRESSIONS OF THE FORMAT "MAX[SEQ1, SEQ2, SEQ3, ...]"
!                               WHERE "SEQ" CAN BE ANY VALID EXPRESSION (NOTE THE LENGTH CAN BE AS MANY TERMS AS NEEDED)
!                               EXAMPLE:  MAX[1,2,3,4] or MIN[A, B, EXP(C*D), ABS(E-F^2)]
!
!  VERSION 1.1 [3/30/2014] ADDED THE ABILITY TO HAVE KEYWORD FUNCTIONS
!                          SUCH AS "EXP", NUMERICAL INPUT (eg 5*8+A), AND NEW ELEMENTAL SUBROUTINE UPPER
!
!  VERSION 1.0 [5/01/2013] ORIGINAL VERSION THAT SUPPORTS 5 OPERATION, +-*/^, PARSING WITH ( )
!
!
! MODULE IS DESIGNED TO TAKE IN A LIST OF VARIABLE VALUES AND EVALUTE
! AN EXPRESSION CONTAINING THE VARIABLES WITHIN A STRING
! THE EXPRESSION FOLLOWS ORDER PRECIDENCE (ORDER OF OPERATIONS)
!
MODULE ExpressionParser                                             !MODULE THAT TAKES IN A STRING EXPRESSION, REMOVES BLANKS, SOLVES FOR VARIABLE DEFINITIONS.
  IMPLICIT NONE
  PRIVATE                                                           !MAKES ALL VARIABLES AND FUNCTIONS PRIVATE
  INTEGER,PARAMETER:: VARLEN=25                                     !LENTH OF VARIABLE NAMES
  CHARACTER(VARLEN),   ALLOCATABLE,DIMENSION(:)::VAR                !NAME OF VARIABLES USED MAX LEN OF 25
  DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:)::VAL                !STORED VALUES OF VARIABLES, 3rd DIM IS 1to1 WITH VAR NAMES
  TYPE EQN_TERMS
    CHARACTER(:),ALLOCATABLE::T
  END TYPE
  !
  PUBLIC::  ExpEval                                                 !THIS IS THE DRIVER INTERFACE OF THE MODULE AND DIRECTS THE INPUTS TO THE APPORPIATE SUBROUTINES. ExpEval DEFERS TO 3 DIFFERENT FUNCTIONS DEPENDING ON INPUT
  !
  INTERFACE ExpEval
    MODULE PROCEDURE ExpEval1D
    MODULE PROCEDURE ExpEval2D
    MODULE PROCEDURE ExpEval3D
  END INTERFACE
  !
  CHARACTER(5),PARAMETER,DIMENSION(10)::  KEYWORDLIST = ['ABS  ','EXP  ','LOG  ','L10  ','NEG  ','SQRT ','ROUND', 'TRUNC','FLOOR', 'CEIL ']   !CHECK SEARCH LOOPS AT "DO WHILE (I <= LINELEN-4)"
  !
  !CHARACTER(3),PARAMETER, DIMENSION(3):: SPECIALWORDLIST = ['IF ', 'MIN', 'MAX']
  !
CONTAINS
!###################################################################### START DRIVER ROUTINES THAT RESHAPE ARRAYS
!######################################################################
!
FUNCTION ExpEval1D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)           !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT A COPY OF NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
  CHARACTER(*),                 INTENT(IN)::Ln                      !STRING THAT CONTAINS EXPRESSION
  CHARACTER(*),    DIMENSION(:),INTENT(IN)::NML                     !ARRAY OF VARIABLE NAMES
  DOUBLE PRECISION,DIMENSION(:),INTENT(IN)::NMV                     !ARRAY OF VARIABLE VALUES WITH HIGHEST DIMENSION ONE TO ONE WITH VARIABLE NAME ARRAY (RIGHT MOST DIM)
  LOGICAL,         OPTIONAL,    INTENT(IN)::CHKCASE,CHKKEY          !OPTIONAL VARIABLES THAT TELL CODE TO MAKE ALL VARIABLES UPPER CASE AND/OR CHECK IF ANY OF THE VARIABLES ARE KEYWORDS
  DOUBLE PRECISION                        ::RES
  !
  DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ANS                  !CONTAINS THE FINAL ANSWER WHICH IS SET TO RES BEFORE EXIT
  !
  INTEGER::ROW,COL,NVAR
  LOGICAL::CHKC,CHKK
  !
  CHKC=.FALSE.
  CHKK=.FALSE.
  IF(PRESENT(CHKCASE))CHKC=CHKCASE
  IF(PRESENT(CHKKEY ))CHKK=CHKKEY
  !
  ROW=1
  COL=1
  NVAR=SIZE(NMV)                                                    !HIGHEST DIMENSION IS 1
  !
  ALLOCATE(VAR(NVAR),VAL(ROW,COL,NVAR),ANS(ROW,COL))
  !
  VAR=ADJUSTL(NML)
  IF(CHKC) CALL UPPER(VAR)
  IF(CHKK) CALL KEYWORDCHECK(VAR)
  !
  VAL=RESHAPE(NMV,[ROW,COL,NVAR])
  ANS=0D0
  !
  CALL ExpEvalRun(Ln,ANS)
  !
  RES=ANS(1,1)
  !
  DEALLOCATE(VAR,VAL,ANS)
  !
END FUNCTION
!
!######################################################################
!
FUNCTION ExpEval2D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)           !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
  CHARACTER(*),                   INTENT(IN)::Ln                    !STRING THAT CONTAINS EXPRESSION
  CHARACTER(*),    DIMENSION(:)  ,INTENT(IN)::NML                   !ARRAY OF VARIABLE NAMES
  DOUBLE PRECISION,DIMENSION(:,:),INTENT(IN)::NMV                   !ARRAY OF VARIABLE VALUES WITH HIGHEST DIMENSION ONE TO ONE WITH VARIABLE NAME ARRAY (RIGHT MOST DIM)
  LOGICAL,         OPTIONAL,      INTENT(IN)::CHKCASE,CHKKEY        !OPTIONAL VARIABLES THAT TELL CODE TO MAKE ALL VARIABLES UPPER CASE AND/OR CHECK IF ANY OF THE VARIABLES ARE KEYWORDS
  DOUBLE PRECISION,DIMENSION(SIZE(NMV,1)) ::RES
  !
  DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ANS                  !CONTAINS THE FINAL ANSWER WHICH IS SET TO RES BEFORE EXIT
  !
  INTEGER::ROW,COL,NVAR
  LOGICAL::CHKC,CHKK
  !
  CHKC=.FALSE.
  CHKK=.FALSE.
  IF(PRESENT(CHKCASE))CHKC=CHKCASE
  IF(PRESENT(CHKKEY ))CHKK=CHKKEY
  !
  ROW=SIZE(NMV,1)
  COL=1
  NVAR=SIZE(NMV,2)                                                !HIGHEST DIMENSION IS 2
  !
  ALLOCATE(VAR(NVAR),VAL(ROW,COL,NVAR),ANS(ROW,COL))
  !
  VAR=ADJUSTL(NML)
  IF(CHKC) CALL UPPER(VAR)
  IF(CHKK) CALL KEYWORDCHECK(VAR)
  !
  VAL=RESHAPE(NMV,[ROW,COL,NVAR])
  ANS=0D0
  !
  CALL ExpEvalRun(Ln,ANS)
  !
  RES=RESHAPE(ANS,[ROW])
  !
  DEALLOCATE(VAR,VAL,ANS)
END FUNCTION
!
!######################################################################
!
FUNCTION ExpEval3D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)           !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
  CHARACTER(*),                     INTENT(IN)::Ln                  !STRING THAT CONTAINS EXPRESSION
  CHARACTER(*),    DIMENSION(:)    ,INTENT(IN)::NML                 !ARRAY OF VARIABLE NAMES
  DOUBLE PRECISION,DIMENSION(:,:,:),INTENT(IN)::NMV                 !ARRAY OF VARIABLE VALUES WITH HIGHEST DIMENSION ONE TO ONE WITH VARIABLE NAME ARRAY (RIGHT MOST DIM)
  LOGICAL,         OPTIONAL,        INTENT(IN)::CHKCASE,CHKKEY      !OPTIONAL VARIABLES THAT TELL CODE TO MAKE ALL VARIABLES UPPER CASE AND/OR CHECK IF ANY OF THE VARIABLES ARE KEYWORDS
  DOUBLE PRECISION,DIMENSION(SIZE(NMV,1),SIZE(NMV,2))::RES
  !
  DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::ANS                  !CONTAINS THE FINAL ANSWER WHICH IS SET TO RES BEFORE EXIT
  !
  INTEGER::ROW,COL,NVAR
  LOGICAL::CHKC,CHKK
  !
  CHKC=.FALSE.
  CHKK=.FALSE.
  IF(PRESENT(CHKCASE))CHKC=CHKCASE
  IF(PRESENT(CHKKEY ))CHKK=CHKKEY
  !
  ROW =SIZE(NMV,1)
  COL =SIZE(NMV,2)
  NVAR=SIZE(NMV,3)                                                !HIGHEST DIMENSION IS 3
  !
  ALLOCATE(VAR(NVAR),VAL(ROW,COL,NVAR),ANS(ROW,COL))
  !
  VAR=ADJUSTL(NML)
  IF(CHKC) CALL UPPER(VAR)
  IF(CHKK) CALL KEYWORDCHECK(VAR)
  !
  !VAL=RESHAPE(NMV,[ROW,COL,NVAR])
  VAL=NMV
  ANS=0D0
  !
  CALL ExpEvalRun(Ln,ANS)
  !
  RES=RESHAPE(ANS,[ROW,COL])
  !
  DEALLOCATE(VAR,VAL,ANS)
  !
END FUNCTION
!
!######################################################################     START MAIN EXPRESSION PARSING ROUTINES
!######################################################################
!
SUBROUTINE ExpEvalRun(Ln,ANS)
  CHARACTER(*),INTENT(IN)::Ln                                       !STRING THAT CONTAINS EXPRESSION
  DOUBLE PRECISION,DIMENSION(:,:),CONTIGUOUS::ANS                              !FINAL ANSWER TO FROM EXPRESSION
  !
  !CHARACTER(VARLEN),DIMENSION(:),ALLOCATABLE::Eqn                  !ARRAY CONTAINING ALL PARTS OF EXPRESSION
  TYPE (EQN_TERMS),DIMENSION(:),ALLOCATABLE::Eqn
  CHARACTER(LEN(Ln))::Ln2
  INTEGER::R,MP
  !
  Ln2=Ln                                                            !COPY STRING FOR EDITING
  !
  CALL SplitFunc(LN2,Eqn)                                           !SPLIT STRING INTO AN ARRAY OF STRINGS FOR EACH VARIABLE, OPERATOR, AND PAREN
  !
  R=1
  MP=1
  CALL EqnParser(Eqn,R,ANS,MP)                                      !SOLVE FOR ALL PIECES OF Eqn
  !
END SUBROUTINE
!
!######################################################################
!
RECURSIVE SUBROUTINE EqnParser(Eqn,R,ANS,MP)                      !EVALUATE PARTS OF AN EXPRESSION
  !CHARACTER(VARLEN),DIMENSION(:), INTENT(IN   )::Eqn                !ARRAY CONTAINING ALL PARTS OF EXPRESSION
  TYPE (EQN_TERMS), DIMENSION(:), INTENT(IN   )::Eqn
  INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON
  INTEGER,                        INTENT(IN   )::MP                 !MINIMUM PRECDIENCE OF CURRENT EXPRESSION
  DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::ANS                !CAN THINK OF THIS AS THE LEFT HAND SIDE OF THE OPERATION (LHS op RIGHT)
  CONTIGUOUS:: Eqn, ANS
  !
  DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::RHS
  INTEGER::PREC
  INTEGER::ROW,COL
  !
  CHARACTER(5), PARAMETER:: OP='+-*/^'
  !CHARACTER(1)::LP='('
  !CHARACTER(1)::RP=')'
  !
  CHARACTER(1)::DIRECTION,COP  !DIRECTION OF OPERATOR, CURRENT OPERATOR
  !
  ! operator
  ! precedence   operators       associativity
  ! 3            ^               right to left
  ! 2            * /             left to right
  ! 1            + -             left to right
  ! StartOver    (
  ! StartOver    KEYWORD
  !
  ROW=SIZE(ANS,1)
  COL=SIZE(ANS,2)
  IF(.NOT. ALLOCATED(RHS)) THEN
    ALLOCATE(RHS(ROW,COL))
    RHS=0D0
  END IF
  !
  CALL NextAtom(Eqn,R,ANS)                                          !NEXT ATOM MOVES TO NEXT PART OF EQUATION
  !
  DO
    IF ( R>SIZE(Eqn)               ) EXIT                           !REACHED END OF FUNCTION
    IF ( INDEX(OP,Eqn(R)%T) < 1    ) EXIT                           !ENSURE THAT IT IS AN OPERATOR
    IF ( PRECEDENCE(Eqn(R)%T) < MP ) EXIT                           !PRECEDENCE LEVEL HAS BEEN REDUCED SO EXIT TO BACK CALCULATE
    !
    COP=Eqn(R)%T                                                    !CURRENT OPERATION
    PREC=PRECEDENCE(COP,DIRECTION)                                  !RERETRIVE PRECEDENCE LEVEL AND GET THE DIRECTION OF THE OPERATOR
    !
    IF (DIRECTION=='L')THEN                                         !DIRECTION IS TO THE LEFT
      PREC=PREC+1
    !ELSE DIRECTION=='R'                                            !DIRECTION IS TO THE RIGHT NO NEED FOR UPDATE
    ! PREC = PREC
    END IF
    R=R+1
    CALL EqnParser(Eqn,R,RHS,PREC)
    !
    ! ERROR CHECK
    SELECT CASE(COP)
    CASE('/')
      IF(ANY(ABS(RHS)<1D-30)) THEN
        WRITE(*,'(4A /, 9x,*(A 1x))')'EXPRESSION ERROR: DIVIDE BY 0 DURING AN OPERATION SOMEWHERE ', &
                   'AFTER A "/" AND BEFORE "',Eqn(R)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
        ERROR STOP
      END IF
    CASE('^')
      IF(ANY(ABS(ANS)<1D-30 .AND. ANY(RHS<1D-30))) THEN
        WRITE(*,'(4A /, 9x,*(A 1x))')'EXPRESSION ERROR: POWER FUNCTION, "^" BASE IS TOO CLOSE TO ZERO ', &
                   ' WITH POWER LESS THEN ZERO WITH GUESS ERROR TERM AT: "',Eqn(R)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
        ERROR STOP
      END IF
    END SELECT
    !
    ANS=MathOP(COP,ANS,RHS)
    !
  END DO
  !
END SUBROUTINE
!
!######################################################################
!
RECURSIVE SUBROUTINE NextAtom(Eqn,R,ANS)                          !PULLS NEXT VARIABLE IN EXPRESSION
  !CHARACTER(*),DIMENSION(:),      INTENT(IN   )::Eqn                !ARRAY CONTAINING ALL PARTS OF EXPRESSION
  TYPE (EQN_TERMS), DIMENSION(:), INTENT(IN   )::Eqn                !!ARRAY CONTAINING ALL PARTS OF EXPRESSION
  INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON
  DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::ANS                !THE ATOM THAT IS RETURNED
  CONTIGUOUS:: Eqn, ANS
  !
  CHARACTER(5),PARAMETER::OP='+-*/^'
  CHARACTER(1),PARAMETER::LP='('
  CHARACTER(1),PARAMETER::RP=')'
  CHARACTER(VARLEN)::WORD
  DOUBLE PRECISION:: NUM
  INTEGER:: MP
  LOGICAL,DIMENSION(SIZE(ANS,1),SIZE(ANS,2)):: VALID
  !
  IF (R>SIZE(Eqn)) THEN
    ANS=0
  ELSE IF(LP==Eqn(R)%T)THEN          !FOUND "(" START PRECDIENCE OVER TO EVALUATE TO ")"
    MP=1
    R=R+1
    CALL EqnParser(Eqn,R,ANS,MP)
    IF( R > SIZE(Eqn,1) ) THEN
       WRITE(*,'(4A /, 9x,*(A 1x))')'EXPRESSION ERROR:  UNMATCHED PARENTHESIS SOMEWHERE AFTER ', &
                   'A "(" AND BEFORE "',Eqn(R-1)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
       WRITE(*,'( /A,//A,/A,/A,//A,/A )')'THIS ERROR COULD ALSO OCCUR IF YOU HAVE AN IF[] BLOCK THAT CONTAINS A SET OF PARENTHESIS AROUND AN INEQUALITY.', &
                   'FOR EXAMPLE:', 'IF[ (A<B), 1, 0] OR', 'IF[ (C>A & A<B) | B>C, 1, 0]','HOWEVER THIS IS ALLOWED:', 'IF[ (B+C)>A & A<B | (A+B)*C > D, 1, 0]'
       ERROR STOP
    END IF
    IF(RP/=Eqn(R)%T) THEN
       WRITE(*,'(4A /, 9x,*(A 1x))')'EXPRESSION ERROR:  UNMATCHED PARENTHESIS SOMEWHERE AFTER ', &
                   'A "(" AND BEFORE "',Eqn(R)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
       ERROR STOP
    END IF
    R=R+1
  ELSE IF( INDEX(OP,Eqn(R)%T)>0 ) THEN
    IF(R==1) R=2
    WRITE(*,'(2A, 1x 2A,/ 9x,*(A 1x))')'EXPRESSION ERROR: EXPECTED VARIABLE, BUT FOUND AN OPERATOR (ie +-*/) AT "', &
                                          Eqn(R-1)%T,Eqn(R)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) ),      &
                                          NEW_LINE(' ')//NEW_LINE(' ')//'PLEASE NOTE THAT A NEGATIVE VARIABLE "-Y + X" IS NOT ALLOWED.'//NEW_LINE(' ')//'NEGATIVE VARIABLES CAN ONLY BE DEFINED WITH THE NEG FUNCTION SUCH AS "NEG(Y) + X" OR CHANGING THE MATH TO BE "X - Y"'
    ERROR STOP
  ELSE IF(KEYWORD(Eqn(R)%T)) THEN      !KEYWORD AND PARETHESIS FOUND SO RESTART EXPRESSION SEARCH
    WORD=Eqn(R)%T
    R=R+1
    IF(LP/=Eqn(R)%T) THEN
       IF(R==1) R=2
       WRITE(*,'(3A, /,A)')'EXPRESSION ERROR: KEYWORD MUST BE FOLLOWED BY "(" ERROR LOCATED AT "',Eqn(R-1)%T, &
                           '" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
       ERROR STOP
    END IF
    MP=1
    R=R+1
    CALL EqnParser(Eqn,R,ANS,MP)                                    !FOUND "(" START PRECDIENCE OVER TO EVALUATE TO ")"
    IF(RP/=Eqn(R)%T) THEN
       WRITE(*,'(5A /, 9x,*(A 1x))')'EXPRESSION ERROR:  UNMATCHED PARENTHESIS FOR KEYWORD "',TRIM(WORD),&
                   '" THAT OCCURS BEFORE "',Eqn(R)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
       STOP
    END IF
    R=R+1
    CALL KEYWORDEVAL(WORD,ANS,VALID)                                !UPDATE ANS BY SOLVING IT WITH THE FUNCTION WORD
    !
    IF( ANY(.NOT.VALID) ) THEN
       WRITE(*,'(3A /, 9x,*(A 1x))')'EXPRESSION ERROR: BAD VALUE ( e.g. LOG(0) ) PASSED TO KEYWORD, "',TRIM(WORD),   &
                                    '" FOR EXPRESSION:', ( Eqn(MP)%T,MP=1,SIZE(Eqn) )
       ERROR STOP
    END IF
  ELSEIF ('IF' == Eqn(R)%T) THEN
      CALL PROCESS_INLINE_CONDITIONAL(Eqn,R,ANS)
  ELSEIF ('MIN'==Eqn(R)%T .OR. 'MAX'==Eqn(R)%T) THEN
      CALL PROCESS_MINMAX(Eqn,R,ANS)
  ELSE IF(' '==Eqn(R)%T)THEN
    IF(R==1) R=2
    WRITE(*,'(4A /, 9x,*(A 1x))')'EXPRESSION ERROR:  UNEXPECTED ENDING OF EXPRESSION AFTER "', &
                   Eqn(R-1)%T,'" FOR EXPRESSION:', ( Eqn(R)%T,R=1,SIZE(Eqn) )
    ERROR STOP
  ELSE
    READ(Eqn(R)%T,*,IOSTAT=MP)NUM
    IF(MP==0)THEN
      ANS=NUM
    ELSE
      ANS=VAL(:,:,MATLOC(Eqn(R)%T))                                   !MATLOC(Eqn(R)) RETURNS THE LOCATION OF MULTIPLIER ARRAY ASSOCIATED WITH VAIRABLE NAME Eqn(R)
    END IF
    R=R+1
  END IF
END SUBROUTINE
!
!######################################################################
!
RECURSIVE SUBROUTINE PROCESS_INLINE_CONDITIONAL(Eqn,R,COND_ANS)
  TYPE (EQN_TERMS), DIMENSION(:), INTENT(IN   )::Eqn                !!ARRAY CONTAINING ALL PARTS OF EXPRESSION
  INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON
  DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::COND_ANS           !HOLDS EITHER THE TRUE OR FALSE PORTION OF THE IF STATEMENT
  CONTIGUOUS:: Eqn, COND_ANS
  !
  TYPE (EQN_TERMS),DIMENSION(:),ALLOCATABLE:: SubEqn
  !
  INTEGER,DIMENSION(:),ALLOCATABLE:: IDX                             !POINTER ARRAY THAT IS ONLY USED TO POINT TO THE & AND | IN CONDITIONAL PART OF IF
  INTEGER:: I, J, Rsub, MP, ROW, COL
  !
  LOGICAL,DIMENSION(:,:), ALLOCATABLE:: COND
  LOGICAL,DIMENSION(:,:), ALLOCATABLE:: COND2   !TEMPORARY ARRAY THAT IS ONLY USED IF THERE IS AN "OR" OPERATOR, ie "|"
  !
  CHARACTER(:), ALLOCATABLE:: TRUE_LN, FALSE_LN
  CHARACTER(3):: NaN

  ROW=SIZE(COND_ANS,1)
  COL=SIZE(COND_ANS,2)
  !
  ALLOCATE(COND(ROW,COL))
  !
  ! 'IF[...'
  !
  R=R+2   !SKIP THE 'IF' AND THE LEFT BRACKET '['
  !
  ! PROCESS CONDITIONAL
  IF ( INDEX(Eqn(R)%T,'|') > 0 ) THEN  !SEARCH IF THERE IS AN "OR" OPERATOR
     !
     ALLOCATE(COND2(ROW,COL))  !REQUIRED FOR THE SECOND PART OF THE "OR" OPERATOR
     !
     I=0                                     !COUND THE NUMBER OF |'s
     DO J=1, LEN(Eqn(R)%T)
         IF ( '|' == Eqn(R)%T(J:J) ) I=I+1
     END DO
     !
     ALLOCATE(IDX(I+2))          !POINTER ARRAY OF THE LOCATION OF ALL |'s
     IDX(1)=0                    !START OF LOCATION
     IDX(I+2)=LEN(Eqn(R)%T)+1    !END OF LOCATION
     !
     I=2
     DO J=1, LEN(Eqn(R)%T)
        IF ( '|' == Eqn(R)%T(J:J) ) THEN
            IDX(I)=J                       !Store the locations from 2 to the last |
            I=I+1
        END IF
     END DO
     !
     DO I=1, SIZE(IDX,1)-1
        IF (I==1) THEN
           CALL EVAL_CONDITION(Eqn(R)%T( IDX(I)+1:IDX(I+1)-1 ), COND)    !PASS ONLY PORTION BETWEEN MULTIPLE |'s
        ELSE
           CALL EVAL_CONDITION(Eqn(R)%T( IDX(I)+1:IDX(I+1)-1 ), COND2)   !PASS ONLY PORTION BETWEEN MULTIPLE |'s
           COND = COND .OR. COND2
        END IF
     END DO
     !
     DEALLOCATE(COND2)
  ELSE
     CALL EVAL_CONDITION(Eqn(R)%T, COND)  !NO | SO EVALUATE CONDITIONAL (THERE STILL CAN BE &'s)
  END IF
  !
  R=R+1  !SKIP TO TRUE LOCATION
  TRUE_LN=Eqn(R)%T
  !
  R=R+1  !SKIP TO FALSE LOCATION
  FALSE_LN=Eqn(R)%T
  !
  DO WHILE (']' .NE. Eqn(R)%T)  !MOVE TO WHERE CLOSING BRACKET IS LOCATED
    R=R+1
  END DO
  R=R+1  !MOVE PAST CLOSING BRACKET SO NEXTATOM EITHER TERMINATES OR PROCESSES THE NEXT OPERATOR
  !
  NaN='NaN'
  DO I=1, ROW
  DO J=1, COL
    IF (COND(I,J)) THEN  !TRUE CONDITION
       CALL SplitFunc(TRUE_LN, SubEqn, .FALSE.)  !AT TRUE LOCATION EVALUATE WHAT IS THERE
       Rsub=1
       MP=1
       CALL EqnParser(SubEqn,Rsub,COND_ANS(I:I,J:J),MP)    !SOLVE FOR THE RIGHT SIDE OF THE CONDITINAL
       DEALLOCATE(SubEqn)
    ELSE
       IF(']' .NE. FALSE_LN) THEN                   !CHECK FALSE LOCATION
          CALL SplitFunc(FALSE_LN, SubEqn, .FALSE.)
          Rsub=1
          MP=1
          CALL EqnParser(SubEqn,Rsub,COND_ANS(I:I,J:J),MP)  !SOLVE FOR THE RIGHT SIDE OF THE CONDITINAL
          DEALLOCATE(SubEqn)
       ELSE
          READ(NaN,*) COND_ANS(I,J)   !SETS CONDITIONAL ANSWER TO NaN IF THERE IS NO FALSE OPTION. THIS ALLOWS THE EXPRESSION PARSER TO FLAG OUTSIDE CODES THAT IF FAILED
       END IF
    END IF
  END DO
  END DO
  !
  !DEALLOCATE(LN)
  DEALLOCATE(COND)
  DEALLOCATE(TRUE_LN, FALSE_LN)
END SUBROUTINE
!
!######################################################################
!
RECURSIVE SUBROUTINE EVAL_CONDITION(COND_LN, COND)
  CHARACTER(*),           INTENT(IN) :: COND_LN
  LOGICAL,DIMENSION(:,:), INTENT(OUT):: COND
  CONTIGUOUS:: COND
  !
  TYPE (EQN_TERMS),DIMENSION(:),ALLOCATABLE:: SubEqn
  !
  DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE:: LCOND, RCOND
  INTEGER,DIMENSION(:),   ALLOCATABLE:: IDX
  INTEGER:: I, J, ROW, COL, MP, Rsub, LINELEN
  INTEGER:: OPLOC1, OPLOC2   !LOCATION JUST BEFORE AND JUST AFTER THE LOGICAL OPERATOR
  CHARACTER(:), ALLOCATABLE:: LCOND_LN, RCOND_LN
  CHARACTER(2):: OP
  !
  LINELEN=LEN(COND_LN)
  ROW=SIZE(COND,1)
  COL=SIZE(COND,2)
  ALLOCATE(LCOND(ROW,COL), RCOND(ROW,COL))
  ALLOCATE(CHARACTER(LINELEN)::LCOND_LN)
  ALLOCATE(CHARACTER(LINELEN)::RCOND_LN)
  !
  ! COUNT THE NUMBER OF & AND FIND THEIR LOCATION
  IF ( INDEX(COND_LN,'&') > 0 ) THEN
     !
     ! COUNT NUMBER OF &
     I=0
     DO J=1, LEN(COND_LN)
         IF ( '&' == COND_LN(J:J) ) I=I+1
     END DO
     ALLOCATE(IDX(I+2))
     IDX(1)=0                   !START OF LOCATION
     IDX(I+2)=LEN(COND_LN)+1    !END OF LOCATION
     I=2
     DO J=1, LEN(COND_LN)
         IF ( '&' == COND_LN(J:J) ) THEN
             IDX(I)=J                    !Store the locations from 2 to the last &
             I=I+1
         END IF
     END DO
  ELSE
     ALLOCATE(IDX(2))
     IDX(1)=0
     IDX(2)=LEN(COND_LN)+1
  END IF
  ! '5<3&6.1>5&ABC<=8'
  ! IDX=0,4,10,17
  DO I=1, SIZE(IDX,1)-1
     OP=''
     DO J=IDX(I)+1, IDX(I+1)-1          !SEARCH FOR LOGICAL OPERATOR
        IF     ( '>=' == COND_LN(J:J+1) ) THEN
            OP='GE'
            OPLOC1=J-1
            OPLOC2=J+2
            EXIT
        ELSEIF ( '<=' == COND_LN(J:J+1) ) THEN
            OP='LE'
            OPLOC1=J-1
            OPLOC2=J+2
            EXIT
        ELSEIF ( '>' == COND_LN(J:J) ) THEN
            OP='GT'
            OPLOC1=J-1
            OPLOC2=J+1
            EXIT
        ELSEIF ( '<' == COND_LN(J:J) ) THEN
            OP='LT'
            OPLOC1=J-1
            OPLOC2=J+1
            EXIT
        END IF
     END DO
     !
     LCOND_LN=COND_LN(  IDX(I)+1:OPLOC1     )      !STORE THE TWO PARTS OF THE CONDITION  --THIS IS THE PART LEFT  OF THE INEQUALITY SYMBOL OP
     RCOND_LN=COND_LN(  OPLOC2  :IDX(I+1)-1 )      !STORE THE TWO PARTS OF THE CONDITION  --THIS IS THE PART RIGHT OF THE INEQUALITY SYMBOL OP
     !
     !FIGURE OUT WHAT IS ON THE LEFT  OF THE LOGICAL OPERATOR
     !
     CALL SplitFunc(LCOND_LN, SubEqn, .FALSE.)
     Rsub=1
     MP=1
     CALL EqnParser(SubEqn,Rsub,LCOND,MP)  !SOLVE FOR THE LEFT SIDE OF THE CONDITINAL
     !
     DEALLOCATE(SubEqn)
     !
     !FIGURE OUT WHAT IS ON THE RIGHT OF THE LOGICAL OPERATOR
     CALL SplitFunc(RCOND_LN, SubEqn, .FALSE.)
     Rsub=1
     MP=1
     CALL EqnParser(SubEqn,Rsub,RCOND,MP)  !SOLVE FOR THE RIGHT SIDE OF THE CONDITINAL
     !
     DEALLOCATE(SubEqn)
     !
     IF (I==1) THEN   ! SET INITIAL CONDITIONAL
        COND = INEQUALITY_EVAL(LCOND, RCOND, OP)
     ELSE                                    !INTIAL CONDITIONAL WAS SET AND THERE WAS AN & DETECTED, SO COMPUTE COMBINED LOGICAL
        COND = COND .AND. INEQUALITY_EVAL(LCOND, RCOND, OP)
     END IF
  END DO
  !
  DEALLOCATE(LCOND, RCOND, IDX)
  DEALLOCATE(LCOND_LN, RCOND_LN)
  !
END SUBROUTINE
!
!######################################################################
!
PURE ELEMENTAL FUNCTION INEQUALITY_EVAL(LCOND, RCOND, OP) RESULT(COND)
  DOUBLE PRECISION, INTENT(IN)::LCOND, RCOND
  CHARACTER(*),     INTENT(IN):: OP
  !
  LOGICAL:: COND
  !
  SELECT CASE (OP)
    CASE ('GE'); COND = LCOND >= RCOND
    CASE ('LE'); COND = LCOND <= RCOND
    CASE ('GT'); COND = LCOND >  RCOND
    CASE ('LT'); COND = LCOND <  RCOND
  END SELECT
  !
END FUNCTION
!
!######################################################################
!
RECURSIVE SUBROUTINE PROCESS_MINMAX(Eqn,R,MINMAX_ANS)
  TYPE (EQN_TERMS), DIMENSION(:), INTENT(IN   )::Eqn                !!ARRAY CONTAINING ALL PARTS OF EXPRESSION
  INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON
  DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::MINMAX_ANS         !HOLDS MIN OR MAX OF ALL THE VALUES WITHIN THE BRACKETS
  !
  TYPE (EQN_TERMS),DIMENSION(:),ALLOCATABLE:: SubEqn
  !
  INTEGER:: LINELEN, I, J, Rsub, MP, ROW, COL
  !
  DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE:: NEXT_VAL
  LOGICAL,DIMENSION(:,:), ALLOCATABLE:: COND
  !
  CHARACTER(:), ALLOCATABLE:: VAL_LN
  CHARACTER(3):: OPT

  OPT=Eqn(R)%T

  ! 'MAX[...' or 'MIN[...'

  R=R+2   !SKIP THE 'MAX' OR 'MIN' AND THE LEFT BRACKET '['

  LINELEN=0
  DO I=R, SIZE(Eqn)
     IF(Eqn(I)%T==']') EXIT
     J=LEN(Eqn(I)%T)
     IF(LINELEN < J ) LINELEN = J
  END DO
  ALLOCATE(CHARACTER(LINELEN)::VAL_LN)

  ROW=SIZE(MINMAX_ANS,1)
  COL=SIZE(MINMAX_ANS,2)
  ALLOCATE(NEXT_VAL(ROW,COL))
  ALLOCATE(COND(ROW,COL))

  !GET THE FIRST VALUE IN THE MINMAX SEQUENCE
  !
  VAL_LN=Eqn(R)%T
  CALL SplitFunc(VAL_LN, SubEqn, .FALSE.)
  Rsub=1
  MP=1
  CALL EqnParser(SubEqn,Rsub,MINMAX_ANS,MP)  !SOLVE FOR THE LEFT SIDE OF THE CONDITINAL
  DEALLOCATE(SubEqn)
  R=R+1
  !
  DO WHILE ( ']' .NE. Eqn(R)%T )
      VAL_LN=Eqn(R)%T
      CALL SplitFunc(VAL_LN, SubEqn, .FALSE.)
      Rsub=1
      MP=1
      CALL EqnParser(SubEqn,Rsub,NEXT_VAL,MP)  !SOLVE FOR THE LEFT SIDE OF THE CONDITINAL
      DEALLOCATE(SubEqn)
      SELECT CASE (OPT)
      CASE ('MAX'); COND = MINMAX_ANS < NEXT_VAL   !WHERE TRUE MEANS NEXT_VAL IS BIGGER  AND SHOULD REPLACE MINMAX_VAL
      CASE ('MIN'); COND = MINMAX_ANS > NEXT_VAL   !WHERE TRUE MEANS NEXT_VAL IS SMALLER AND SHOULD REPLACE MINMAX_VAL
      END SELECT
      !
      !DO CONCURRENT(I=1:ROW, J=1:COL, COND(I,J)); MINMAX_ANS(I,J) = NEXT_VAL(I,J)
      !END DO
      WHERE(COND) MINMAX_ANS = NEXT_VAL
      !
      R=R+1
  END DO
  !
  ! MOVE PAST CLOSING BRACKET SO NEXT ATOM EITHER TERMINATES OR PROCESSES THE NEXT OPERATOR
  R=R+1
  !
END SUBROUTINE
!
!######################################################################
!
PURE SUBROUTINE REMOVEBLANK(LN)                                   !REMOVE INTERNAL BLANK SPACES IN A STRING
  CHARACTER(*),INTENT(INOUT)::LN
  !
  CHARACTER(LEN(LN))::T
      CHARACTER:: TAB
  INTEGER::I, J
      !
      TAB=ACHAR(9)
  !
  LN=ADJUSTL(LN)
  !
  T=' '
  J=1
  DO I=1, LEN_TRIM(LN)
     IF(LN(I:I).NE.' ' .AND. LN(I:I).NE.TAB) THEN   !IF NOT BLANK AND NOT TAB THEN ADD CHARACTER
        T(J:J)=LN(I:I)
        J=J+1
     END IF
  END DO
  !
  LN=T
  !
END SUBROUTINE
!
! OLD VERSION:
!   PURE SUBROUTINE REMOVEBLANK(LN)                                   !REMOVE INTERNAL BLANK SPACES IN A STRING
!     CHARACTER(*),INTENT(INOUT)::LN
!     !
!     CHARACTER(LEN(LN))::T
!     CHARACTER:: C
!     INTEGER::I
!     !
!     LN=ADJUSTL(LN)
!     !
!     T=' '
!     DO I=1,LEN(TRIM(LN))
!       C=LN(I:I)
!       IF(C.NE.' '.AND.C.NE.ACHAR(9)) T=TRIM(T)//C                     !IF NOT BLANK OR TAB ADD CHARACTER
!     END DO
!     !
!     LN=T
!     !
!   END SUBROUTINE
!
!######################################################################
!
PURE SUBROUTINE SplitFunc(LN,Eqn,LN_SETUP)                           !SPLITS LINE BY VARIABLES,OPERATIONS, PARENTHESES, ALLOCATES SPACE REQUIRED FOR EQUATION STORAGE, LN_SETUP=.TRUE. => MAKE LN UPPER CASE AND REMOVE BLANK SPACES
  CHARACTER(*),INTENT(INOUT)::LN
  !CHARACTER(*),DIMENSION(:),ALLOCATABLE,INTENT(INOUT)::Eqn          !Eqn WILL BE ALLOCATED TO APPROPIATE SIZE CONTAINING SPECIFIED STRINGS FROM CALLING ROUTINE
  TYPE (EQN_TERMS),DIMENSION(:),ALLOCATABLE,INTENT(INOUT)::Eqn
  LOGICAL, INTENT(IN), OPTIONAL:: LN_SETUP
  !
  CHARACTER(5),PARAMETER::OP='+-*/^'

  CHARACTER(1),PARAMETER::LP='('
  CHARACTER(1),PARAMETER::RP=')'
  CHARACTER(1),PARAMETER::LB='['
  CHARACTER(1),PARAMETER::RB=']'
  CHARACTER(1),PARAMETER::CM=','
  CHARACTER(1),PARAMETER::NG='-'
  INTEGER::I,J,G,R,B,SymCnt,LINELEN,ISTART
  LOGICAL:: LN_CHECK
  !
  LN_CHECK=.TRUE.
  IF(PRESENT(LN_SETUP)) LN_CHECK=LN_SETUP
  !
  IF (LN_CHECK) THEN
     !MAKE LINE ALL UPPER CASE FOR COMPARISONS
     CALL UPPER(LN)
     !REMOVE ALL INTERNAL BLANK SPACES
     CALL REMOVEBLANK(LN)
  END IF
  !
  !COUNT NUMBER OF TOKENS WITH
  !R:=PAREN AND KEYWORD COUNT
  !G:=BRACE GROUP COUNT
  !J:=OP COUNT
  !B:=BRACE COUNT --SHOULD BE ZERO
  !
  B=0
  G=0
  R=0
  J=0
  LINELEN=LEN_TRIM(LN)
  ISTART = 1
  IF(LN(1:1) == NG) ISTART = 2
  DO I=ISTART,LINELEN                         !FIRST COUNT +,-,*,/,(,) and []
    IF(LB==LN(I:I)) THEN                 !FOUND '[' SKIP UNTIL ']' IS FOUND
      IF (B==0) G=G+1                    !A [ INDICATES STORAGE OF A KEYWORD AND THE [. KEYWORD IS AUTOMATICALLY COUNTED BY
      B=B+1
    END IF
    IF(CM==LN(I:I) .AND. B==1) G=G+1     !FOUND ',' WITHIN [ and ]
    IF(RB==LN(I:I)) THEN                 !SEARCHING FOR CLOSING ']'
      B=B-1
      IF (B==0) G=G+2
    END IF
    IF (B > 0) CYCLE                      !WITH TWO BRACES [ ... X ... ]
    IF(INDEX(OP,LN(I:I))>0)  J=J+1
    IF(LP==LN(I:I)) R=R+1
    IF(RP==LN(I:I)) R=R+1
  END DO
  !
  I=ISTART
  DO WHILE (I <= LINELEN-4)                !NEXT COUNT KEYWORDS  --NOTE A KEYWORD IS ALWAYS FOLLOWED BY A "("  eg ABS( or EXP( ALSO THE SMALLEST EXPRESSION WITH A KEYWORD IS LENGTH 6 SO IT SHOULD ALWAYS CHECK FOR IT
     !
     IF(LB==LN(I:I)) THEN                 !FOUND '[' SKIP UNTIL ']' IS FOUND
       I=I+1
       B=1
       DO WHILE (B > 0 .AND. I < LINELEN-4)  !NOTE THAT THIS WILL STOP ONE LESS THAN THE OUTER LOOP.
         IF(LB==LN(I:I)) B=B+1
         IF(RB==LN(I:I)) B=B-1
         I=I+1
       END DO
     END IF
     !
     IF     ( KEYWORD(LN(I:I+2))  .AND. LN(I+3:I+3)==LP ) THEN
        R=R+1
        I=I+3
     ELSEIF ( KEYWORD(LN(I:I+3))  .AND. LN(I+4:I+4)==LP ) THEN
        R=R+1
        I=I+4
     END IF
     I=I+1
  END DO
  !
  SymCnt=2*J+1+R+G  !COUNT OF ALL TOKENS
  !
  !IF(G>0) SymCnt=SymCnt-1
  !IF (J==0) SymCnt=SymCnt-1  !THERE ARE NO OPERATORS SO COUNT IS OFF BY 1
  !
  IF(ALLOCATED(Eqn)) DEALLOCATE(Eqn)
  ALLOCATE(Eqn(SymCnt))
  !
  R=1
  I=1
  J=ISTART  !J IS NOW THE POSITION. WORDS WILL BE FROM I:J-1
  DO WHILE (J.LE.LEN(LN) .AND. R.LE.SymCnt)
    IF(LB==LN(J:J)) THEN                      ! FOUND A LEFT BRACKET, [, NOW BEGIN SEARCH FOR RIGHT BRACKET
      IF(I<J) THEN
        Eqn(R)%T=LN(I:J-1)
        R=R+1
      END IF
      Eqn(R)%T=LN(J:J)
      R=R+1
      J=J+1
      I=J
      B=1
      DO WHILE (B > 0 .AND. J.LE.LINELEN)      !BEGIN SEARCH FOR "]", B KEEPS COUNT OF HOW MANY LEFT BRACKETS ARE FOUND
         IF(CM==LN(J:J) .AND. B==1) THEN
            Eqn(R)%T=LN(I:J-1)
            R=R+1
            I=J+1
         END IF
         IF(LB==LN(J:J)) B=B+1
         IF(RB==LN(J:J)) B=B-1
         J=J+1
      END DO
      J=J-1                 !DUE TO LOOP DESIGN J IS ONE PAST BRACKET
      Eqn(R)%T=LN(I:J-1)
      R=R+1
      Eqn(R)%T=LN(J:J)
      R=R+1
      J=J+1
      I=J
      CYCLE
    END IF
    !
    IF(INDEX(OP,LN(J:J))>0)THEN              ! FOUND AN OPERATOR SO STORE PRECEDING INFORMATION AND OPERATOR
      IF(I<J) THEN
        Eqn(R)%T=LN(I:J-1)
        R=R+1
      END IF
      Eqn(R)%T=LN(J:J)
      R=R+1
      J=J+1
      I=J
      CYCLE
    END IF
    !
    IF(LP==LN(J:J))THEN                     ! FOUND A LEFT PARENTHESIS STORE AND START LOOP OVER
      Eqn(R)%T=LN(J:J)
      R=R+1
      J=J+1
      I=J
      CYCLE
    END IF
    !
    IF(RP==LN(J:J))THEN                    ! FOUND A RIGHT PARENTHESIS STORE AND START LOOP OVER
      IF (I<J)THEN
        Eqn(R)%T=LN(I:J-1)
        R=R+1
      END IF
      Eqn(R)%T=LN(J:J)
      R=R+1
      J=J+1
      I=J
      CYCLE
    END IF
    !
    IF( KEYWORD(LN(I:J)) )THEN            ! FOUND KEYWORD, STORE AND START LOOP OVER
      Eqn(R)%T=LN(I:J)
      R=R+1
      J=J+1
      I=J
      CYCLE
    END IF
    !
    IF(' '==LN(J:J)) EXIT                !REACHED END OF EXPRESSION TERMINATE THE LOOP
    !
    J=J+1
  END DO
  !
  IF (I<J)Eqn(R)%T=TRIM(LN(I:))
  !
END SUBROUTINE
!
!######################################################################
!
FUNCTION PRECEDENCE(OP,DIRECT)                                    !DETERMINES PRIORITY OF OPERATION ***CAN ADD ADDITION OPERATORS LIKE LOG/EXP
  INTEGER::PRECEDENCE
  CHARACTER(*),INTENT(IN   )         ::OP                           !OPERATION
  CHARACTER(*),INTENT(INOUT),OPTIONAL::DIRECT                       !DIRECTION OF OPERATION
  !
  !
  SELECT CASE(OP)
  CASE ('+', '-')
    PRECEDENCE=1
    IF(PRESENT(DIRECT))DIRECT='L'
  CASE ('*','/')
    PRECEDENCE=2
    IF(PRESENT(DIRECT))DIRECT='L'
  CASE ('^')
    PRECEDENCE=3
    IF(PRESENT(DIRECT))DIRECT='R'
  CASE DEFAULT
    WRITE(*,'(3A)') 'EXPRESSION ERROR WITH FUNCTION OpInfo(OP): UNABLE TO UNDERSTAND OPERATOR "',OP,'". IT IS NOT DEFINED IN FUNCTION.'
    ERROR STOP
  END SELECT
END FUNCTION
!
!######################################################################
!
PURE ELEMENTAL FUNCTION MathOP(OP,L,R) RESULT(ANS)                     !APPLY THE CURRENT OPPORATOR TO THE TWO ADJACENT VARIABLES
  CHARACTER(*),    INTENT(IN)::OP
  DOUBLE PRECISION,INTENT(IN)::L,R
  DOUBLE PRECISION           ::ANS
  !
  SELECT CASE(OP)
  CASE ('+');  ANS =  L+R
  CASE ('-');  ANS =  L-R
  CASE ('*');  ANS =  L*R
  CASE ('/');  ANS =  L/R
  CASE ('^')
           IF(L==0D0) THEN
               ANS =  0D0
           ELSE
               ANS =  L**R
           END IF
  END SELECT
  !
END FUNCTION
!
!######################################################################
!
FUNCTION MATLOC(NAME)                                             !RETURNS LOCATION OF MULT ARRAY BASED ON NAME  **ASSUMES MLTNAM HAS BEEN INITIALIZED TO ' '
  INTEGER::MATLOC
  CHARACTER(*):: NAME
  !
  INTEGER:: I,N
  !
  N=SIZE(VAR)
  !
  DO I=1, N
    IF(NAME.EQ.VAR(I))THEN
      MATLOC=I
      EXIT
    END IF
    IF (I.EQ.N) THEN
      WRITE(*,'(3A,*(/A))') 'EXPRESSION ERROR WITH FUNCTION MATLOC(NAME): COULD NOT FIND VARIABLE WITH NAME "',NAME, &
                      '" IN LIST OF PROVIDED ARRAY NAMES.','IT MAY NOT BE DEFINED YET OR IS A SPELLING ERROR.',   &
                      'IF THERE IS AN OPERATOR, + - / * IN THE NAME, THEN THE PARSER HAD AN ISSUE WITH THE EXPRESSION STRUCTURE.','MAYBE YOU HAVE TWO OPERATORS IN A ROW, LIKE "X+-Y"','OR YOU HAVE A NEGATIVE SYMBOL WHEN YOU SHOULD USE THE NEG FUNCTION "-Y+X" INSTEAD OF "NEG(Y)+X"'
      ERROR STOP
    END IF
  END DO
  !
END FUNCTION
!
!######################################################################
!
PURE ELEMENTAL FUNCTION KEYWORD(WORD) RESULT(KEY)                      !LIST OF KEYWORDS
  CHARACTER(*),INTENT(IN)::WORD

  LOGICAL:: KEY
  !
  KEY=ANY(WORD==KEYWORDLIST)       !KEYWORDLIST IS A GLOBAL VARIABLE THAT CONTAINS ALL DEFINED KEY WORDS (eg LOG)
  !
END FUNCTION
!
!######################################################################
!
PURE ELEMENTAL SUBROUTINE KEYWORDEVAL(WORD,ANS,VALID)
   CHARACTER(*),     INTENT(IN   )::WORD
   DOUBLE PRECISION, INTENT(INOUT)::ANS
   LOGICAL,          INTENT(OUT)::VALID
   !
   VALID=.TRUE.
   !
   SELECT CASE(WORD)
   CASE('ABS');  ANS=ABS(ANS)
   CASE('EXP');  ANS=EXP(ANS)
   CASE('LOG')
               IF(ANS<=0D0) THEN
                 VALID=.FALSE.
               ELSE
                 ANS=LOG(ANS)
               END IF
   CASE('L10')
               IF(ANS<=0D0) THEN
                 VALID=.FALSE.
               ELSE
                 ANS=LOG10(ANS)
               END IF
   CASE('NEG');  ANS=-1D0*ANS
   CASE('SQRT')
               IF(ANS<0D0) THEN
                 VALID=.FALSE.
               ELSE
                 ANS=SQRT(ANS)
               END IF
   CASE('ROUND');  ANS=NINT(ANS)
   CASE('TRUNC');  ANS=AINT(ANS)
   CASE('FLOOR');  ANS=DBLE(FLOOR(ANS))
   CASE('CEIL' );  ANS=DBLE(CEILING(ANS))
   CASE DEFAULT
               VALID=.FALSE.
  END SELECT
  !
END SUBROUTINE
!
!######################################################################
!
SUBROUTINE KEYWORDCHECK(LIST)
  CHARACTER(*),DIMENSION(:)::LIST
  INTEGER,DIMENSION(SIZE(LIST)):: FOUND
  INTEGER:: NLIST
  !
  INTEGER::I,J,N,NKEY
  !
  FOUND=0
  N=LEN(LIST(1))
  NKEY=SIZE(KEYWORDLIST)
  NLIST=SIZE(LIST)
  !
  DO I=1, NLIST
     DO J=1, NKEY
        IF( INDEX( LIST(I),TRIM(KEYWORDLIST(J)) ) >0 ) THEN
           FOUND(I)=J
           EXIT
        END IF
     END DO
  END DO
  !
  IF(ANY( FOUND>0 )) THEN
    WRITE(*,'(A,/A)') 'EXPRESSION ERROR: KEYWORD FOUND IN VARIABLE NAME LIST (NML) THE FOLLOWING ARE THE VARIABLES AND KEYWORDS THAT WERE FOUND:'
    DO I=1,NLIST
       IF(FOUND(I)>0) WRITE(*,'(2(5x,A))') TRIM(LIST(I)), TRIM( KEYWORDLIST( FOUND(I) )  )
    END DO
        WRITE(*,'(/A)') '***THIS IS NOT ALLOWED; PROGRAM WILL NOW TERMINATE***'
        ERROR STOP
  END IF
  !
END SUBROUTINE
!!######################################################################
!
PURE ELEMENTAL SUBROUTINE UPPER(LN)  !CLONE OF UTIL_INTERFACE SUBROUTINE OF SAME NAME
    CHARACTER(*),INTENT(INOUT):: LN
    INTEGER, PARAMETER:: IDIFF=ICHAR('a')-ICHAR('A')
    INTEGER::I
    !
    DO CONCURRENT(I=1:LEN(LN), LN(I:I).GE.'a' .AND. LN(I:I).LE.'z');  LN(I:I)=CHAR(ICHAR(LN(I:I))-IDIFF)
    END DO
    !
END SUBROUTINE
!
!
!######################################################################
!######################################################################
!
END MODULE


