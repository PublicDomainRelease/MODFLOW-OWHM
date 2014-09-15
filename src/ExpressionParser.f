! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
!TWO MAIN AND INDEPENDENT MODULES
!
! MODULE ExpressionParser
!  DESIGNED TO TAKE IN A LIST OF VARIABLE VALUES AND EVALUTE
!  VERSION 1.1 [3/30/2014] ADDED THE ABILITY TO HAVE KEYWORD FUNCTIONS 
!                          SUCH AS "EXP", NUMERICAL INPUT, AND NEW ELEMENTAL SUBROUTINE UPPER
!  VERSION 1.0 [5/01/2013] ORIGINAL VERSION THAT SUPPORTS 5 OPERATION, +-*/^, PARISING WITH ( )
!
! MODULE CVT2STR 
! DESIGNED TO TAKE A SINGLE/DOUBLE/INTEGER AND CONVERT IT TO A STRING
!
! MODULE IS DESIGNED TO TAKE IN A LIST OF VARIABLE VALUES AND EVALUTE
! AN EXPRESSION CONTAINING THE VARIABLES WITHIN A STRING
! THE EXPRESSION FOLLOWS ORDER PRECIDENCE (ORDER OF OPERATIONS)
      !
      MODULE ExpressionParser                                           !MODULE THAT TAKES IN A STRING EXPRESSION, REMOVES BLANKS, SOLVES FOR VARIABLE DEFINITIONS
      PRIVATE                                                           !MAKES ALL VARIABLES AND FUNCTIONS PRIVATE
      INTEGER,PARAMETER:: VARLEN=25                                     !LENTH OF VARIABLE NAMES
      CHARACTER(VARLEN),   ALLOCATABLE,DIMENSION(:)::VAR                !NAME OF VARIABLES USED MAX LEN OF 25
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:,:)::VAL                !STORED VALUES OF VARIABLES, 3rd DIM IS 1to1 WITH VAR NAMES
      !
      PUBLIC::  UPPER                                                   !SUBROUTINE THAT CONVERTS A STRANGE OR AN ARRAY OF STRINGS TO UPPER CASE. THIS IS PROVIDED IN CASE ANY OF THE INPUT VARIABLES NEED PREPROCESSING FOR SPEED. THIS SUBORINTE IS USED INTERNALLY IF THE USER REQUESTS IT.
      PUBLIC::  EqnEval                                                 !THIS IS THE DRIVER INTERFACE OF THE MODULE AND DIRECTS THE INPUTS TO THE APPORPIATE SUBROUTINES. EqnEval DEFERS TO 3 DIFFERENT FUNCTIONS DEPENDING ON INPUT
      !
      INTERFACE EqnEval
        MODULE PROCEDURE EqnEval1D
        MODULE PROCEDURE EqnEval2D
        MODULE PROCEDURE EqnEval3D
      END INTERFACE 
      !
      CHARACTER(5),PARAMETER,DIMENSION(6)::KEYWORDLIST=
     + ['ABS  ','EXP  ','LOG  ','L10  ','NEG  ','SQRT '] 
      !
      CONTAINS
!###################################################################### START DRIVER ROUTINES THAT RESHAPE ARRAYS     
!######################################################################
      !
      FUNCTION EqnEval1D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)         !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT A COPY OF NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
      IMPLICIT NONE
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
      CALL EqnEvalRun(Ln,ANS)
      !
      RES=ANS(1,1)
      !
      DEALLOCATE(VAR,VAL,ANS)
      !
      END FUNCTION
      !
!######################################################################      
      !
      FUNCTION EqnEval2D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)         !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
      IMPLICIT NONE
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
      CALL EqnEvalRun(Ln,ANS)
      !
      RES=RESHAPE(ANS,[ROW])
      !
      DEALLOCATE(VAR,VAL,ANS)
      END FUNCTION
      !
!######################################################################      
      !
      FUNCTION EqnEval3D(Ln,NML,NMV,CHKCASE,CHKKEY) RESULT(RES)         !Ln='Actual Expression',NML='Named Variable List',NMV='Named Variable Values',CHKCASE=OPTIONAL:CONVERT NML TO UPPER,CHKKEY=OPTIONAL:CHECK FOR KEYWORD IN NML,RES='Expression Result'
      IMPLICIT NONE
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
      VAL=RESHAPE(NMV,[ROW,COL,NVAR])
      VAL=NMV
      ANS=0D0
      !
      CALL EqnEvalRun(Ln,ANS)
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
      SUBROUTINE EqnEvalRun(Ln,ANS)
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)::Ln                                       !STRING THAT CONTAINS EXPRESSION
      DOUBLE PRECISION,DIMENSION(:,:)::ANS                              !FINAL ANSWER TO FROM EXPRESSION
      !
      CHARACTER(VARLEN),DIMENSION(:),ALLOCATABLE::Eqn                   !ARRAY CONTAINING ALL PARTS OF EXPRESSION
      CHARACTER(LEN(Ln))::Ln2
      INTEGER::R,MP
      !
      Ln2=Ln                                                            !COPY STRING FOR EDITING
      !
      CALL SplitFunc(LN2,Eqn)                                           !SPLIT STRING INTO AN ARRAY OF STRINGS FOR EACH VARIABLE, OPERATOR, AND PAREN
      !
      R=1
      MP=1      
      CALL EqnParser(EQN,R,ANS,MP)                                      !SOLVE FOR ALL PIECES OF EQN
      !
      END SUBROUTINE
      !
!######################################################################            
      !
      RECURSIVE SUBROUTINE EqnParser(EQN,R,ANS,MP)                      !EVALUATE PARTS OF AN EXPRESSION
      IMPLICIT NONE
      CHARACTER(VARLEN),DIMENSION(:), INTENT(IN   )::Eqn                !ARRAY CONTAINING ALL PARTS OF EXPRESSION
      INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON 
      INTEGER,                        INTENT(IN   )::MP                 !MINIMUM PRECDIENCE OF CURRENT EXPRESSION
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::ANS                !CAN THINK OF THIS AS THE LEFT HAND SIDE OF THE OPERATION (LHS op RIGHT)
      !
      DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE::RHS
      INTEGER::PREC
      INTEGER::ROW,COL
      CHARACTER(5)::OP='+-*/^'
      CHARACTER(1)::LP='('
      CHARACTER(1)::RP=')'
      CHARACTER(1)::DIRECTION,COP  !DIRECTION OF OPERATOR, CURRENT OPERATOR
      CHARACTER(150)::ERR
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
        IF ( INDEX(OP,Eqn(R)(1:1)) < 1 ) EXIT                           !ENSURE THAT IT IS AN OPERATOR 
        IF ( PRECEDENCE(Eqn(R)) < MP   ) EXIT                           !PRECEDENCE LEVEL HAS BEEN REDUCED SO EXIT TO BACK CALCULATE
        !
        COP=Eqn(R)(1:1)                                                 !CURRENT OPERATION
        PREC=PRECEDENCE(COP,DIRECTION)                                  !RERETRIVE PRECEDENCE LEVEL AND GET THE DIRECTION OF THE OPERATOR
        !
        IF (DIRECTION=='L')THEN                                         !DIRECTION IS TO THE LEFT
          PREC=PREC+1
        !ELSE DIRECTION=='R'                                            !DIRECTION IS TO THE RIGHT NO NEED FOR UPDATE
        ! PREC = PREC  
        END IF
        R=R+1
        CALL EqnParser(EQN,R,RHS,PREC)
        !
        IF(COP=='/')THEN
        IF( ANY(ABS(RHS)<1D-30) ) THEN
         WRITE(*,'(2A,/ 12x A,/ 12x,*(A))')'EXPRESSION: ',
     +      'DIVIDE BY 0 ERROR DURING AN OPERATION',
     +      'FOR EXPRESSION:', ( TRIM(Eqn(R)),R=1,SIZE(Eqn) )
          STOP
        END IF
        END IF
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
      IMPLICIT NONE
      CHARACTER(*),DIMENSION(:),      INTENT(IN   )::Eqn                !ARRAY CONTAINING ALL PARTS OF EXPRESSION
      INTEGER,                        INTENT(INOUT)::R                  !CURRENT INDEX OF ARRAY TO OPERATE ON 
      DOUBLE PRECISION,DIMENSION(:,:),INTENT(INOUT)::ANS
      !
      CHARACTER(5)::OP='+-*/^'
      CHARACTER(1)::LP='('
      CHARACTER(1)::RP=')'
      CHARACTER(VARLEN)::WORD
      DOUBLE PRECISION:: NUM
      INTEGER:: MP
      LOGICAL,DIMENSION(SIZE(ANS,1),SIZE(ANS,2)):: VALID
      !
      IF (R>SIZE(Eqn)) THEN
        ANS=0
      ELSE IF(LP==Eqn(R)(1:1))THEN
        MP=1
        R=R+1
        CALL EqnParser(EQN,R,ANS,MP)                                    !FOUND "(" START PRECDIENCE OVER TO EVALUATE TO ")"
        IF(RP/=Eqn(R)(1:1)) STOP 'EXPRESSION: UNMATCHED "(" with ")"'
        R=R+1
      ELSE IF( INDEX(OP,Eqn(R)(1:1))>0 ) THEN
        STOP 'EXPRESSION: EXPECTED VARIABLE, BUT READ OPERATOR: +-*/'
      ELSE IF(' '==Eqn(R)(1:1))THEN
        STOP 'EXPRESSION: UNEXPECTED ENDING OF EXPRESSION READ'
      ELSE IF(KEYWORD(Eqn(R))) THEN
        WORD=Eqn(R)
        R=R+1
        IF(LP/=Eqn(R)(1:1)) 
     +                STOP 'EXPRESSION: KEYWORD MUST BE FOLLOWED BY "("'
        MP=1
        R=R+1
        CALL EqnParser(EQN,R,ANS,MP)                                    !FOUND "(" START PRECDIENCE OVER TO EVALUATE TO ")"
        IF(RP/=Eqn(R)(1:1)) 
     +             STOP 'EXPRESSION: KEYWORD HAS UNMATCHED "(" with ")"'
        R=R+1
        CALL KEYWORDEVAL(WORD,ANS,VALID)                                !UPDATE ANS BY SOLVING IT WITH THE FUNCTION WORD
        !
        IF( ANY(.NOT.VALID) ) THEN
         WRITE(*,'(4A,/ 12x A,/ 12x,*(A))')'EXPRESSION: BAD VALUE, ',
     +      'i.e. <=0 or <0, PASSED TO KEYWORD, ',TRIM(WORD),', ',
     +      'FOR EXPRESSION:', ( TRIM(Eqn(MP)),MP=1,SIZE(Eqn) )
          STOP
        END IF
            
      ELSE
        READ(Eqn(R),*,IOSTAT=MP)NUM
        IF(MP==0)THEN
          ANS=NUM
        ELSE
          ANS=VAL(:,:,MLTLOC(Eqn(R)))                                   !MLTLOC(Eqn(R)) RETURNS THE LOCATION OF MULTIPLIER ARRAY ASSOCIATED WITH VAIRABLE NAME Eqn(R)
        END IF
        R=R+1
      END IF
      END SUBROUTINE
      !
!######################################################################      
      !
      PURE SUBROUTINE REMOVEBLANK(LN)                                   !REMOVE INTERNAL BLANK SPACES IN A STRING
      IMPLICIT NONE
      CHARACTER(*),INTENT(INOUT)::LN
      !
      CHARACTER(LEN(LN))::T
      CHARACTER:: C
      INTEGER::I
      !
      LN=ADJUSTL(LN)
      !
      T=' '
      DO I=1,LEN(TRIM(LN))
        C=LN(I:I)
        IF(C.NE.' '.AND.C.NE.ACHAR(9)) T=TRIM(T)//C                     !IF NOT BLANK OR TAB ADD CHARACTER
      END DO
      !
      LN=T
      !
      END SUBROUTINE
      !
!######################################################################    
      !
      PURE SUBROUTINE SplitFunc(LN,Eqn)                                 !SPLITS LINE BY VARIABLES,OPERATIONS, PARENTHESES, ALLOCATES SPACE REQUIRED FOR EQUATION STORAGE
      IMPLICIT NONE
      CHARACTER(*),INTENT(INOUT)::LN
      CHARACTER(*),DIMENSION(:),ALLOCATABLE,INTENT(INOUT)::Eqn          !Eqn WILL BE ALLOCATED TO APPROPIATE SIZE CONTAINING SPECIFIED STRINGS FROM CALLING ROUTINE
      !
      CHARACTER(5),PARAMETER::OP='+-*/^'
      CHARACTER(1),PARAMETER::LP='('
      CHARACTER(1),PARAMETER::RP=')'
      INTEGER::I,J,R,SymCnt,LINELEN
      !
      !MAKE LINE ALL UPPER CASE FOR COMPARISONS
      CALL UPPER(LN)
      !REMOVE ALL INTERNAL BLANK SPACES
      CALL REMOVEBLANK(LN)
      !
      !COUNT NUMBER OF TOKENS WITH 
      !R:=PAREN AND KEYWORD COUNT
      !J:=OP COUNT
      !
      R=0
      J=0
      LINELEN=LEN(LN)
      DO I=1,LINELEN                        !FIRST COUNT +,-,*,/,(,)
        IF(INDEX(OP,LN(I:I))>0)  J=J+1
        IF(LP==LN(I:I)) R=R+1
        IF(RP==LN(I:I)) R=R+1
      END DO
      !
      DO I=1,LINELEN-4                      !NEXT COUNT KEYWORDS
        IF( KEYWORD(LN(I:I+4)) ) THEN
          R=R+1
          CYCLE
        END IF
        IF( KEYWORD(LN(I:I+3)) ) THEN
          R=R+1
          CYCLE
        END IF
        IF( KEYWORD(LN(I:I+2)) ) THEN
          R=R+1
          CYCLE
        END IF
      END DO
      !
      SymCnt=2*J+1+R  !COUNT OF ALL TOKENS
      !
      IF(ALLOCATED(Eqn)) DEALLOCATE(Eqn)
      ALLOCATE(Eqn(SymCnt))
      !
      R=1
      I=1
      J=1
      DO WHILE (J.LE.LEN(LN).AND.R.LE.SymCnt)
        IF(INDEX(OP,LN(J:J))>0)THEN
          IF(I<J) THEN
            EQN(R)=LN(I:J-1)
            R=R+1
          END IF  
          EQN(R)=LN(J:J)
          R=R+1
          J=J+1
          I=J
          CYCLE
        END IF
        !
        IF(LP==LN(J:J))THEN
          EQN(R)=LN(J:J)
          R=R+1
          J=J+1
          I=J
          CYCLE
        END IF
        !
        IF(RP==LN(J:J))THEN
          IF (I<J)THEN
            EQN(R)=LN(I:J-1)
            R=R+1
          END IF  
          EQN(R)=LN(J:J)
          R=R+1
          J=J+1
          I=J
          CYCLE
        END IF
        !
        IF( KEYWORD(LN(I:J)) )THEN
          EQN(R)=LN(I:J)
          R=R+1
          J=J+1
          I=J
          CYCLE
        END IF
        IF(' '==LN(J:J)) EXIT
        J=J+1
      END DO
      !
      IF (I<J)EQN(R)=TRIM(LN(I:))
      !
      END SUBROUTINE 
      !
!######################################################################       
      !
      FUNCTION PRECEDENCE(OP,DIRECT)                                    !DETERMINES PRIORITY OF OPERATION ***CAN ADD ADDITION OPERATORS LIKE LOG/EXP
      IMPLICIT NONE
      INTEGER::PRECEDENCE
      CHARACTER(*),INTENT(IN   )         ::OP                           !OPERATION
      CHARACTER(*),INTENT(INOUT),OPTIONAL::DIRECT                       !DIRECTION OF OPERATION
      !
      CHARACTER(150)::ERR
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
        ERR='FUNCTION OpInfo(OP) ERROR:  OPERATOR: '//OP//' NOT FOUND'
        WRITE(*,'(A)') TRIM(ERR)
        STOP 
      END SELECT
      END FUNCTION
      !
!######################################################################
      !
      ELEMENTAL FUNCTION MathOP(OP,L,R) RESULT(ANS)                     !APPLY THE CURRENT OPPORATOR TO THE TWO ADJACENT VARIABLES
      IMPLICIT NONE
      CHARACTER(*),    INTENT(IN)::OP
      DOUBLE PRECISION,INTENT(IN)::L,R
      DOUBLE PRECISION           ::ANS
      !
      SELECT CASE(OP)
      CASE ('+');  ANS =  L+R
      CASE ('-');  ANS =  L-R
      CASE ('*');  ANS =  L*R
      CASE ('/');  ANS =  L/R
      CASE ('^');  ANS =  L**R
      END SELECT
      !
      END FUNCTION
      !
!######################################################################       
      !
      FUNCTION MLTLOC(NAME)                                             !RETURNS LOCATION OF MULT ARRAY BASED ON NAME  **ASSUMES MLTNAM HAS BEEN INITIALIZED TO ' '
      IMPLICIT NONE
      INTEGER::MLTLOC
      CHARACTER(*):: NAME
      !
      CHARACTER(150)ERR
      INTEGER:: I,N
      !
      N=SIZE(VAR)
      !
      DO I=1, N
        IF(NAME.EQ.VAR(I))THEN
          MLTLOC=I
          EXIT
        END IF
        IF (I.EQ.N) THEN
          ERR='MLTLOC(NAME) ERROR: COULD NOT FIND MULT MATRIX: '
     +        //NAME//' IN LIST OF MULT ARRAYS'
          WRITE(*,'(A)')TRIM(ERR)
          STOP 
        END IF
      END DO
      !
      END FUNCTION
      !
!######################################################################    
      !
      ELEMENTAL FUNCTION KEYWORD(WORD) RESULT(KEY)                      !LIST OF KEYWORDS
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN)::WORD
!      CHARACTER(5),PARAMETER,DIMENSION(6)::KEYWORDLIST=
!     + ['ABS','EXP','LOG','L10','NEG','SQRT'] 
      LOGICAL:: KEY
      !
      KEY=ANY(WORD==KEYWORDLIST)
!      KEY=.FALSE.
!      IF(WORD=='ABS')  KEY=.TRUE.
!      IF(WORD=='EXP')  KEY=.TRUE.
!      IF(WORD=='LOG')  KEY=.TRUE.
!      IF(WORD=='NEG')  KEY=.TRUE.
!      IF(WORD=='SQRT') KEY=.TRUE.
      !
      END FUNCTION
      !
!######################################################################    
      !
      ELEMENTAL SUBROUTINE KEYWORDEVAL(WORD,ANS,VALID)
      IMPLICIT NONE
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
!      CASE DEFAULT                                                     !FUNCTION CONVERTED TO PURE FOR SPEED, HOWEVER THIS NEGATES THE ABILITY OF I/O AND STOP
!        WRITE(*,'(3A)')'EXPRESSION: CODE ERROR - KEYWORD, ',TRIM(WORD),  
!        ', IS NOT SPECIFIED IN KEYWORDEVAL() FOR EVALUATION.'
!        STOP
      END SELECT
      !
      END SUBROUTINE
      !
!######################################################################    
      !
      SUBROUTINE KEYWORDCHECK(LIST)
      IMPLICIT NONE
      CHARACTER(*),DIMENSION(:)::LIST
      INTEGER,DIMENSION(SIZE(LIST)):: FOUND
      INTEGER:: NLIST
!      CHARACTER(5),PARAMETER,DIMENSION(6)::KEYWORDLIST=
!     + ['ABS','EXP','LOG','L10','NEG','SQRT'] 
      !
      LOGICAL:: KEYFOUND
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
        WRITE(*,'(A,/A)')
     +   'EXPRESSION: KEYWORD FOUND IN VARIABLE NAME LIST (NML)',
     +   'THE FOLLOWING ARE THE VARIABLES AND KEYWORDS THAT WERE FOUND:'
        DO I=1,NLIST
          IF(FOUND(I)>0) WRITE(*,'(2(5x,A))')
     +                   TRIM(LIST(I)), TRIM( KEYWORDLIST( FOUND(I) )  )
        END DO
        WRITE(*,*)
        STOP '***THIS IS NOT ALLOWED; PROGRAM WILL NOW TERMINATE***'
      END IF
      !
      END SUBROUTINE
      !!######################################################################    
      !
      ELEMENTAL SUBROUTINE UPPER(LN)
      IMPLICIT NONE
      CHARACTER(*),INTENT(INOUT):: LN
      INTEGER,PARAMETER::IDIFF=ICHAR('a')-ICHAR('A')
      INTEGER::I
      !
      FORALL(I=1:LEN(LN), LN(I:I).GE.'a' .AND. LN(I:I).LE.'z')
         LN(I:I)=CHAR(ICHAR(LN(I:I))-IDIFF)
      END FORALL
      !
      END SUBROUTINE 
      !
      !
!######################################################################
!######################################################################    
      !
      END MODULE
C
C     
C MODULE IS DESIGNED TO TAKE A SINGLE/DOUBLE/INTEGER AND CONVERT IT TO A STRING
C GENERIC CALL IS MADE AS NUM2STR(X) WHERE X CAN BE SINGLE/DOUBLE/INTEGER RETURNS 'X'
      MODULE CVT2STR
      !
      PRIVATE
      PUBLIC::  NUM2STR
      INTERFACE NUM2STR
        MODULE PROCEDURE INT2STR
        MODULE PROCEDURE REAL2STR
        MODULE PROCEDURE DBLE2STR
      END INTERFACE
      !
      CONTAINS
      !
      PURE FUNCTION INT2STR(IVAL)
      INTEGER,        INTENT(IN):: IVAL
      CHARACTER(:), ALLOCATABLE :: INT2STR
      INTEGER:: NCHAR
      CHARACTER(30)::NUM
      !
      WRITE(NUM,'(I30)') IVAL
      !
      INT2STR=TRIM(ADJUSTL(NUM))
      !
      END FUNCTION
      !
      PURE FUNCTION REAL2STR(RVAL,GENERAL)
      REAL,            INTENT(IN):: RVAL
      LOGICAL,OPTIONAL,INTENT(IN):: GENERAL
      CHARACTER(:),  ALLOCATABLE :: REAL2STR
      CHARACTER(50)::NUM
      LOGICAL::GEN
      GEN=.FALSE.
      IF(PRESENT(GENERAL))GEN=GENERAL
      !
      NUM=''
      IF(.NOT. GEN) THEN
       !
       IF(RVAL==0E0)                               THEN
          WRITE(NUM,'(F3.1)') RVAL
       ELSEIF(RVAL>=1E10 .OR. RVAL<=-1E10)         THEN
          WRITE(NUM,'(ES40.7E2)') RVAL
       ELSEIF(RVAL>=1D6 .OR. RVAL<=-1D6)           THEN
          WRITE(NUM,'(ES40.7E1)') RVAL
       ELSEIF(RVAL>=1E0 .OR. RVAL<=-1E0 )          THEN
          WRITE(NUM,'(F40.5)') RVAL
       ELSEIF(RVAL>=0.001E0 .OR. RVAL<=-0.001E0 )  THEN
          WRITE(NUM,'(F40.7)') RVAL
       ELSEIF(RVAL>1E-9 .OR. RVAL<-1E-9)           THEN
          WRITE(NUM,'(ES40.5E1)') RVAL
       ELSEIF(RVAL>0E0 .OR. RVAL<0E0)              THEN
          WRITE(NUM,'(ES40.5E2)') RVAL
       END IF
       !
      ELSE
          WRITE(NUM,'(1PG15.6)') RVAL 
      END IF
      !
      REAL2STR=TRIM(ADJUSTL(NUM))
      !
      END FUNCTION
      !
      PURE FUNCTION DBLE2STR(DVAL,GENERAL)
      DOUBLE PRECISION,INTENT(IN):: DVAL
      LOGICAL,OPTIONAL,INTENT(IN):: GENERAL
      CHARACTER(:),  ALLOCATABLE :: DBLE2STR
      CHARACTER(50)::NUM
      LOGICAL::GEN
      GEN=.FALSE.
      IF(PRESENT(GENERAL))GEN=GENERAL
      !
      NUM=''
      !
      IF(.NOT. GEN) THEN
       !
       IF(DVAL==0D0)                               THEN
          WRITE(NUM,'(F3.1)') DVAL
       ELSEIF(DVAL>=1D100 .OR. DVAL<=-1D100)       THEN
          WRITE(NUM,'(ES40.7E3)') DVAL
       ELSEIF(DVAL>=1D10 .OR. DVAL<=-1D10)         THEN
          WRITE(NUM,'(ES40.7E2)') DVAL
       ELSEIF(DVAL>=1D6 .OR. DVAL<=-1D6)           THEN
          WRITE(NUM,'(ES40.7E1)') DVAL
       ELSEIF(DVAL>=1D0 .OR. DVAL<=-1D0 )          THEN
          WRITE(NUM,'(F40.5)') DVAL
       ELSEIF(DVAL>=0.001D0 .OR. DVAL<=-0.001D0 )  THEN
          WRITE(NUM,'(F40.7)') DVAL
       ELSEIF(DVAL>=1D-9 .OR. DVAL<=-1D-9)         THEN
          WRITE(NUM,'(ES40.5E1)') DVAL
       ELSEIF(DVAL>=1D-99 .OR. DVAL<=-1D-99)       THEN
          WRITE(NUM,'(ES40.5E2)') DVAL
       ELSEIF(DVAL>0D0 .OR. DVAL<0D0)              THEN
          WRITE(NUM,'(ES40.5E3)') DVAL
       END IF
       !
      ELSE
          WRITE(NUM,'(1PG15.6)') DVAL 
      END IF
      !
      DBLE2STR=TRIM(ADJUSTL(NUM))
      !
      END FUNCTION
      END MODULE