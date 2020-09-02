﻿! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
!
! MODULE LISTING:
! 
!   BAS_UTIL
! 
!   BUDGET_DATEBASE_WRITER
! 
!   OWHM_HEADER_INTERFACE
! 
! 
MODULE BAS_UTIL
  !
  !  GET_LOWEST_LAYER(IR,IC,IBOUND) RESULT(IL)
  !  MONTHPRINT(DECYEAR) RESULT(MONTH)
  !  CVRT2DYEAR(ITMUNI,YEARTYPE)
  !  CHECK_FOR_VALID_DIMENSIONS(DIMTOL,IOUT,NROW,NCOL,NLAY,IBOUND,LBOTM,BOTM,DELR,DELC)
  !  DECIMAL_YEAR(DYEAR,DELT,ITMUNI,USE_LEAP)
  !  DELT_TO_DAY(DELT,ITMUNI) RESULT(DAY)      
  !  GET_NAME_AND_LGR_CHECK(ILGR,NGRIDS,FNAME)
  !  OPEN_NAME_FILE(NAM_FILE,INAM,INFILE)
  !
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, STDIN=>INPUT_UNIT, STDOUT=>OUTPUT_UNIT
  !
  USE GENERIC_OPEN_INTERFACE, ONLY: GENERIC_OPEN
  USE UTIL_INTERFACE,         ONLY: STOP_ERROR, WARNING_MESSAGE, FILE_IO_ERROR, READ_TO_DATA, UPPER, PARSE_WORD, PARSE_WORD_UP, GET_INTEGER
  USE NUM2STR_INTERFACE,      ONLY: NUM2STR
  USE CONSTANTS,              ONLY: BLNK,NL,BLN,TAB,COM,NEG,Z,ONE,TWO,THREE,TEN,DZ,UNO,DOS,DIEZ,TRUE,FALSE
  USE CALENDAR_FUNCTIONS,     ONLY: ISLEAPYEAR
  USE ARRAY_DATA_TYPES,       ONLY: CHARACTER_TYPE
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: OPEN_NAME_FILE, GET_NAME_AND_LGR_CHECK, GETNAMFILLGR,               &
           GET_LOWEST_LAYER, RELAX_HNEW, TOP_LIM_HNEW, PRINT_TOP_LIM_HNEW,     &
           MONTHPRINT, CVRT2DYEAR, DELT_TO_DAY, DECIMAL_YEAR,                  &
           MASS_ERROR_COUNT_PRINT, MASS_ERROR_PRINT, CHECK_FOR_VALID_DIMENSIONS
  !
  CONTAINS
  !
  PURE FUNCTION GET_LOWEST_LAYER(IR,IC,IBOUND) RESULT(IL)
    INTEGER,                              INTENT(IN):: IR,IC
    INTEGER, DIMENSION(:,:,:),CONTIGUOUS, INTENT(IN):: IBOUND
    INTEGER:: IL
    INTEGER:: I
    !
    IL = Z
    DO I = SIZE(IBOUND,THREE), ONE, NEG 
        IF(IBOUND(IC,IR,I).NE.Z) THEN
            IL = I
            EXIT
        END IF
    END DO
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  !!!PURE FUNCTION ELEV_TO_LAYER(IR,IC,IBOUND) RESULT(IL)
  !!!  INTEGER,                              INTENT(IN):: IR,IC
  !!!  INTEGER, DIMENSION(:,:,:),CONTIGUOUS, INTENT(IN):: IBOUND
  !!!  INTEGER:: IL
  !!!  INTEGER:: I
  !!!  !
  !!!  IL = Z
  !!!  DO I = SIZE(IBOUND,THREE), ONE, NEG 
  !!!      IF(IBOUND(IC,IR,I).NE.Z) THEN
  !!!          IL = I
  !!!          EXIT
  !!!      END IF
  !!!  END DO
  !!!  !
  !!!END FUNCTION
  !
  !#########################################################################################################################
  !
  SUBROUTINE GET_NAME_AND_LGR_CHECK(ILGR,NGRIDS,FNAME)
    INTEGER,      INTENT(INOUT):: ILGR, NGRIDS
    CHARACTER(*), INTENT(INOUT):: FNAME
    !
    INTEGER:: IU                        !Temp unit to check if input is LGR Control or Name File
    INTEGER:: LLOC
    INTEGER:: ISTOP, ISTART
    CHARACTER(200):: LINE  !ONLY USED TO READ IN 'LGR' and 'NGRID'
    !
    FNAME=BLNK
    LINE =BLNK
    IF (COMMAND_ARGUMENT_COUNT()>Z) THEN
        CALL GET_COMMAND_ARGUMENT(ONE, FNAME)
    ELSE
        WRITE (STDOUT,'(A)', ADVANCE='NO') 'Enter the name of the NAME file or LGR Control file:  '
        READ  (STDIN, '(A)') FNAME
    END IF
    !
    CALL OPEN_NAME_FILE(FNAME,IU,STDIN)   !STDIN = 6 cause its the cmd prompt
    !
    CALL READ_TO_DATA(LINE,IU)
    IF(LINE==BLNK) CALL STOP_ERROR(INFILE=IU,MSG='FILE APPEARS TO BE EMPTY')
    !
    LLOC=ONE
    CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
    !
    IF(LINE(ISTART:ISTOP) .EQ. 'LGR') THEN
      ILGR = ONE
      WRITE(*,'(/A,/A/)') ' FOUND LGR CONTROL FILE',' RUNNING MODFLOW-OWHM WITH LGR'
      !
      CALL READ_TO_DATA(LINE,IU)
      !
      LLOC=ONE
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,STDOUT,IU,NGRIDS,MSG='FOUND LGR ON FIRST LINE OF NAME FILE, SO ASSUMMED IT IS AN LGR CONTROL FILE, BUT FAILED TO LOAD "NGRIDS" ON THE SECOND LINE OF FILE.')
      WRITE(*,'(1x 2A/)') 'LGR NGRIDS = ', NUM2STR(NGRIDS)
    ENDIF
    !
    CLOSE(IU)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  !-----VERSION 1.0 15FEBRUARY2006 GETNAMFILLGR
  !
  SUBROUTINE GETNAMFILLGR(INLGR,FNAME,IGRID)   ! INLGR = LGR unit number 
    ! ******************************************************************
    ! READ NAMES OF THE CORRESPONDING NAME FILES FROM LGR CONTROL FILE
    ! ******************************************************************
    !
    !    SPECIFICATIONS:
    ! ------------------------------------------------------------------
    INTEGER,      INTENT(INOUT):: INLGR
    CHARACTER(*), INTENT(INOUT):: FNAME
    INTEGER,      INTENT(IN   ):: IGRID
    !
    INTEGER:: LLOC, ISTART, ISTOP
    CHARACTER(700):: LINE
    !     ------------------------------------------------------------------
    !1-----READ IN THE NAME OF THE NAME FILE FOR THIS GRID
    IF ( IGRID==1) THEN                              !OPEN THE LGR CONTROL FILE
       CALL OPEN_NAME_FILE(FNAME,INLGR,0)
       CALL READ_TO_DATA(LINE,INLGR,0)  !LGR FLAG
       CALL READ_TO_DATA(LINE,INLGR,0)  !NGRID SPEC
    END IF
    !      
    CALL READ_TO_DATA(LINE,INLGR,0)     !NAME FILE NAME
    !
    LLOC=ONE
    CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
    !
    FNAME=LINE(ISTART:ISTOP)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE OPEN_NAME_FILE(NAM_FILE,INAM,INFILE)
    CHARACTER(*), INTENT(IN   ):: NAM_FILE  ! File location to open
    INTEGER,      INTENT(  OUT):: INAM      ! Name File Unit
    INTEGER,      INTENT(IN   ):: INFILE    ! File unit of file that read NAM_FILE 
    !
    CHARACTER(LEN_TRIM(NAM_FILE)+6):: FNAME  !ADDED SPACE IS NEEDED FOR POTENTIAL OF ADDING .nam and 1 space for URWORD
    INTEGER:: ISTOP, ISTART, LLOC
    CHARACTER(4):: NAM_CHECK
    LOGICAL:: EXIST
    !
    FNAME=TRIM(NAM_FILE)
    LLOC=ONE
    CALL PARSE_WORD(FNAME,LLOC,ISTART,ISTOP,EOL=EXIST)
    !
    IF(FNAME(ISTART:ISTOP)==BLNK .OR. EXIST) CALL STOP_ERROR(LINE=NAM_FILE,INFILE=INFILE, MSG='ERROR OPENING NAME FILE OR LGR FILE THAT WAS SPECIFIED IN THE COMMAND PROMPT.'//NL//'NAME APPEARS TO BE BLANK, PLEASE SPECIFY THE NAME FILE AFTER THE THE NAME OF OneWater EXECUTABLE AT THE COMMAND PROMPT (e.g "OneWater.exe MyName.nam")')
    !    
    INQUIRE(FILE=FNAME(ISTART:ISTOP), EXIST=EXIST)
    ! 'FILE.nam'
    !
    IF (.NOT. EXIST) THEN        !Check if .nam needs to be appended
       IF(LEN_TRIM(FNAME)>3) THEN
                             NAM_CHECK = FNAME(ISTOP-3:ISTOP)
                             CALL UPPER(NAM_CHECK)
                             !
                             IF (NAM_CHECK.NE.'.NAM') THEN
                                 FNAME(ISTOP+1:ISTOP+4) = '.nam'
                                 ISTOP=ISTOP+4
                             END IF
       END IF
    END IF
    !
    INAM = Z
    CALL GENERIC_OPEN(FNAME(ISTART:ISTOP), INAM, ACTION='READ', STATUS='OLD', BUFFER_BLOCKSIZE=65536, BUFFER_COUNT=ONE,ERROR=EXIST)
    IF(EXIST) CALL STOP_ERROR(LINE=NAM_FILE,INFILE=INFILE, MSG='ERROR OPENING NAME FILE OR LGR FILE THAT WAS SPECIFIED IN THE COMMAND PROMPT.'//NL//'ATTEMPED TO OPEN THE FOLLOWING FILE (NOTE ".nam" IS ADDED TO CHECK IF EXTENSION WAS NOT INCLDUED):'//NL//'"'//FNAME(ISTART:ISTOP)//'"'//NL//'PLEASE CHECK SPELLING OR IF LOCATION OF NAME/LGR FILE IS CORRECT.')
    !OPEN(NEWUNIT=INAM, FILE=FNAME(ISTART:ISTOP), STATUS='OLD', ACTION='READ', POSITION='REWIND',IOSTAT=IERR)
    !IF(IERR.NE.Z) CALL FILE_IO_ERROR(IERR,FNAME=TRIM(NAM_FILE),MSG='ERROR OPENING NAME FILE OR LGR FILE THAT WAS SPECIFIED ON COMMAND PROMPT.'//NL//'PLEASE CHECK SPELLING OR IF LOCATION OF NAME/LGR FILE IS CORRECT.',INFILE=INFILE)
    !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE FUNCTION MONTHPRINT(DECYEAR) RESULT(MONTH) !CONSIDER MOVING TO MODULE AND USING ALLOCATABLE CHARACTER(:),ALLOCATABLE::MONTH
    CHARACTER(9)::MONTH
    DOUBLE PRECISION,INTENT(IN)::DECYEAR
    DOUBLE PRECISION:: FRAC
    INTEGER::YEAR
    LOGICAL::LEAPYR
    DOUBLE PRECISION:: B
    
    YEAR=INT(DECYEAR)
    FRAC=DECYEAR - DBLE(YEAR)
    LEAPYR=FALSE
    B=0.00068  !yr = ~6 hours  --Correction factor, if new month is six hours away, print out that one.
    
    IF( (MOD(YEAR,4).EQ.Z .AND. MOD(YEAR,100).NE.Z)  .OR.  MOD(YEAR,400).EQ.Z ) LEAPYR=TRUE  !LEAP YEAR IF YEAR IS DIVISABLE BY 4 AND NOT 100 OR IF YEAR IS DIVISABLE BY 400
       
!ASSUMES 28 days in feb      
    IF(LEAPYR)THEN
                  IF(FRAC < 0.084699454D0 - B)THEN
                      MONTH='January'
              ELSEIF(FRAC < 0.163934426D0 - B)THEN
                      MONTH='February'
              ELSEIF(FRAC < 0.248633880D0 - B)THEN
                      MONTH='March'
              ELSEIF(FRAC < 0.330601093D0 - B)THEN
                      MONTH='April'
              ELSEIF(FRAC < 0.415300546D0 - B)THEN
                      MONTH='May'
              ELSEIF(FRAC < 0.497267760D0 - B)THEN
                      MONTH='June'
              ELSEIF(FRAC < 0.581967213D0 - B)THEN
                      MONTH='July'
              ELSEIF(FRAC < 0.666666667D0 - B)THEN
                      MONTH='August'
              ELSEIF(FRAC < 0.748633880D0 - B)THEN
                      MONTH='September'
              ELSEIF(FRAC < 0.833333333D0 - B)THEN
                      MONTH='October'
              ELSEIF(FRAC < 0.915300546D0 - B)THEN
                      MONTH='November'
              ELSE
                      MONTH='December'
              END IF
    ELSE
                  IF(FRAC < 0.084931507D0)THEN
                      MONTH='January'
              ELSEIF(FRAC < 0.161643836D0)THEN
                      MONTH='February'
              ELSEIF(FRAC < 0.246575342D0)THEN
                      MONTH='March'
              ELSEIF(FRAC < 0.328767123D0)THEN
                      MONTH='April'
              ELSEIF(FRAC < 0.413698630D0)THEN
                      MONTH='May'
              ELSEIF(FRAC < 0.495890411D0)THEN
                      MONTH='June'
              ELSEIF(FRAC < 0.580821918D0)THEN
                      MONTH='July'
              ELSEIF(FRAC < 0.665753425D0)THEN
                      MONTH='August'
              ELSEIF(FRAC < 0.747945205D0)THEN
                      MONTH='September'
              ELSEIF(FRAC < 0.832876712D0)THEN
                      MONTH='October'
              ELSEIF(FRAC < 0.915068493D0)THEN
                      MONTH='November'
              ELSE
                      MONTH='December'
              END IF
          !
    END IF
  END FUNCTION
  !
  !#########################################################################################################################
  !
  SUBROUTINE CHECK_FOR_VALID_DIMENSIONS(DIMTOL,IN, IOUT,NROW,NCOL,NLAY,IBOUND,LBOTM,BOTM,DELR,DELC,WARN)
    REAL,                                INTENT(IN):: DIMTOL!=1E-5
    INTEGER,                             INTENT(IN):: IN, IOUT
    INTEGER,                             INTENT(IN):: NROW, NCOL, NLAY
    INTEGER, DIMENSION(:,:,:),CONTIGUOUS,INTENT(IN):: IBOUND
    INTEGER, DIMENSION(:),    CONTIGUOUS,INTENT(IN):: LBOTM
    REAL, DIMENSION(:,:,0:),  CONTIGUOUS,INTENT(IN):: BOTM
    REAL, DIMENSION(:),       CONTIGUOUS,INTENT(IN):: DELR, DELC
    LOGICAL,                             INTENT(IN):: WARN
    !
    REAL::MAXR,MAXC,MINR,MINC,MINTHCK
    REAL::THK
    INTEGER::IR,IC,IL
    LOGICAL::KILLPROG
    CHARACTER(8):: L, R, C
    CHARACTER(16):: VAL
    CHARACTER(:),ALLOCATABLE:: BAD_DELR, BAD_DELC, BAD_THCK, HED
    !
    HED=NL
    BAD_DELR = NL
    BAD_DELC = NL
    BAD_THCK = NL
    KILLPROG=.NOT. WARN
    !
    MAXR=MAXVAL(DELR)
    MAXC=MAXVAL(DELC)
    !
    !MAXTHCK=MAXVAL( BOTM(:,:,LBOTM(1:NLAY)-1) - BOTM(:,:,LBOTM(1:NLAY)), MASK=IBOUND(:,:,:).NE.Z )
    !
    MINR=DIMTOL*MAXR
    MINC=DIMTOL*MAXC
    !MINTHCK=DIMTOL*MAXTHCK
    !
    IF(WARN) THEN
        !
        DO IR=ONE, NROW
            IF(ALL(IBOUND(:,IR,:).EQ.Z)) CYCLE
            IF(DELC(IR) < MINC) THEN
                VAL = NUM2STR(DELC(IR))
                R = NUM2STR(IR)
                BAD_DELC=BAD_DELC//VAL//R//NL
            END IF
            IF(DELC(IR) .LE. 0.0) KILLPROG=TRUE 
        END DO
        !
        IF (BAD_DELC .NE. NL) THEN
            HED = HED//'A DELC HAS A LENGTH 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELC'//NL
            BAD_DELC = NL//'A DELC HAS A LENGTH 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELC'//NL//'THE FOLLOWING ARE THE ROWS WITH THIS DELC'//BLN//'DELC(ROW)       ROW'//BAD_DELC
        END IF
        !
        DO IC=ONE,NCOL
            IF(ALL(IBOUND(IC,:,:).EQ.Z)) CYCLE
            IF(DELR(IC) < MINR) THEN
                VAL = NUM2STR(DELR(IC))
                C = NUM2STR(IC)
                BAD_DELR=BAD_DELR//VAL//C//NL
             END IF
             IF(DELR(IC) .LE. 0.0) KILLPROG=TRUE 
             !
        END DO
        !
        IF (BAD_DELR .NE. NL) THEN
            HED = HED//'A DELR HAS A LENGTH 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELR'//NL
            BAD_DELR = NL//'A DELR HAS A LENGTH 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELR'//NL//'THE FOLLOWING ARE THE ROWS WITH THIS DELR'//BLN//'DELR(COL)       COL'//BAD_DELR
        END IF
        !
        DO IL = ONE, NLAY 
        MINTHCK=DIMTOL*MAXVAL( BOTM(:,:,LBOTM(IL)-1) - BOTM(:,:,LBOTM(IL)), MASK=IBOUND(:,:,IL).NE.Z )
        DO IR = ONE, NROW
        DO IC = ONE, NCOL
         IF ( IBOUND(IC,IR,IL).NE.Z ) THEN
            THK=BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
            IF(THK < MINTHCK) THEN
                VAL = NUM2STR(THK)
                L = NUM2STR(IL)
                R = NUM2STR(IR)
                C = NUM2STR(IC)
                BAD_THCK=BAD_THCK//VAL//L//R//C//NL
             END IF
             IF(THK .LE. 0.0) KILLPROG=TRUE
         END IF
        END DO
        END DO
        END DO
        !
        IF (BAD_THCK .NE. NL) THEN
            HED = HED//'THERE ARE CELLS WITHIN A MODEL LAYER THAT ARE'//NL//'5 ORDERS OF MAGNIDITE SMALLER THICKNESS (BOTM_TOP - BOTM_BOTTOM) THAN THE THICKEST MODEL CELL OF THAT LAYER.'//NL
            BAD_THCK = NL//'THERE ARE CELLS WITHIN A MODEL LAYER THAT ARE'//NL//'5 ORDERS OF MAGNIDITE SMALLER THICKNESS (BOTM_TOP - BOTM_BOTTOM) THAN THE THICKEST MODEL CELL OF THAT LAYER.'//NL//'THE FOLLOWING ARE THE CELLS WITH THIS SMALL THICKNESS'//BLN//'THICK(R,C,L)    ROW     COL     LAY'//BAD_THCK
        END IF
        !
        IF(.NOT. KILLPROG .AND. WARN .AND. HED.NE.NL) THEN
            CALL WARNING_MESSAGE(INFILE=IN,OUTPUT=IOUT,MSG=HED//BAD_DELC//BAD_DELR//BAD_THCK//BLN//HED)
        END IF
    END IF
    !
    IF(KILLPROG)THEN
        !
        HED=NL
        BAD_DELR = NL
        BAD_DELC = NL
        BAD_THCK = NL
        !
        DO IR=ONE, NROW
            IF(ALL(IBOUND(:,IR,:).EQ.Z)) CYCLE
            IF(DELC(IR) .LE. 0.0) THEN
                VAL = NUM2STR(DELC(IR))
                R = NUM2STR(IR)
                BAD_DELC=BAD_DELC//VAL//R//NL
            END IF
        END DO
        !
        IF (BAD_DELC .NE. NL) THEN
            HED = HED//'A DELC HAS A LENGTH LESS THAN ZERO'//NL
            BAD_DELC = NL//'A DELC HAS A LENGTH LESS THAN ZERO'//NL//'THE FOLLOWING ARE THE ROWS WITH THIS DELC'//BLN//'DELC(ROW)       ROW'//BAD_DELC
        END IF
        !
        DO IC=ONE,NCOL
            IF(ALL(IBOUND(IC,:,:).EQ.Z)) CYCLE
            IF(DELR(IC) .LE. 0.0) THEN
                VAL = NUM2STR(DELR(IC))
                C = NUM2STR(IC)
                BAD_DELR=BAD_DELR//VAL//C//NL
             END IF
             !
        END DO
        !
        IF (BAD_DELR .NE. NL) THEN
            HED = HED//'A DELR HAS A LENGTH LESS THAN ZERO'//NL
            BAD_DELR = NL//'A DELR HAS A LENGTH LESS THAN ZERO'//NL//'THE FOLLOWING ARE THE ROWS WITH THIS DELR'//BLN//'DELR(COL)       COL'//BAD_DELR
        END IF
        !
        DO IL = ONE, NLAY                                                   !seb FIX SEARCH TO NOT NEGATE  CELLS
        DO IR = ONE, NROW
        DO IC = ONE, NCOL
         IF ( IBOUND(IC,IR,IL).NE.Z ) THEN
             THK=BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
            IF(THK .LE. 0.0) THEN
                VAL = NUM2STR(THK)
                L = NUM2STR(IL)
                R = NUM2STR(IR)
                C = NUM2STR(IC)
                BAD_THCK=BAD_THCK//VAL//R//C//L//NL
             END IF
         END IF
        END DO
        END DO
        END DO
        !
        IF (BAD_THCK .NE. NL) THEN
            HED = HED//'THERE ARE CELLS THAT HAVE A NEGATIVE THICKNESS (BOTM_TOP < BOTM_BOTTOM).'//NL
            BAD_THCK = NL//'THERE ARE CELLS THAT HAVE A NEGATIVE THICKNESS (BOTM_TOP < BOTM_BOTTOM).'//NL//'THE FOLLOWING ARE THE CELLS WITH THIS NEGATIVE THICKNESS'//BLN//'THICK(R,C,L)    ROW     COL     LAY'//BAD_THCK
        END IF
        !
        IF(HED.NE.NL) THEN
            HED=HED//NL//'     MODEL DIMENSION FAILURE'//BLN
            IF(BAD_DELC.NE.NL) HED=HED//BAD_DELC
            IF(BAD_DELR.NE.NL) HED=HED//BAD_DELR
            IF(BAD_THCK.NE.NL) HED=HED//BAD_THCK
            CALL STOP_ERROR(INFILE=IN,OUTPUT=IOUT, MSG=HED)
        END IF
    END IF
    !
  END SUBROUTINE
  !SUBROUTINE CHECK_FOR_VALID_DIMENSIONS(DIMTOL,IOUT,NROW,NCOL,NLAY,IBOUND,LBOTM,BOTM,DELR,DELC)
  !  REAL,                                INTENT(IN):: DIMTOL!=1E-5
  !  INTEGER,                             INTENT(IN):: IOUT
  !  INTEGER,                             INTENT(IN):: NROW, NCOL, NLAY
  !  INTEGER, DIMENSION(:,:,:),CONTIGUOUS,INTENT(IN):: IBOUND
  !  INTEGER, DIMENSION(:),    CONTIGUOUS,INTENT(IN):: LBOTM
  !  REAL, DIMENSION(:,:,0:),  CONTIGUOUS,INTENT(IN):: BOTM
  !  REAL, DIMENSION(:),       CONTIGUOUS,INTENT(IN):: DELR, DELC
  !  !
  !  REAL::MAXR,MAXC,MAXTHCK,MINR,MINC,MINTHCK
  !  REAL::THK
  !  INTEGER::IR,IC,IL
  !  LOGICAL::KILLPROG,PRINTHEAD
  !  CHARACTER(1)  :: NL
  !  CHARACTER(422):: HEADER_WARN
  !  CHARACTER(424):: HEADER_FAIL
  !  !
  !  NL=ACHAR(10) !CARRAGE RETURN
  !  HEADER_WARN = REPEAT('#',35)//' MODEL DIMENSION WARNING ' // REPEAT('#',80) //NL// REPEAT('#',140) //NL// REPEAT('#',140)
  !  HEADER_FAIL = REPEAT('#',35)//' MODEL DIMENSION FAILURE ' // REPEAT('#',80) //NL// REPEAT('#',140) //NL// REPEAT('#',140)
  !  !
  !  KILLPROG=FALSE
  !  !
  !  WRITE(IOUT,'(/ A /)') 'CHECKING FOR VALID MODEL DIMENIONS FOR DELR, DELC, AND THICKNESS'
  !  !
  !  MAXR=MAXVAL(DELR)
  !  MAXC=MAXVAL(DELC)
  !  !
  !  MAXTHCK=MAXVAL( BOTM(:,:,LBOTM(1:NLAY)-1) - BOTM(:,:,LBOTM(1:NLAY)), MASK=IBOUND(:,:,:).NE.Z )
  !  !
  !  MINR=DIMTOL*MAXR
  !  MINC=DIMTOL*MAXC
  !  MINTHCK=DIMTOL*MAXTHCK
  !  PRINTHEAD=TRUE
  !  DO IR=ONE, NROW
  !      IF(ALL(IBOUND(:,IR,:).EQ.Z)) CYCLE
  !      IF(DELC(IR) < MINC) THEN
  !          IF(PRINTHEAD) THEN
  !           WRITE(IOUT,'(A)')HEADER_WARN
  !           PRINTHEAD=FALSE
  !          END IF
  !          WRITE(IOUT,'(/ 5A)') 'WARNING: EXTREMELY SMALL DELC WITH LENGTH OF ', NUM2STR(DELC(IR)),', LOCATED AT ROW: ',NUM2STR(IR), '  (THIS IS 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELC)'
  !      END IF
  !      IF(DELC(IR) .LE. 0.0) KILLPROG=TRUE 
  !  END DO
  !  DO IC=ONE,NCOL
  !      IF(ALL(IBOUND(IC,:,:).EQ.Z)) CYCLE
  !      IF(DELR(IC) < MINR) THEN
  !          IF(PRINTHEAD) THEN
  !           WRITE(IOUT,'(A)')HEADER_WARN
  !           PRINTHEAD=FALSE
  !          END IF
  !          WRITE(IOUT,'(/ 5A)') 'WARNING: EXTREMELY SMALL DELR WIDTH LENGTH OF ', NUM2STR(DELR(IC)),', LOCATED AT COL: ',NUM2STR(IC), '  (THIS IS 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST DELR)'
  !       END IF
  !       IF(DELR(IC) .LE. 0.0) KILLPROG=TRUE 
  !       !
  !  END DO
  !  !
  !  DO IL = ONE, NLAY                                                   !seb FIX SEARCH TO NOT NEGATE  CELLS
  !  DO IR = ONE, NROW
  !  DO IC = ONE, NCOL
  !   IF ( IBOUND(IC,IR,IL).NE.Z ) THEN
  !       THK=BOTM(IC,IR,LBOTM(IL)-1) - BOTM(IC,IR,LBOTM(IL))
  !      IF(THK < MINTHCK) THEN
  !          IF(PRINTHEAD) THEN
  !           WRITE(IOUT,'(A)') HEADER_WARN
  !           PRINTHEAD=FALSE
  !          END IF
  !          WRITE(IOUT,'(/ *(A))') 'WARNING: EXTREMELY SMALL CELL THICKNESS (BOTM_TOP - BOTM_BOTTOM) LOCATED AT ROW, COL, LAY: ', NUM2STR(IR),', ',NUM2STR(IC),', ',NUM2STR(IL),' WITH A CELL THICKNESS OF: ', NUM2STR(THK), '  (THIS IS 5 ORDERS OF MAGNITUDE SMALLER THAN THE LARGEST CELL THICKNESS)'
  !       END IF
  !       IF(THK .LE. 0.0) KILLPROG=TRUE
  !   END IF
  !  END DO
  !  END DO
  !  END DO
  !  !
  !  IF(KILLPROG)THEN
  !      WRITE(IOUT,'(A)')HEADER_FAIL
  !      WRITE(IOUT,'(/ 13X A /)')'ONE OR MORE MODEL CELLS HAVE A WIDTH OR LENGTH OR THICKNESS = 0 OR < 0'
  !      CALL STOP_ERROR(OUTPUT=IOUT, MSG='MODEL DIMENSION FAILURE')
  !  END IF
  !  !
  !END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE FUNCTION DELT_TO_DAY(DELT,ITMUNI) RESULT(DAY)
  CLASS(*), INTENT(IN)::DELT
  INTEGER, INTENT(IN):: ITMUNI
  DOUBLE PRECISION:: DAY
  !
  SELECT TYPE(DELT)
  TYPE IS(REAL(REAL64))
                       SELECT CASE (ITMUNI)
                         CASE (0, 4)                             !DAYS
                             DAY = DELT
                         CASE (1)                                !SECONDS
                             DAY = DELT * 1.15740740740741D-05
                         CASE (2)                                !MINUTES
                             DAY = DELT * 6.94444444444444D-04
                         CASE (3)                                !HOURS
                             DAY = DELT / 24D0
                         CASE (5)                                !YEARS  --NO WAY TO CORRECT FOR LEAP YEARS FOR THIS CASE
                             DAY = DELT * 365.25D0                                  
                       END SELECT
  TYPE IS(REAL(REAL32))
                       SELECT CASE (ITMUNI)
                         CASE (0, 4)                             !DAYS
                             DAY = DELT                          
                         CASE (1)                                !SECONDS
                             DAY = DELT * 1.15740740740741E-05   
                         CASE (2)                                !MINUTES
                             DAY = DELT * 6.94444444444444E-04   
                         CASE (3)                                !HOURS
                             DAY = DELT / 24E0                   
                         CASE (5)                                !YEARS  --NO WAY TO CORRECT FOR LEAP YEARS FOR THIS CASE
                             DAY = DELT * 365.25                                    
                       END SELECT
  END SELECT
  !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  PURE SUBROUTINE DECIMAL_YEAR(DYEAR,DELT,ITMUNI,USE_LEAP)
  !SUBROUTINES THAT UPDATES DYEAR WITH THE APPROPIATE DECIMAL YEAR OF DELT
    DOUBLE PRECISION,INTENT(INOUT)::DYEAR
    DOUBLE PRECISION,INTENT(IN)::DELT
    INTEGER,         INTENT(IN)::ITMUNI
    LOGICAL,         INTENT(IN)::USE_LEAP
    !
    INTEGER:: YR_NEW,YR_OLD,YEAR_TRANSITION
    DOUBLE PRECISION:: DYR_NEW, DYR_OLD, DTY,DELTP
    DOUBLE PRECISION:: DBLE_YR_NEW, DBLE_YR_OLD
    LOGICAL:: YR_NEW_LEAP,YR_OLD_LEAP
    !
!    INTERFACE
!      PURE FUNCTION ISLEAPYEAR(YEAR)
!         INTEGER,INTENT(IN):: YEAR
!         LOGICAL::ISLEAPYEAR
!      END FUNCTION
!      !
!      PURE FUNCTION CVRT2DYEAR(ITMUNI,YEARTYPE)
!         INTEGER,INTENT(IN)::ITMUNI,YEARTYPE
!         DOUBLE PRECISION::CVRT2DYEAR
!      END FUNCTION
!    END INTERFACE
    !
    ! YEAR_TRANSITION = 1 NO  LEAP AND NO TRANSITION TO NEW YEAR
    !                   2 YES LEAP AND NO TRANSITION TO NEW YEAR 
    !                   3 TRANSITION FROM NO LEAP TO LEAP YEAR
    !                   4 TRANSITION FROM LEAP YEAR TO NO LEAP
    !IF TIME STEP IS IN YEARS THEN LEAP YEAR DOES NOT MATTER AND RETURN
    DYR_OLD=DYEAR
    !
    IF(ITMUNI.EQ.5) THEN
        DYEAR=DYEAR+DELT
        RETURN
    END IF
    !
    !USE DECIMAL YEAR OF 365.24
    DYR_NEW = DYEAR + DELT * CVRT2DYEAR(ITMUNI,Z)
    !
    !IF NOT REQUESTION A LEAP YEAR CORRECTION RETURN
    IF(.NOT. USE_LEAP) THEN
      DYEAR=DYR_NEW
      RETURN
    END IF
    !
    !DETERMINE IF THERE IS A CHANGE IN THE YEAR NUMBER  !ALGORITHM BREAKS DOWN IF THERE IS MORE THAN 1 YEAR CHANGE AND USING A NON-YEAR TIMESTEAP AND LEAPYEAR SEARCH. THAT IS SOMETHING LIKE HAVING A TIME STEP IN DAYS EQUAL TO 750 DAYS
    YR_OLD=INT(DYR_OLD)
    YR_NEW=INT(DYR_NEW)
    DBLE_YR_NEW=DBLE(YR_NEW)
    DBLE_YR_OLD=DBLE(YR_OLD)
    !
    YR_NEW_LEAP=ISLEAPYEAR(YR_NEW)
    YR_OLD_LEAP=ISLEAPYEAR(YR_OLD)
    !
    !FIRST CHECK IF TIME STEP KEEPS WITHIN SAME YEAR
    !TRANSITION REFERS TO A CHANGE IN YEAR THAT RESULTS FROM GOING FROM A NONLEAP YEAR TO A LEAP YEAR OR VICE-VERSA
    YEAR_TRANSITION=-2                                                !IF THERE IS AN UNKNOWN ERROR THEN DYR_NEW = DYEAR + DELT / 365.2425 IS RETURNED
    !
    IF    (.NOT. YR_NEW_LEAP .AND. .NOT.YR_OLD_LEAP)              THEN
                                                   YEAR_TRANSITION=1  !NO TRANSION, BUT NOT IN A LEAP YEAR
    ELSEIF(      YR_NEW_LEAP .AND.      YR_OLD_LEAP)              THEN
                                                   YEAR_TRANSITION=2  !NO TRANSION, BUT     IN A LEAP YEAR
    !
    !NOW IF YEAR CHANGE AND TRANSION FROM LEAP YEAR TO NON LEAN YEAR OR VICE-VERSA
    ELSEIF(DELT > 0D0)                                            THEN!   TRANSITION BETWEEN YEARS IN FORWARD DIRECTION (eg 1932 to 1933 or 1931 to 1932)
      IF(      YR_NEW_LEAP .AND. .NOT.YR_OLD_LEAP) YEAR_TRANSITION=3  !TRANSITION FROM NOLEAP TO LEAP YEAR
      IF(.NOT. YR_NEW_LEAP .AND.      YR_OLD_LEAP) YEAR_TRANSITION=4  !TRANSITION FROM LEAP TO NOLEAP YEAR
      !
    ELSE                                                              !   TRANSITION BETWEEN YEARS IN BACKWARD DIRECTION  (ie DELT<0 and eg 1933 to 1932 or 1932 to 1931)
      IF(      YR_OLD_LEAP .AND. .NOT.YR_NEW_LEAP) YEAR_TRANSITION=5  !TRANSITION FROM LEAP TO NOLEAP YEAR (BECAUSE YOUR GOING BACK IN TIME)
      IF(.NOT. YR_OLD_LEAP .AND.      YR_NEW_LEAP) YEAR_TRANSITION=6  !TRANSITION FROM NOLEAP TO LEAP YEAR (BECAUSE YOUR GOING BACK IN TIME)
    END IF
    !
    SELECT CASE (YEAR_TRANSITION)
       CASE (1) !NO TRANSITION NO LEAP YEAR
          DYR_NEW = DYR_OLD + DELT * CVRT2DYEAR(ITMUNI,1)
          !
       CASE (2) !NO TRANSITION YES LEAP YEAR
          DYR_NEW = DYR_OLD + DELT * CVRT2DYEAR(ITMUNI,2)
          !
       CASE (3)
          DTY=DBLE_YR_NEW - DYR_OLD            !FRACTION OF YEAR REMAINING DURING NONLEAP YEAR
          DELTP=DTY/CVRT2DYEAR(ITMUNI,1)       !NUMBER OF ITMUNI TIME UNITS IN A NONLEAP YEAR REQUIRED TO FINISH OFF THE YEAR
          !
          DYR_NEW = DBLE_YR_NEW + (DELT - DELTP)*CVRT2DYEAR(ITMUNI,2)
          !
       CASE (4)
          DTY=DBLE_YR_NEW - DYR_OLD            !FRACTION OF YEAR REMAINING DURING LEAP YEAR
          DELTP=DTY/CVRT2DYEAR(ITMUNI,2)       !NUMBER OF ITMUNI TIME UNITS IN A LEAP YEAR REQUIRED TO FINISH OFF THE YEAR
          !
          DYR_NEW = DBLE_YR_NEW + (DELT - DELTP)*CVRT2DYEAR(ITMUNI,1)
          !
       CASE (5)                                !NOTE DYR_OLD IS LARGER THAN DYR_NEW BECAUSE DELT<0
          DTY=DYR_OLD - DBLE_YR_OLD            !FRACTION OF YEAR REMAINING DURING LEAP YEAR GOING BACKWARD IN TIME
          DELTP=DTY/CVRT2DYEAR(ITMUNI,2)       !NUMBER OF ITMUNI TIME UNITS IN A LEAP YEAR REQUIRED TO GO BACK IN TIME TO START OF YEAR
          !
          DYR_NEW = DBLE_YR_OLD + (DELT + DELTP)*CVRT2DYEAR(ITMUNI,1) !NOTE DELT<0 AND (DELT + DELTP) IS IN A NOLEAP YEAR
          !
       CASE (6)                                !NOTE DYR_OLD IS LARGER THAN DYR_NEW BECAUSE DELT<0
          DTY=DYR_OLD - DBLE_YR_OLD            !FRACTION OF YEAR REMAINING DURING LEAP YEAR GOING BACKWARD IN TIME
          DELTP=DTY/CVRT2DYEAR(ITMUNI,1)       !NUMBER OF ITMUNI TIME UNITS IN A NOLEAP YEAR REQUIRED TO GO BACK IN TIME TO START OF YEAR
          !
          DYR_NEW = DBLE_YR_OLD + (DELT + DELTP)*CVRT2DYEAR(ITMUNI,2) !NOTE DELT<0 AND (DELT + DELTP) IS IN A LEAP YEAR
          !
    END SELECT
    
    DYEAR=DYR_NEW
    
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  PURE FUNCTION CVRT2DYEAR(ITMUNI,YEARTYPE)
    INTEGER,INTENT(IN)::ITMUNI,YEARTYPE
    DOUBLE PRECISION::CVRT2DYEAR
    !
    ! YEARTYPE =  0  365.2425 days to a year 
    !             1  365     days to a year (NONLEAP YEAR)
    !             2  366     days to a year (   LEAP YEAR)
    !
    IF    (YEARTYPE .EQ. 0) THEN  ! 365.2425 days to year
       !
       SELECT CASE (ITMUNI)
         CASE (0, 4)                                                  !DAYS
             CVRT2DYEAR = 2.737907006989D-03
         CASE (1)                                                     !SECONDS
             CVRT2DYEAR = 3.168873850681D-08
         CASE (2)                                                     !MINUTES
             CVRT2DYEAR = 1.901324310409D-06
         CASE (3)                                                     !HOURS
             CVRT2DYEAR = 1.140794586245D-04
         CASE (5)
             CVRT2DYEAR = 1D0
       END SELECT
       !
    ELSEIF(YEARTYPE .EQ. 1) THEN  ! 365 days to year
       !  	   		
       SELECT CASE (ITMUNI)
         CASE (0, 4)                                                  !DAYS
             CVRT2DYEAR = 2.739726027397D-03
         CASE (1)                                                     !SECONDS
             CVRT2DYEAR = 3.170979198376D-08
         CASE (2)                                                     !MINUTES
             CVRT2DYEAR = 1.902587519026D-06
         CASE (3)                                                     !HOURS
             CVRT2DYEAR = 1.141552511416D-04
         CASE (5)
             CVRT2DYEAR = 1D0
       END SELECT
       !
    ELSEIF(YEARTYPE .EQ. 2) THEN  ! 366 days to year
       !
       SELECT CASE (ITMUNI)
         CASE (0, 4)                                                  !DAYS
             CVRT2DYEAR = 2.732240437158D-03
         CASE (1)                                                     !SECONDS
             CVRT2DYEAR = 3.162315320785D-08
         CASE (2)                                                     !MINUTES
             CVRT2DYEAR = 1.897389192471D-06
         CASE (3)                                                     !HOURS
             CVRT2DYEAR = 1.138433515483D-04
         CASE (5)
             CVRT2DYEAR = 1D0
       END SELECT
       !
    END IF
    !
  END FUNCTION
  !
  !#########################################################################################################################
  !
  SUBROUTINE MASS_ERROR_COUNT_PRINT(IOUT,MXITER,NFAIL,ISTP,PVOL_ERR)
  INTEGER,          INTENT(IN):: IOUT, MXITER, NFAIL, ISTP
  DOUBLE PRECISION, INTENT(IN):: PVOL_ERR
  CHARACTER(32)::PERR
  !
  WRITE(PERR,'(F32.2)') ABS(PVOL_ERR)
  PERR = ADJUSTL(PERR)
  !
  IF(NFAIL > Z) THEN
      WRITE(*,'(/ 1X A, //3X 3A, // 3x 5A, // 3x A, // 3x 3A /)') 'Final Remark:',                               &
      'The solver reached the maximum number of outer iterations (MXITER = ',NUM2STR(MXITER),')',                &
      'A total of ',NUM2STR(NFAIL),' times out of ',NUM2STR(ISTP),' time steps.',                                &
      'Please check the "VOLUMETRIC BUDGET FOR ENTIRE MODEL" to ensure that mass rate balance errors are acceptable for all time steps.', &
      'The cumulative mass error for the entire simulation is ',TRIM(PERR),' %'
      !
      WRITE(IOUT,'(/ 1X A, //3X 3A, // 3x 5A, //3x A, // 3x 3A /)') 'Final Remark:',                             &
      'The solver reached the maximum number of outer iterations (MXITER = ',NUM2STR(MXITER),')',                &
      'A total of ',NUM2STR(NFAIL),' times out of ',NUM2STR(ISTP),' time steps.',                                &
      'Please check the "VOLUMETRIC BUDGET FOR ENTIRE MODEL" to ensure that mass rate balance errors are acceptable for all time steps.', &
      'The cumulative mass error for the entire simulation is ',TRIM(PERR),' %'
  ELSE
      WRITE(*,'(/ 1X A, // 3x 3A, // 3x A, // 3x 3A /)') 'Final Remark:',                                                                        &
      'The solver successfully met the user supplied convergence criteria, HCLOSE and RCLOSE, for all ',NUM2STR(ISTP),' time steps.',                 &
      'Please check the "VOLUMETRIC BUDGET FOR ENTIRE MODEL" to ensure that mass rate balance errors are acceptable for all time steps.', &
      'The cumulative mass error for the entire simulation is ',TRIM(PERR),' %'
      !
      WRITE(IOUT,'(/ 1X A, // 3x 3A, // 3x A, // 3x 3A /)') 'Final Remark:',                                                                        &
      'The solver successfully met the user supplied convergence criteria, HCLOSE and RCLOSE, for all ',NUM2STR(ISTP),' time steps.',                 &
      'Please check the "VOLUMETRIC BUDGET FOR ENTIRE MODEL" to ensure that mass rate balance errors are acceptable for all time steps.', &
      'The cumulative mass error for the entire simulation is ',TRIM(PERR),' %'
  END IF
  !
  END SUBROUTINE
  !
  !#########################################################################################################################
  !
  SUBROUTINE MASS_ERROR_PRINT(IOUT,ICNVG,HAS_PDIFFPRT,MXITER,MAX_REL_VOL_INVOKED,MAX_REL_VOL_ERROR,PDIFFPRT,KPER,KSTP,BUDPERC,R,C,L,ERR,I,J,K,VERR)
  INTEGER, INTENT(IN):: ICNVG,MXITER,IOUT,PDIFFPRT,KPER,KSTP
  LOGICAL, INTENT(IN):: MAX_REL_VOL_INVOKED,HAS_PDIFFPRT
  DOUBLE PRECISION, INTENT(IN):: MAX_REL_VOL_ERROR,ERR,VERR,BUDPERC
  INTEGER, INTENT(IN):: R,C,L,I,J,K
  TYPE(CHARACTER_TYPE):: TXT
  CHARACTER(5):: CH5
  !
  CH5 = BLNK
  CALL TXT%DESTROY()
  !
  IF(HAS_PDIFFPRT) THEN
     WRITE(*,'(/25x A,F6.1,A/)')'Warning: Volumetric Rate Budget Percent Error is ',BUDPERC,'%'
     WRITE(*,'(34x 3A,/34x 6A/)')                                                                &
     'The worst vol. rate error is:             ',NUM2STR(ERR),'   (L^3/T)',                     &
     'For model cell (Lay, Row, Col):           ',NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
     WRITE(*,'(34x 3A,/34x 6A/)')                                                                &
     'Worst vol. rate error per cell volume is: ',NUM2STR(VERR),'   (1/T)',                      &
     'For model cell (Lay, Row, Col):           ',NUM2STR(L),', ',NUM2STR(R),', ',NUM2STR(C)
  END IF
  !
  ! ADDITIONAL OUTPUT DO TO NEW MASS BALANCE CHECK
  !
  IF(MAX_REL_VOL_INVOKED) THEN
     IF(ICNVG.EQ.0) THEN
         TXT=BLN//CH5//                                                                                                   &
        'THIS WARNING MAY HAVE OCCURRED EITHER DUE TO HAVING AT LEAST ONE MODEL CELL EXCEEDING'//NL//CH5//                                     &
        'THE A VOLUMETRIC FLOW RATE ERROR FRACTION TOLERANCE AND/OR'//NL//CH5//                            &
        'IT FAILED TO MEET SOLVER CONVERGENCE CRITERIA.'
     ELSE
        TXT=BLN//CH5//                                                                                         &
       'NOTE: THE REQUIRED NUMBER OF SOLVER ITERATIONS WERE EXTENDED DUE HAVING AT LEAST ONE MODEL CELL'//NL//CH5//  &
       'EXCEED THE A VOLUMETRIC FLOW RATE ERROR FRACTION TOLERANCE.'
     END IF
     !
     CALL TXT%ADD(BLN//CH5//                                                                                         &
     "THIS FRACTION IS THE CELL'S VOL. RATE ERROR DIVIDED BY THE MODEL CELL'S VOLUME (ERROR UNOT OF 1/T)"//NL//CH5// &
     'OneWater HAS A VOL. RATE ERROR PER CELL VOLUME LIMIT OF '//NUM2STR(MAX_REL_VOL_ERROR)//NL//CH5//              &
     'BEFORE CONVERGENCE IS ALLOWED.'//BLN//CH5//                                                                    &
     'THIS CAN BE ADJUSTED WITH THE BASIC (BAS) PACKAGE OPTION '//NL//CH5//                                          &
     '"MAX_RELATIVE_VOLUME_ERROR" FOLLOWED BY THE DESIRED LIMIT.'                                                    )
  END IF
!
!------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
  IF(ICNVG.EQ.0) THEN
   !
   CALL TXT%ADD_BEGIN(CH5//                                                                                                   &
     'THE SOLVER ITERATION COUNT REACHED THE MAXIMUM NUMBER OF OUTER ITERATIONS (MXITER = '//NUM2STR(MXITER)//')'//NL//CH5//  &
     'FOR STRESS PERIOD '//NUM2STR(KPER)//' TIME STEP NUMBER '//NUM2STR(KSTP)//BLN//CH5//                                     &
     "THE LAST SOLVER ITERATION'S VOLUMETRIC RATE BUDGET PERCENT DISCREPANCY IS "//NUM2STR(BUDPERC)//' %'//BLN//CH5//         &
     'THE WORST VOL. RATE ERROR IS: '//NUM2STR(ERR,-15)//'                 (L^3/T)'//NL//CH5//                                &
     'FOR MODEL CELL LAY, ROW, COL:  '//NUM2STR(L)//', '//NUM2STR(R)//', '//NUM2STR(C)//BLN//CH5//                            &
     'THE WORST VOL. RATE ERROR PER CELL VOLUME IS: '//NUM2STR(VERR,-15)//' (1/T)'//NL//CH5//                                 &
     'FOR MODEL CELL LAY, ROW, COL:                 '//NUM2STR(K)//', '//NUM2STR(I)//', '//NUM2STR(J) )
   !
  ELSEIF(HAS_PDIFFPRT) THEN
   !
   CALL TXT%ADD_BEGIN( CH5//                                                                                                              &
     "THE LAST SOLVER ITERATION'S VOLUMETRIC RATE BUDGET PERCENT DISCREPANCY IS "//NUM2STR(BUDPERC)//' %'//NL//CH5//                      &
     'WHICH IS GREATER THEN THE PERCENTERROR LIMIT OF '//NUM2STR(PDIFFPRT)//'%'//BLN//CH5//                                               &
     'THIS IS JUST A WARNING, YOU CAN CHANGE THE PERCENTERROR LIMIT'//NL//CH5//                                                           &
     'WITH THE BASIC PACKAGE OPTION "PERCENTERROR" FOLLOWED BY THE NEW LIMIT AS AN INTEGER (eg 5 indicates a limit of 5%).'//BLN//CH5//   &
     'THE WORST VOL. RATE ERROR IS: '//NUM2STR(ERR,-15)//'                 (L^3/T)'//NL//CH5//                                            &
     'FOR MODEL CELL LAY, ROW, COL:  '//NUM2STR(L)//', '//NUM2STR(R)//', '//NUM2STR(C)//BLN//CH5//                                        &
     'THE WORST VOL. RATE ERROR PER CELL VOLUME IS: '//NUM2STR(VERR,-15)//' (1/T)'//NL//CH5//                                             &
     'FOR MODEL CELL LAY, ROW, COL:             '//NUM2STR(K)//', '//NUM2STR(I)//', '//NUM2STR(J) )
    !
  END IF !(ICNVG.EQ.0)
  !
  IF(ICNVG.EQ.0 .OR. HAS_PDIFFPRT) CALL WARNING_MESSAGE(OUTPUT=IOUT, MSG=TXT%STR )
  !
  END SUBROUTINE
  !
  PURE SUBROUTINE RELAX_HNEW(DIM1, DIM2, DIM3, RELAX, IB, HOLD, HNEW)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64),                            INTENT(IN   ):: RELAX
    INTEGER,      DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: IB
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: HOLD
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(INOUT):: HNEW
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1, IB(I,J,K) > Z)
             !
             HNEW(I,J,K) = HOLD(I,J,K) + RELAX*( HNEW(I,J,K) - HOLD(I,J,K) )
    END DO
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE TOP_LIM_HNEW(DIM1, DIM2, DIM3, LIM, IB, TOP, HNEW, ICNVG)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3
    REAL(REAL64),                            INTENT(IN   ):: LIM
    INTEGER,      DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: IB
    REAL(REAL64), DIMENSION(DIM1,DIM2),      INTENT(IN   ):: TOP
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(INOUT):: HNEW
    INTEGER,                                 INTENT(INOUT):: ICNVG
    INTEGER:: I,J,K
    !
    DO CONCURRENT(K=1:DIM3, J=1:DIM2, I=1:DIM1, IB(I,J,K) > Z)
             !
             IF( TOP(I,J)+LIM < HNEW(I,J,K))  THEN
                                !
                                HNEW(I,J,K) = TOP(I,J)+LIM
                                !ICNVG = Z
             END IF
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE PRINT_TOP_LIM_HNEW(DIM1, DIM2, DIM3, LIM, IB, TOP, HNEW, SP, TS, IU, DT)
    INTEGER,                                 INTENT(IN   ):: DIM1, DIM2, DIM3, SP, TS, IU
    REAL(REAL64),                            INTENT(IN   ):: LIM
    INTEGER,      DIMENSION(DIM1,DIM2,DIM3), INTENT(IN   ):: IB
    REAL(REAL64), DIMENSION(DIM1,DIM2),      INTENT(IN   ):: TOP
    REAL(REAL64), DIMENSION(DIM1,DIM2,DIM3), INTENT(INOUT):: HNEW
    CHARACTER(8),                            INTENT(IN   ):: DT
    INTEGER:: I,J,K
    !
    DO K=1,DIM3
    DO J=1,DIM2
    DO I=1,DIM1
    IF(IB(I,J,K) > Z) THEN
             !
             IF( TOP(I,J)+LIM < HNEW(I,J,K))  THEN
                                WRITE(IU,'(A, 1X A, 2X A, 2(1X A), 3(2x A))') NUM2STR(SP,-3), NUM2STR(TS,-3), NUM2STR(K,-3), NUM2STR(J,-4), NUM2STR(I,-4), NUM2STR(TOP(I,J),-13), NUM2STR(HNEW(I,J,K),-13), TRIM(DT)
             END IF
    END IF
    END DO
    END DO
    END DO
    !
  END SUBROUTINE
  !
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  

!!!MODULE PACKAGE_LISTING_INTERFACE
!!!    USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
!!!    USE UTIL_INTERFACE, ONLY: UPPER, READ_TO_DATA, GET_WORD
!!!    USE CONSTANTS,   ONLY:BLNK,NL,TAB,COM,Z,ONE,TWO,TEN,DZ,UNO,DOS,DIEZ,TRUE,FALSE
!!!    IMPLICIT NONE
!!!    PRIVATE
!!!    PUBLIC:: PACKAGE_LISTING
!!!    !
!!!    TYPE, EXTENDS(GENERIC_OUTPUT_FILE):: PACKAGE_LISTING
!!!        LOGICAL:: PRINT
!!!    CONTAINS
!!!    PROCEDURE, PASS(POUT):: INIT => INITIALIZE_PACKAGE_LISTING_LINE_SCAN
!!!    END TYPE
!!!    !
!!!    CONTAINS
!!!    !
!!!    SUBROUTINE INITIALIZE_PACKAGE_LISTING_LINE_SCAN(POUT,LN,IOUT,IN)
!!!    !CHECKS FOR KEYWORD "PACKAGE_LIST_FILE" AND THEN OPENS A GENERIC OUTPUT FILE
!!!    !SET IN=0 TO SUPRESS READING NEXT LINE WHEN KEYWORD IS FOUND
!!!    CLASS(PACKAGE_LISTING):: POUT
!!!    INTEGER,      INTENT(IN   ):: IOUT, IN
!!!    CHARACTER(*), INTENT(INOUT):: LN
!!!    INTEGER:: LLOC, ISTART, ISTOP, DIM
!!!    CHARACTER(10):: KEY
!!!    !
!!!    POUT%PRINT = TRUE
!!!    POUT%IOUT = IOUT
!!!    POUT%IU   = IOUT 
!!!    !
!!!    DIM = LEN_TRIM(LN)
!!!    LLOC = ONE
!!!    DO WHILE (LLOC < DIM)
!!!          CALL GET_WORD(KEY,LN,LLOC)
!!!          !
!!!          IF(KEY == 'PACKAGE_LIST_FILE') THEN
!!!                                      CALL POUT%OPEN(LN,LLOC,IOUT,IN,SPLITMAXCOUNT=11)
!!!                                      !
!!!                                      POUT%IOUT = POUT%IU
!!!                                      !
!!!                                      IF(POUT%IU == Z)  POUT%PRINT = FALSE
!!!                                      !
!!!                                      IF(LLOC>ISTART) THEN
!!!                                            ISTOP = LLOC - ONE
!!!                                            LN(ISTART:ISTOP) = BLNK
!!!                                      END IF
!!!                                      !
!!!                                      IF(IN.NE.Z) CALL READ_TO_DATA(LN,IN,IOUT,IOUT)
!!!                                      !
!!!                                      EXIT
!!!          END IF
!!!    END DO
!!!    !
!!!  END SUBROUTINE
!!!  !    
!!!END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
MODULE BUDGET_DATEBASE_WRITER
  USE CONSTANTS,                       ONLY:Z,ONE,FIVE,NL,BLN
  USE NUM2STR_INTERFACE,               ONLY: NUM2STR
  USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
  USE CAST_TO_STRING
  IMPLICIT NONE
  PRIVATE
  !
  PUBLIC:: WRITE_DATEBASE
  !
  CONTAINS
  !
  SUBROUTINE WRITE_DATEBASE(DB, MSUM,VBNM,VBVL,KSTP,KPER,TOTIM,DELT,DATE)
    TYPE(GENERIC_OUTPUT_FILE),       INTENT(INOUT):: DB
    REAL,          DIMENSION(4,MSUM),INTENT(IN   ):: VBVL
    CHARACTER(16), DIMENSION(MSUM),  INTENT(IN   ):: VBNM
    CHARACTER(19),                   INTENT(IN   ):: DATE
    INTEGER,                         INTENT(IN   ):: MSUM, KSTP, KPER
    REAL,                            INTENT(IN   ):: TOTIM, DELT
    INTEGER:: MSUM1, I, J
    CHARACTER(:), ALLOCATABLE:: LN, BIN_REC
    CHARACTER(16):: TEXT, TEXT2
    REAL:: INFLOW, OUTFLOW, AVEFLOW, PERR
    !
    IF (DB%IU == Z)  RETURN
    !
    MSUM1 = MSUM-ONE
    !
    IF(KPER==ONE .AND. KSTP==ONE) THEN
        !
        IF(DB%BINARY) THEN
            LN = CAST2STR(MSUM1+FIVE)//'      DATE_START             PER             STP            DELT         SIMTIME'
            BIN_REC = BLN//'BAS PACKAGE DATABASE FRIENDLY OUTPUT IN BINARY FORMAT.'//NL//'THE FIRST RECORD OF BINARY IS AN INTEGER (int) THAT IS A COUNT OF THE NUMBER OF COLUMNS WRITTEN TO BINARY.'//NL//'AFTER THE "COUNT", THERE ARE "COUNT" CHARACTER VARIABLES (16char) THAT DEFINE THE ORDER OF ALL THE SUBSQUENT NUMERICAL OUTPUT.'//NL//'AFTER THE HEADER RECORD EACH BINARY RECORD IS AS FOLLOWS:'//BLN//'DATE_START (19char), PER (int), STP (int), DELT (single), SIMTIME (single)'
        ELSE
            LN = '         DATE_START   PER   STP           DELT        SIMTIME'
        END IF
        !
        DO I=ONE, MSUM1
            SELECT CASE (VBNM(I))
            CASE('   CONSTANT HEAD'); TEXT = 'CHD'
            CASE(' GHOST-NODE FLUX'); TEXT = 'LGR'
            CASE('     ET SEGMENTS'); TEXT = 'ETS'
            CASE(' SPECIFIED FLOWS'); TEXT = 'FHB'
            CASE(' HEAD DEP BOUNDS'); TEXT = 'GHB'
            CASE('INTERBED STORAGE'); TEXT = 'SWT'
            CASE('   LAKE  SEEPAGE'); TEXT = 'LAK'
            CASE('             MNW'); TEXT = 'MNW1'
            CASE('        RECHARGE'); TEXT = 'RCH'
            CASE('          DRAINS'); TEXT = 'DRN'
            CASE('      DRAINS_DRT'); TEXT = 'DRT'
            CASE(' RESERV. LEAKAGE'); TEXT = 'RES'
            CASE('     RIPARIAN_ET'); TEXT = 'RIP'
            CASE('   RIVER LEAKAGE'); TEXT = 'RIV'
            CASE('  STREAM LEAKAGE'); TEXT = 'SFR'
            CASE('INST. IB STORAGE'); TEXT = 'SUB_INST'
            CASE('DELAY IB STORAGE'); TEXT = 'SUB_DELAY'
            CASE('SUB INST    ELAS'); TEXT = 'SUB_INST_EL'
            CASE('SUB INST  INELAS'); TEXT = 'SUB_INST_IN'
            CASE('SUB DELAY   ELAS'); TEXT = 'SUB_DELAY_EL'
            CASE('SUB DELAY INELAS'); TEXT = 'SUB_DELAY_IN'
            CASE('      SWIADDTOCH'); TEXT = 'SWI'
            CASE('     SWR LEAKAGE'); TEXT = 'SWR'
            CASE('        SWR GWET'); TEXT = 'SWR_GWET'
            CASE('    UZF RECHARGE'); TEXT = 'UZF_RECH'
            CASE('           GW ET'); TEXT = 'UZF_GW_ET'
            CASE(' SURFACE LEAKAGE'); TEXT = 'UZF_LEAK'
            CASE('         WELLSv1'); TEXT = 'WEL_V1'
            CASE('           WELLS'); TEXT = 'WEL'
            CASE('      FARM WELLS'); TEXT = 'FMP_WELLS'
            CASE('FARM  NET  RECH.'); TEXT = 'FMP_FNR'
            CASE DEFAULT
                                     TEXT = ADJUSTL(VBNM(I))
                                     DO CONCURRENT(J=1:LEN_TRIM(TEXT), TEXT(J:J)==' ')
                                         TEXT(J:J)='_'
                                     END DO
            END SELECT
            !
            TEXT2 = TRIM(TEXT(:12))//'_OUT'
            TEXT  = TRIM(TEXT(:12))//'_IN'
            !
            IF(DB%BINARY) THEN
                LN = LN//ADJUSTR(TEXT)//ADJUSTR(TEXT2)
                !
                BIN_REC = BIN_REC//', '//TRIM(TEXT)//' (single)'//', '//TRIM(TEXT2)//' (single)'
            ELSE
                LN = LN//' '//ADJUSTR(TEXT)//' '//ADJUSTR(TEXT2)
            END IF
        END DO
        !
        IF(DB%BINARY) THEN
                            WRITE(DB%IOUT,'(A)') BIN_REC
                            DEALLOCATE(BIN_REC)
        END IF
        !
        IF(.NOT. DB%BINARY) LN = LN//'         IN_OUT  PERCENT_ERROR'
        !
        CALL DB%SET_HEADER(LN)
    END IF
    !
    IF(MOD(KPER,11)==Z) CALL DB%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IF(DB%BINARY) THEN
        WRITE(DB%IU) DATE,KPER,KSTP,DELT,TOTIM
        DO I=ONE, MSUM1
                     WRITE(DB%IU) VBVL(3,I),VBVL(4,I)
        END DO
    ELSE
        !
        WRITE(DB%IU,'(A,2I6,2A15)',ADVANCE='NO') DATE, KPER, KSTP, NUM2STR(DELT), NUM2STR(TOTIM)
        !
        INFLOW=0.0
        OUTFLOW=0.0
        DO I=ONE, MSUM1
                     WRITE(DB%IU,'(2A17)',ADVANCE='NO') NUM2STR(VBVL(3,I)), NUM2STR(VBVL(4,I))
                     INFLOW  = INFLOW + VBVL(3,I)
                     OUTFLOW = OUTFLOW + VBVL(4,I)
        END DO
        AVEFLOW = (INFLOW + OUTFLOW)/2.0
        PERR = 0.0
        IF(AVEFLOW.GT.0.0) PERR = 100.0*(INFLOW - OUTFLOW)/AVEFLOW
        !
        WRITE(DB%IU,'(2A15)') NUM2STR(INFLOW-OUTFLOW), NUM2STR(PERR)
        !
    END IF
  END SUBROUTINE
    
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  
MODULE OWHM_HEADER_INTERFACE
  USE CONSTANTS, ONLY:NL   
  IMPLICIT NONE
  PRIVATE:: NL   
  !
  CONTAINS
  !
  FUNCTION OWHM_HEADER() RESULT(LINE)
    CHARACTER(:),ALLOCATABLE:: LINE
    !
    LINE=                                                                                                                            &
         '      MMMMMMM                                       MM      MMM     MM'                                            //NL//  &
         '    MMM     MMM    MM      M   MMMMMMMM             LM     DM M     ML     MM     MMMMMMMMM  MMMMMMMM  MMMMMMMM'   //NL//  &
         '   MM         MM   MMM     M   MM                    MM    MM M     M     MMMM        M      M         MM     NM'  //NL//  &
         '   MM         MM   MM M    M   MM                    MM    M  MM   MM    MM  MM       M      M         MM      M'  //NL//  &
         '   MM         MM   MM  M   M   MMMMMMMM               MN  MM   M   MM   MM    MM      M      MMMMMMM   MMMMMMMMM'  //NL//  &
         '   MM         MM   MM   M  M   MM         MMMMMMM     MM  M    MM MM    MMMMMMMM      M      M         MM   MM'    //NL//  &
         '   MM         MM   MM    M M   MM                     LM MM    NM MM   MM      MM     M      M         MM    MM'   //NL//  &
         '    MM       NM    MM     MM   MM                      M M      M M   MN        MM    M      M         MM     MM'  //NL//  &
         '     MMMMMMMMN     MM      M   MMMMMMMM                7MM      7MM  MN          MM   M      MMMMMMMM  MM      MM'
  
  !   LINE=                                                                                                              &
  !        '      MMMMMMM      MM      MMM     MM   MM       MM   MMMI      IMMM                  MMMMMD'         //NL//  &
  !        '    MMM     MMM    LM     DMMM     MM   MM       MM   MMMM      MMMM                MM/   LMM'        //NL//  &
  !        '   MM         MM    MM    MM M     M    MM       MM   MM LM    ML MM    MM      ML  MM     MM'        //NL//  &
  !        '   MM         MM    MM    M  MM   MM    MM       MM   MM MM    MM MM     MN     MN         MM'        //NL//  &
  !        '   MZ         MM     MN  MM   M   MM    MMMMMMMMMMM   MM  MM   MM MM     LM    MN         MM'         //NL//  &
  !        '   MM         MM     MM  M    MM  M     MM       MM   MM  MM   M  MM      MM   M        MM'           //NL//  &
  !        '   MM         MM     LM MM    LM MM     MM       MM   MM   ML LM  MM       M  MM      MMM'            //NL//  &
  !        '    MM       NM       M_M/     M_M      MM       MM   MM   LM_M   MM       NM M      MM'              //NL//  &
  !        '     MMMMMMMMN        7MM      MMM      MM       MM   MM    LM    MM        MML     MMMMMMMMM'
  END FUNCTION
  !
  SUBROUTINE PROGRAM_DONE(IOUT)
    INTEGER, INTENT(IN):: IOUT
    !
    WRITE(IOUT,'(//A,/A/)') 'The following is UTF-8 (unicode) formatting, so it may not appear correctly.','If not it just says simulation complete.'
    WRITE(IOUT,'(//A)')                                                                                                                   &
          '  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄        ▄  ▄▄▄▄▄▄▄▄▄▄▄               ▄         ▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄  ▄▄▄▄▄▄▄▄▄▄▄ '//NL//  &
          ' ▐░░░░░░░░░░░▌▐░░▌      ▐░▌▐░░░░░░░░░░░▌             ▐░▌       ▐░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌'//NL//  &
          ' ▐░█▀▀▀▀▀▀▀█░▌▐░▌░▌     ▐░▌▐░█▀▀▀▀▀▀▀▀▀              ▐░▌       ▐░▌▐░█▀▀▀▀▀▀▀█░▌ ▀▀▀▀█░█▀▀▀▀ ▐░█▀▀▀▀▀▀▀▀▀ ▐░█▀▀▀▀▀▀▀█░▌'//NL//  &
          ' ▐░▌       ▐░▌▐░▌▐░▌    ▐░▌▐░▌                       ▐░▌       ▐░▌▐░▌       ▐░▌     ▐░▌     ▐░▌          ▐░▌       ▐░▌'//NL//  &
          ' ▐░▌       ▐░▌▐░▌ ▐░▌   ▐░▌▐░█▄▄▄▄▄▄▄▄▄   ▄▄▄▄▄▄▄▄▄  ▐░▌   ▄   ▐░▌▐░█▄▄▄▄▄▄▄█░▌     ▐░▌     ▐░█▄▄▄▄▄▄▄▄▄ ▐░█▄▄▄▄▄▄▄█░▌'//NL//  &
          ' ▐░▌       ▐░▌▐░▌  ▐░▌  ▐░▌▐░░░░░░░░░░░▌ ▐░░░░░░░░░▌ ▐░▌  ▐░▌  ▐░▌▐░░░░░░░░░░░▌     ▐░▌     ▐░░░░░░░░░░░▌▐░░░░░░░░░░░▌'//NL//  &
          ' ▐░▌       ▐░▌▐░▌   ▐░▌ ▐░▌▐░█▀▀▀▀▀▀▀▀▀   ▀▀▀▀▀▀▀▀▀  ▐░▌ ▐░▌░▌ ▐░▌▐░█▀▀▀▀▀▀▀█░▌     ▐░▌     ▐░█▀▀▀▀▀▀▀▀▀ ▐░█▀▀▀▀█░█▀▀ '//NL//  &
          ' ▐░▌       ▐░▌▐░▌    ▐░▌▐░▌▐░▌                       ▐░▌▐░▌ ▐░▌▐░▌▐░▌       ▐░▌     ▐░▌     ▐░▌          ▐░▌     ▐░▌  '//NL//  &
          ' ▐░█▄▄▄▄▄▄▄█░▌▐░▌     ▐░▐░▌▐░█▄▄▄▄▄▄▄▄▄              ▐░▌░▌   ▐░▐░▌▐░▌       ▐░▌     ▐░▌     ▐░█▄▄▄▄▄▄▄▄▄ ▐░▌      ▐░▌ '//NL//  &
          ' ▐░░░░░░░░░░░▌▐░▌      ▐░░▌▐░░░░░░░░░░░▌             ▐░░▌     ▐░░▌▐░▌       ▐░▌     ▐░▌     ▐░░░░░░░░░░░▌▐░▌       ▐░▌'//NL//  &
          '  ▀▀▀▀▀▀▀▀▀▀▀  ▀        ▀▀  ▀▀▀▀▀▀▀▀▀▀▀               ▀▀       ▀▀  ▀         ▀       ▀       ▀▀▀▀▀▀▀▀▀▀▀  ▀         ▀ '//NL//  &
          NL//NL//                                                                                                                        &
          '                  ███████╗██╗███╗   ███╗██╗   ██╗██╗      █████╗ ████████╗██╗ ██████╗ ███╗   ██╗'//NL//  &
          '                  ██╔════╝██║████╗ ████║██║   ██║██║     ██╔══██╗╚══██╔══╝██║██╔═══██╗████╗  ██║'//NL//  &
          '                  ███████╗██║██╔████╔██║██║   ██║██║     ███████║   ██║   ██║██║   ██║██╔██╗ ██║'//NL//  &
          '                  ╚════██║██║██║╚██╔╝██║██║   ██║██║     ██╔══██║   ██║   ██║██║   ██║██║╚██╗██║'//NL//  &
          '                  ███████║██║██║ ╚═╝ ██║╚██████╔╝███████╗██║  ██║   ██║   ██║╚██████╔╝██║ ╚████║'//NL//  &
          '                  ╚══════╝╚═╝╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝'//NL//  &
          NL//                                                                                                      &
          '                       ██████╗ ██████╗ ███╗   ███╗██████╗ ██╗     ███████╗████████╗███████╗     '//NL//  &
          '                      ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██║     ██╔════╝╚══██╔══╝██╔════╝     '//NL//  &
          '                      ██║     ██║   ██║██╔████╔██║██████╔╝██║     █████╗     ██║   █████╗       '//NL//  &
          '                      ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║     ██╔══╝     ██║   ██╔══╝       '//NL//  &
          '                      ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ███████╗███████╗   ██║   ███████╗     '//NL//  &
          '                       ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚══════╝╚══════╝   ╚═╝   ╚══════╝     '//NL//  &
          NL//                                                                                                      &
          NL//                                                                                                      &
          '                                          █████████████████████████████'//NL//  &
          '                                          █░░░░░░░░░░░░░░░░░░░░░░░░░░░█'//NL//  &
          '                                          █░░░ ▄▀▀▄░░░░░░░░░░▄▀▀▄░░░░░█'//NL//  &
          '                                          █░░░█░░░▀▄░▄▄▄▄▄░▄▀░░░█░░░░░█'//NL//  &
          '                                          █░░░░▀▄░░░▀░░░░░▀░░░▄▀░░░░░░█'//NL//  &
          '                                          █░░░░░░▌░▄▄░░░▄▄░▐▀▀░░░░░░░░█'//NL//  &
          '                                          █░░░░░▐░░█▄░░░▄█░░▌▄▄▀▀▀▀█░░█'//NL//  &
          '                                          █░░░░░▌▄▄▀▀░▄░▀▀▄▄▐░░░░░░█░░█'//NL//  &
          '                                          █░░▄▀▀▐▀▀░▄▄▄▄▄░▀▀▌▄▄▄░░░█░░█'//NL//  &
          '                                          █░░█░░░▀▄░█░░░█░▄▀░░░░█▀▀▀░░█'//NL//  &
          '                                          █░░░▀▄░░▀░░▀▀▀░░▀░░░▄█▀░░░░░█'//NL//  &
          '                                          █░░░░░█░░░░░░░░░░░▄▀▄░▀▄░░░░█'//NL//  &
          '                                          █░░░░░█░░░░░░░░░▄▀█░░█░░█░░░█'//NL//  &
          '                                          █░░░░░█░░░░░░░░░░░█▄█░░▄▀░░░█'//NL//  &
          '                                          █░░░░░█░░░░░░░░░░░████▀░░░░░█'//NL//  &
          '                                          █░░░░░▀▄▄▀▀▄▄▀▀▄▄▄█░░░░░░░░░█'//NL//  &
          '                                          █░░░░░░░░░░░░░░░░░░░░░░░░░░░█'//NL//  &
          '                                          █████████████████████████████'//NL
            
  END SUBROUTINE
  !
END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  
!MODULE ADAPTIVE_DAMPING
!   TYPE ADAP_TYPE
!       INTEGER:: NHEAD
!   END TYPE
!   !
!   TYPE(ADAP_TYPE):: ADAP
!   !
!   
!END MODULE
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  
!!!MODULE DATE_STEPS_INTERFACE
!!!  USE CONSTANTS
!!!  USE DATE_OPERATOR_INSTRUCTION  
!!!  IMPLICIT NONE
!!!  !
!!!  TYPE DATE_TS
!!!     INTEGER:: N=Z
!!!     TYPE(DATE_OPERATOR),DIMENSION(:),ALLOCATABLE:: TS
!!!  END TYPE
!!!  !
!!!  TYPE DATE_STEPS
!!!      INTEGER:: INUSE = FALSE
!!!      INTEGER:: N=Z
!!!      TYPE(DATE_TS),DIMENSION(:),ALLOCATABLE:: SP
!!!      !
!!!      CONTAINS
!!!      !
!!!      PROCEDURE, PASS(DTS):: INIT     => INITIALIZE_DATE_STEPS!(N) 
!!!      PROCEDURE, PASS(DTS):: ALLOC_TS => ALLOCATE_TIMESTEP_DATE_STEPS!(N,M) 
!!!      PROCEDURE, PASS(DTS):: SET      => SET_TIMESTEP_DATE_STEPS!(N,M,DATE)
!!!      PROCEDURE, PASS(DTS):: DESTROY  => DESTROY_DATE_STEPS
!!!  END TYPE
!!!  !
!!!  CONTAINS
!!!  !
!!!  PURE SUBROUTINE INITIALIZE_DATE_STEPS(DTS,N)
!!!  CLASS(DATE_STEPS), INTENT(INOUT):: DTS
!!!  INTEGER,           INTENT(IN   ):: N
!!!  !
!!!  CALL  DESTROY_DATE_STEPS(DTS)
!!!  !
!!!  DTS%N = N
!!!  !
!!!  DTS%INUSE = TRUE
!!!  !
!!!  ALLOCATE(DTS%SP(N))
!!!  !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE ALLOCATE_TIMESTEP_DATE_STEPS(DTS,N,M)
!!!  CLASS(DATE_STEPS), INTENT(INOUT):: DTS
!!!  INTEGER,           INTENT(IN   ):: N,M
!!!  !
!!!  IF(DTS%INUSE) THEN
!!!    IF(ALLOCATED(DTS%SP(N)%TS)) DEALLOCATE(DTS%SP(N)%TS)
!!!    ALLOCATE(DTS%SP(N)%TS(Z:M))
!!!    DTS%SP(N)%N=M
!!!  END IF
!!!  !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SET_TIMESTEP_DATE_STEPS(DTS,N,M,DATE)
!!!  CLASS(DATE_STEPS),   INTENT(INOUT):: DTS
!!!  INTEGER,             INTENT(IN   ):: N,M
!!!  TYPE(DATE_OPERATOR), INTENT(IN   ):: DATE
!!!  !
!!!  IF(DTS%INUSE) DTS%SP(N)%TS(M) = DATE
!!!  !
!!!  END SUBROUTINE
!!!  !
!!!  !PURE FUNCTION GET_TIMESTEP_DATE_STEPS(DTS,N,M) RESULT (DATE)
!!!  !CLASS(DATE_STEPS),   INTENT(INOUT):: DTS
!!!  !INTEGER,             INTENT(IN   ):: N,M
!!!  !TYPE(DATE_OPERATOR), INTENT(IN   ):: DATE
!!!  !!
!!!  !IF(DTS%INUSE) DTS%SP(N)%TS(M) = DATE
!!!  !!
!!!  !END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE DESTROY_DATE_STEPS(DTS)
!!!  CLASS(DATE_STEPS), INTENT(INOUT):: DTS
!!!  !
!!!  IF(ALLOCATED(DTS%SP)) DEALLOCATE(DTS%SP)
!!!  !
!!!  DTS%N = Z
!!!  DTS%INUSE = FALSE
!!!  !
!!!  END SUBROUTINE
!!!  !
!!!END MODULE

MODULE LOAD_SUPER_NAMES_INTERFACE!, ONLY: LOAD_SUPER_NAMES
  USE CONSTANTS
  USE GENERIC_OPEN_INTERFACE,   ONLY: UTF8_BOM_OFFSET_REWIND
  USE UTIL_INTERFACE,           ONLY: READ_TO_DATA, PARSE_WORD_UP, GET_INTEGER             
  USE NAME_ID_INTERFACE,        ONLY: NAME_ID
  USE WARNING_TYPE_INSTRUCTION, ONLY: WARNING_TYPE
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: LOAD_SUPER_NAMES
  !
  CONTAINS
  !
  SUBROUTINE LOAD_SUPER_NAMES(LINE, IFIL, IOUT, SN)
    CHARACTER(*),                            INTENT(INOUT):: LINE 
    INTEGER,                                 INTENT(IN   ):: IFIL, IOUT
    TYPE(NAME_ID), DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: SN
    !
    TYPE(WARNING_TYPE):: WRN
    LOGICAL:: EOF
    INTEGER:: LLOC, ISTOP, ISTART
    INTEGER:: IVAL
    INTEGER, DIMENSION(TWO):: IVEC
    !
    INTEGER:: DIM_SFR, DIM_FMP, DIM_SWO
    INTEGER:: I
    LOGICAL:: IS_BOM
    !
    CALL WRN%INIT()
    !
    CALL UTF8_BOM_OFFSET_REWIND(IFIL, IS_BOM)
    !
    DIM_SFR = Z
    DIM_FMP = Z
    DIM_SWO = Z
    !
    CALL READ_TO_DATA(LINE, IFIL, IOUT, EOF=EOF)
    !
    DO WHILE (.NOT. EOF)
        LLOC=ONE
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        SELECT CASE ( LINE(ISTART:ISTOP) )
        CASE ("SFR"); DIM_SFR = DIM_SFR + ONE
        CASE ("FMP"); DIM_FMP = DIM_FMP + ONE
        CASE ("SWO"); DIM_SWO = DIM_SWO + ONE
        END SELECT
        !
        CALL READ_TO_DATA(LINE, IFIL, IOUT, EOF=EOF)
    END DO
    !
    CALL SN(1)%INIT(DIM_SFR)
    CALL SN(2)%INIT(DIM_FMP)
    CALL SN(3)%INIT(DIM_SWO)
    !
    IF(IS_BOM) THEN
        CALL UTF8_BOM_OFFSET_REWIND(IFIL)
    ELSE
        REWIND(IFIL)
    END IF
    !
    DIM_SFR = Z
    DIM_FMP = Z
    DIM_SWO = Z
    !
    CALL READ_TO_DATA(LINE, IFIL, IOUT, EOF=EOF)
    !
    DO WHILE (.NOT. EOF)
        LLOC=ONE
        CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
        SELECT CASE ( LINE(ISTART:ISTOP) )
        CASE ("SFR")
                      I = ONE  !SFR POSITION
                      DIM_SFR = DIM_SFR + ONE
                      !
                      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                      !
                      CALL SN(I)%SET_NAM(DIM_SFR, LINE(ISTART:ISTOP))
                      !
                      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IFIL,IVEC(ONE), MSG = 'FAILED TO LOAD "ISEG"')
                      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IFIL,IVEC(TWO), ERROR_VAL=Z)
                      !
                      CALL SN(I)%SET_ID_ALLOC(DIM_SFR, IVEC=IVEC)
        CASE ("FMP")
                      I = TWO  !FMP POSITION
                      DIM_FMP = DIM_FMP + ONE
                      !
                      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                      !
                      CALL SN(I)%SET_NAM(DIM_FMP, LINE(ISTART:ISTOP))
                      !
                      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IFIL,IVAL)
                      !
                      CALL SN(I)%SET_ID_ALLOC(DIM_FMP, IVAL=IVAL)
        CASE ("SWO")
                      I = THREE  !FMP POSITION
                      DIM_SWO = DIM_SWO + ONE
                      !
                      CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
                      !
                      CALL SN(I)%SET_NAM(DIM_SWO, LINE(ISTART:ISTOP))
                      !
                      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IFIL,IVAL)
                      !
                      CALL SN(I)%SET_ID_ALLOC(DIM_SWO, IVAL=IVAL)
        CASE DEFAULT
                      CALL WRN%INIT(LINE(ISTART:ISTOP)//' PACKAGE IS NOT SUPPORTED BY SUPER_NAMES. THE FOLLOWING LINE WILL BE IGNORED: "'//TRIM(LINE)//'"'//NL)
        END SELECT
        !
        CALL READ_TO_DATA(LINE, IFIL, IOUT, EOF=EOF)
    END DO
    !
    IF(WRN%RAISED ) CALL WRN%CHECK(HED='SUPER_NAMES HAD THE FOLLOWING WARNINGS RAISED:'//NL, OUTPUT=IOUT, TAIL=NL )
    !
  END SUBROUTINE
END MODULE
!
!
!
MODULE BAS_OPTIONS_AND_STARTDATE!, ONLY: GET_BAS_OPTIONS(LINE, INBAS, IOUT, ICHFLG, IPRTIM, MXBUD, HSHIFT, WARN_DIM, USE_PAUSE, TIME_INFO, SUPER_NAMES_IN, STARTING_DATE)
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  !
  USE CONSTANTS
  USE GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,  ONLY: GENERIC_OUTPUT_FILE
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USE UTIL_INTERFACE,                   ONLY: STOP_ERROR, WARNING_MESSAGE, READ_TO_DATA, PARSE_WORD, PARSE_WORD_UP, &
                                              GET_WORD, GET_INTEGER, GET_NUMBER, COMMENT_INDEX, IS_INTEGER
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR
  USE ULOAD_AND_SFAC_INTERFACE,         ONLY: ULOAD
  USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
  USE DATE_OPERATOR_INSTRUCTION,        ONLY: DATE_OPERATOR
  USE BAS_UTIL,                         ONLY: DELT_TO_DAY

  !
  USE GWFBASMODULE, ONLY: PRNT_CNVG_NTERM, PRNT_CNVG_OUTER, PRNT_CNVG, PRNT_CNVG_LRC, PRNT_CNVG_DIF,     &
                          PRNT_FRES_NTERM, PRNT_FRES_OUTER, PRNT_FRES, PRNT_FRES_LRC, PRNT_FRES_DIF,     &
                          PRNT_VERR_NTERM, PRNT_VERR_OUTER, PRNT_VERR, PRNT_VERR_LRC, PRNT_VERR_DIF,     &
                          MIN_ITER_INPUT,  MIN_SOLVER_INTER,  MIN_SOLVER_INTER_SP, MIN_SOLVER_INTER_NEW, &
                          MAX_REL_VOL_ERROR, OSCIL_DMP_LRC, OSCIL_DMP_DIF, OSCIL_DMP_OUTER,              &
                          DAMPEN_START, DAMPEN_START_ITR, DAMPEN_START_DMP,                              &
                          ABOVE_GSE_LIM, ABOVE_GSE_PRT_LIM, ABOVE_GSE_PRT,                               &
                          DEALLOCATE_MULT, STOPER, PDIFFPRT, HAS_STARTDATE,PRNT_RES,                     &
                          INTER_INFO, BUDGETDB, DATE_SP, REALTIM, USE_LEAP_YR, REALTIM_PER, REALTIM,     &
                          PRNT_RES, PRNT_RES_LIM, PRNT_RES_CUM, PRNT_RES_CUM_ARR
  USE GLOBAL,       ONLY: IXSEC, IFREFM, NOCBC, BIN_REAL_KIND, NLAY, NROW, NCOL, NOCBC, NPER, SPEND, SPSTART, INPUT_CHECK
  USE PARAMMODULE,  ONLY: MXPAR,MXCLST,MXINST,PROPPRINT
  !
  USE GLOBAL,       ONLY: ITMUNI, NSTP, SPTIM, PERLEN
  IMPLICIT NONE
  !
  PRIVATE
  PUBLIC:: GET_BAS_OPTIONS
  !
  CONTAINS
  !  
  SUBROUTINE GET_BAS_OPTIONS(LINE, INBAS, IOUT, ICHFLG, IPRTIM, MXBUD, HSHIFT, WARN_DIM, USE_PAUSE, TIME_INFO, SUPER_NAMES_IN, STARTING_DATE)
    CHARACTER(*),                    INTENT(INOUT):: LINE
    INTEGER,                         INTENT(IN   ):: INBAS, IOUT
    INTEGER,                         INTENT(INOUT):: ICHFLG, IPRTIM, MXBUD
    REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: HSHIFT
    LOGICAL,                         INTENT(INOUT):: WARN_DIM, USE_PAUSE
    TYPE(GENERIC_OUTPUT_FILE),       INTENT(INOUT):: TIME_INFO
    TYPE(GENERIC_INPUT_FILE) ,       INTENT(INOUT):: SUPER_NAMES_IN
    TYPE(DATE_OPERATOR),             INTENT(INOUT):: STARTING_DATE
    !
    INTEGER:: LLOC, ISTART, ISTOP, I, J, K
    LOGICAL:: SKIP_OPT_LINE, HAS_OPT_LINE, NO_OPT_LINE, FOUND_BEGIN, NO_CONVERGENCE_STOP
    CHARACTER(32):: KEY
    TYPE(GENERIC_BLOCK_READER):: BL
    TYPE(DATE_OPERATOR):: DATE
    TYPE(WARNING_TYPE):: WRN
    !
    CALL WRN%INIT()
    !
    NO_CONVERGENCE_STOP = FALSE
    !
    !Read BAS Package file.
    !READ AND PRINT COMMENTS.  SAVE THE FIRST TWO COMMENTS IN HEADNG.
    CALL READ_TO_DATA(LINE,INBAS,IOUT,IOUT)
    !
    WRITE(IOUT,'(/A)')' THE BAS PACKAGE NOW DEFAULTS TO FREE FORMAT.'
    WRITE(IOUT,'(A/)') ' USE OPTION "NOFREE" TO DISABLE FREE FORMAT.'
    !
    ! CHECK FOR OPTIONS BLOCK
    LLOC=1
    CALL GET_WORD(LINE,LLOC,ISTART,ISTOP,KEY)
    SKIP_OPT_LINE= FALSE
    HAS_OPT_LINE = FALSE
    !
    SELECT CASE(KEY)
    CASE('INTERNAL','EXTERNAL','OPEN/CLOSE','CONSTANT')
        SKIP_OPT_LINE= TRUE
        HAS_OPT_LINE = TRUE
    CASE('STARTDATE','START_DATE','DATE_START','DATESTART',                                                           &
         'LEAPYEARS','LEAPYEAR', 'STARTTIME',                                                                         &
         'XSECTION','CHTOCH','SHOWPROGRESS','FREE','NOFREE','PRINTTIME',                                              &
         'PRINT_CONVERGENCE','PRINT_FLOW_RESIDUAL','PRINT_RELATIVE_VOLUME_ERROR','PRINT_RELATIVE_VOL_ERROR',          &
         'MAX_RELATIVE_VOLUME_ERROR','MAX_RELATIVE_VOL_ERROR','MIN_SOLVER_ITERATION','MIN_SOLVER_ITER',               &
         'DAMPEN_OSCILLATION', 'DAMPEN_OSCILLATIONS','DAMPEN_START','DOUBLE_PRECISION_CBC',                           &
         'SHIFT_STRT','PROPPRINT','HEAD_DISTANCE_ABOVE_GSE_LIMIT','ABOVE_GSE_LIM','HEAD_DISTANCE_ABOVE_GSE_PRINT',    &
         'DEALLOCATE_MULT','NOCBC','NOCBCPACK','CBC_EVERY_TIMESTEP','CBC_LAST_TIMESTEP','NO_DIM_CHECK', 'NODIMCHECK', &
         'NO_CONVERGENCE_STOP', 'NO_FAILED_CONVERGENCE_STOP','STOPERROR','PERCENTERROR','MAXBUDGET','MAXPARAM',       &
         'RESIDUAL_ERROR_ARRAY','RESIDUAL_ERROR_ARRAY_THRESHOLD', 'CUMULATIVE_RESIDUAL_ERROR_ARRAY',                  &
         'ITERATION_INFO','INTERATION_INFO','BUDGETDB','TIME_INFO','SUPER_NAMES', 'SUPERNAMES',                 &
         'INPUT_CHECK', 'INPUTCHECK','PAUSE','FASTFORWARD')        
         !
         HAS_OPT_LINE = TRUE
         CALL WARNING_MESSAGE(TRIM(LINE),INBAS,IOUT,                                                                                   &
                          'BAS OPTION WARNING. FOUND OPTION "'//TRIM(KEY)//'"'//BLN//                                               &
                          'BUT BAS NOW EXPECTS OPTIONS BE WITHIN AN OPTIONS BLOCK RATHER THAN ALONG THE FIRST LINE OF INPUT.'//NL// &
                          '(IT WILL STILL PROCESS THIS OPTIONS LINE, BUT WILL FAIL ON OPTIONS THAT REQUIRE EXTRA INPUT,'//NL//      &
                          'SUCH AS "STOPERROR STOPER" OR "MAXPARAM MXPAR MXCLST MXINST".)'//BLN//                                   &
                          'PLEASE USE "BEGIN OPTIONS" AND THEN HAVE ONE OPTION PER LINE AND'//NL//                                  &
                          'END THE OPTIONS BLOCK WITH THE WORD "END" IN PLACE OF SINGLE LINE OF OPTIONS.'//BLN//                    &
                          'FOR EXAMPLE:'//BLN//                                                                                     &
                          'BEGIN OPTIONS'//BLN//                                                                                    &
                          '  FREE'//NL//                                                                                            &
                          '  NOCBC'//NL//                                                                                           &
                          '  BUDGETDB ./VOL_BUDGET_OUT.txt'//BLN//                                                                  &
                          'END OPTIONS'//BLN//                                                                                      &
                          'NOTE THAT BLOCK MAYBE EMPTY OR ENTIRELY NOT PRESENT TO NOT INCLUDE ANY OPTIONS.' )
    END SELECT
    !
    NO_OPT_LINE = .NOT. HAS_OPT_LINE
    IF(SKIP_OPT_LINE) THEN
        BL%NAME = 'SKIP'
        BACKSPACE(INBAS)  !NO OPTIONS SPECIFIED
    ELSEIF(HAS_OPT_LINE) THEN
        CALL BL%INIT()
        BL%NAME = 'OPTION'
        !
        I = COMMENT_INDEX(LINE)
        !
        CALL BL%ADD_LINE(LINE(1:I))  !Add trimmed line to be parsed
        !
        BL%NLINE = Z  !estimate number of loops
        LLOC=1
        CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
        !
        DO WHILE( KEY .NE. BLNK )
          !
          BL%NLINE = BL%NLINE + 1
          !
          CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY,NO_UPCASE=FALSE)
        END DO
        !
        ! Set for use in OPTIONS Block loop
        LLOC=1
    ELSE
          CALL BL%LOAD(INBAS,IOUT,LINE=LINE,FOUND_BEGIN=FOUND_BEGIN)
          IF(FOUND_BEGIN) BACKSPACE(INBAS)                   ! BLOCK LOADER AUTO-MOVES FORWARD A LINE WHEN BLOCK IS FOUND, BUT U2INT READS A LINE RATHER THEN PROCESSING LINE
    END IF
    !
    ! Pre-Check for Options that must be read first
    !
    IF((BL%NAME == 'OPTION' .OR. BL%NAME == 'OPTIONS') .AND. BL%NLINE>Z) THEN
       !
       K = LLOC
       CALL BL%START()
       DO J=1, BL%NLINE
          IF(  BL%LINE=='ERROR' ) EXIT
          !
          IF(NO_OPT_LINE) LLOC=1
          !
          CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
          !
          SELECT CASE (KEY)
          CASE('STARTDATE','START_DATE','DATE_START','DATESTART' )
            !
            CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
            CALL STARTING_DATE%INIT(BL%LINE(ISTART:ISTOP))  !EXPECTS 1/15/2015 or 2015-1-15
            !
            IF(STARTING_DATE%NOT_SET()) CALL STOP_ERROR(BL%LINE,                                                                              &
                                        INFILE=INBAS,OUTPUT=IOUT,MSG='FOUND BAS OPTION  "STARTDATE", BULT FAILED TO PARSE A DATE AFTER KEYWORD.'//NL//  &
                                        'THE ACCEPTED FORMATS ARE:'//NL//           &
                                       'mm/dd/yyyy                                  (where mm = Month, dd = Day, yyyy = four digit year hh = hour in 24 hour format, MM = minute, ss = second, and T is a flag to indicate 24 hour clock time is specified)'//NL//  &
                                       'yyyy-mm-dd'//NL//                           &
                                       'mm/yyyy'//NL//                              &
                                       'mm/dd/yyyyThh:MM:ss'//NL//                  &
                                       'yyyy-mm-ddThh:MM:ss'//NL//                  &
                                       'OR A Decimal Year (e.g. 1979.3232)')
            !
          CASE('LEAPYEARS','LEAPYEAR', 'STARTTIME')
             CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
             !
             IF(LINE(ISTART:ISTOP) == 'LEAPYEARS' .OR. LINE(ISTART:ISTOP) == 'LEAPYEAR' .OR. LINE(ISTART:ISTOP) == 'STARTTIME') THEN
                 USE_LEAP_YR=TRUE
                 CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
             END IF
             !
             CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,REALTIM,MSG='ERROR READING STARTTIME', NO_PARSE_WORD=TRUE)
             REALTIM_PER=REALTIM
             IF(USE_LEAP_YR) CALL STARTING_DATE%INIT(REALTIM)  !DATES CAN BE USED IF DECIMAL YEARS TAKE INTO ACCOUNT LEAP YEARS
          END SELECT
          !
          IF(NO_OPT_LINE) CALL BL%NEXT()
       END DO
       LLOC = K
    END IF
    !
    ! Set up STARTDATE and DATE_SP
    !
    CALL SETUP_STARTDATE(IOUT, STARTING_DATE)
    !
    ! Load all BAS Options
    !
    IF((BL%NAME == 'OPTION' .OR. BL%NAME == 'OPTIONS') .AND. BL%NLINE>Z) THEN
       CALL BL%START()
       DO J=1, BL%NLINE
          IF(  BL%LINE=='ERROR' ) CALL STOP_ERROR(INFILE=INBAS, OUTPUT=IOUT, MSG='UNFORTUNATELY UNKNOWN "'//BL%NAME//'" BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
          !
          IF(NO_OPT_LINE) LLOC=1
          !
          CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
          !
          SELECT CASE (KEY)
          CASE('STARTDATE','START_DATE','DATE_START','DATESTART'); CONTINUE  !Previously loaded
          CASE('LEAPYEARS','LEAPYEAR', 'STARTTIME'              ); CONTINUE  !Previously loaded
          CASE('XSECTION')
                         IXSEC=1
          CASE('CHTOCH')
                         ICHFLG=1
                         !
          CASE('SHOWPROGRESS')  !KEYWORD DOCUMENTED BUT DOES NOTHING IN MF2005 NOR OWHM
                                CONTINUE
          CASE('FREE')
              IFREFM=1
              WRITE(IOUT,'(A )')' THE FREE FORMAT OPTION HAS BEEN SELECTED'
              WRITE(IOUT,'(A/)')' NOTE THIS NOW IS THE DEFAULT AND NOT NECESSARY.'
              !
          CASE('NOFREE')
              IFREFM=Z
              WRITE(IOUT,'(A/)')' THE FREE FORMAT OPTION IS DISABLED'
              !
          CASE('PRINTTIME')
              IPRTIM=1
              WRITE(IOUT,'(A/)')' THE PRINTTIME OPTION HAS BEEN SELECTED'
                           !
          CASE('PRINT_CONVERGENCE') ! OUTER_START NTERM FILE
              !
              WRITE(IOUT,'(A)')' FOUND OPTION "PRINT_CONVERGENCE"'
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS, PRNT_CNVG_NTERM,MSG='FOUND BAS OPTION "PRINT_CONVERGENCE"'//NL//'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PRNT_CNVG_OUTER,MSG='FOUND BAS OPTION "PRINT_CONVERGENCE"'//NL//'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
              !
              CALL PRNT_CNVG%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,NO_INTERNAL=TRUE)
              !
              IF(.NOT. PRNT_CNVG%BINARY)CALL PRNT_CNVG%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL           HEAD     CHNG_HEAD   DATE      CELL_ID')
              !
              ALLOCATE(PRNT_CNVG_LRC(PRNT_CNVG_NTERM), PRNT_CNVG_DIF(PRNT_CNVG_NTERM)  )
              !
          CASE('PRINT_FLOW_RESIDUAL') ! OUTER_START NTERM FILE
              !
              WRITE(IOUT,'(A)')' FOUND OPTION "PRINT_FLOW_RESIDUAL"'
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PRNT_FRES_NTERM,MSG='FOUND BAS OPTION "PRINT_FLOW_RESIDUAL"'//NL//'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PRNT_FRES_OUTER,MSG='FOUND BAS OPTION "PRINT_FLOW_RESIDUAL"'//NL//'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
              !
              CALL PRNT_FRES%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,NO_INTERNAL=TRUE)
              !
              IF(.NOT. PRNT_FRES%BINARY) CALL PRNT_FRES%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL           HEAD    FLOW_RESIDUAL    VOL_RESIDUAL     CELL_VOLUME      DATE  CELL_ID')
              !
              ALLOCATE(PRNT_FRES_LRC(PRNT_FRES_NTERM), PRNT_FRES_DIF(PRNT_FRES_NTERM))
              !
          CASE('PRINT_RELATIVE_VOLUME_ERROR','PRINT_RELATIVE_VOL_ERROR') ! OUTER_START NTERM FILE
              !
              WRITE(IOUT,'(A)') ' FOUND OPTION "PRINT_RELATIVE_VOLUME_ERROR"'
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS, PRNT_VERR_NTERM,MSG='FOUND BAS OPTION "PRINT_RELATIVE_VOLUME_ERROR"'//NL//'BUT FAILED TO LOAD THE PRINT TERM COUNT (NTERM)')
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PRNT_VERR_OUTER,MSG='FOUND BAS OPTION "PRINT_RELATIVE_VOLUME_ERROR"'//NL//'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)')
              !
              CALL PRNT_VERR%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,NO_INTERNAL=TRUE)
              !
              IF(.NOT. PRNT_VERR%BINARY) CALL PRNT_VERR%SET_HEADER('   SP   TS  ITER  LAY  ROW  COL           HEAD  REL_VOL_ERR    VOL_RESIDUAL'//'   FLOW_RESIDUAL  DATE      CELL_ID')
              !
              ALLOCATE(PRNT_VERR_LRC(PRNT_VERR_NTERM), PRNT_VERR_DIF(PRNT_VERR_NTERM)  )
              !
          CASE('MAX_RELATIVE_VOLUME_ERROR','MAX_RELATIVE_VOL_ERROR')
                !
                CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS, MAX_REL_VOL_ERROR,MSG='FOUND BAS OPTION "MAX_RELATIVE_VOL_ERROR"'//NL//'BUT FAILED TO THE NUMBER LOCATED AFTER THE KEYWORD')
                !
          CASE('MIN_SOLVER_ITERATION','MIN_SOLVER_ITER')
              !
              CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
              IF( BL%LINE(ISTART:ISTOP) == 'BY_STRESS_PERIOD') THEN
                  !
                  CALL MIN_ITER_INPUT%OPEN(BL%LINE,LLOC,IOUT,INBAS,NO_INTERNAL=TRUE)
                  I = MIN_ITER_INPUT%IU
                  CALL BL%READ_AND_SET_LINE(I, EOF=FOUND_BEGIN)              !RESUSING VARIABLE FOUND_BEGIN
                  !
                  K = 1
                  CALL GET_INTEGER(BL%LN,K,ISTART,ISTOP,IOUT,I,MIN_SOLVER_INTER_SP, MSG='FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//'WHICH LOADS FROM BY_STRESS_PERIOD FILE BUT FAILED TO LOAD STRESS PERIOD NUMBER')
                  CALL GET_INTEGER(BL%LN,K,ISTART,ISTOP,IOUT,I,MIN_SOLVER_INTER_NEW,MSG='FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//'WHICH LOADS FROM BY_STRESS_PERIOD FILE BUT FAILED TO THE NUMBER LOCATED AFTER STRESS PERIOD NUMBER')
              ELSE
                  CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,MIN_SOLVER_INTER,NO_PARSE_WORD=TRUE,MSG='FOUND BAS OPTION "MIN_SOLVER_ITER"'//NL//'BUT FAILED TO THE NUMBER LOCATED AFTER THE KEYWORD')
              END IF
          !
          CASE('DAMPEN_OSCILLATION', 'DAMPEN_OSCILLATIONS')
              !
              WRITE(IOUT,'(A)')' FOUND OPTION "DAMPEN_OSCILLATION"'
              ALLOCATE(OSCIL_DMP_LRC, OSCIL_DMP_DIF)
              OSCIL_DMP_LRC = Z
              OSCIL_DMP_DIF = DZ
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,OSCIL_DMP_OUTER,MSG='FOUND BAS OPTION "DAMPEN_OSCILLATION"'//NL//'BUT FAILED TO LOAD THE NUMER OF ITERATIONS AT START TO CHECK IF OSCILLATIONS OCCUR.')
              WRITE(IOUT,'(3A)')' HEAD OCCILLATIONS WILL BE DAMPEN BY 50% AFTER ',NUM2STR(OSCIL_DMP_OUTER),' ITERATIONS.'
              ! 
          CASE('DAMPEN_START')
              !
              WRITE(IOUT,'(A)')' FOUND OPTION "DAMPEN_START"'
              !
              DAMPEN_START = TRUE
              DAMPEN_START_ITR = Z
              DAMPEN_START_DMP = DZ
              !
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,DAMPEN_START_ITR,MSG='FOUND BAS OPTION "DAMPEN_START"'//NL//'BUT FAILED TO LOAD THE NUMER OF ITERATIONS AT START TO DAMPEN (NITER)')
              !
              CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,DAMPEN_START_DMP,MSG='FOUND BAS OPTION "DAMPEN_START"'//NL//'BUT FAILED TO LOAD THE DAMPING VALUE (DAMP)')
              !
          CASE('DOUBLE_PRECISION_CBC');  BIN_REAL_KIND = REAL64
              !
          CASE('SHIFT_STRT')
              WRITE(IOUT,'(A/)')' SHIFT_STRT OPTION FOUND. NOW READING WITH ULOAD LIST STYLE NLAY NUMBERS THAT ARE ADDED TO STRT'
              ALLOCATE(HSHIFT(NLAY))
              I = LLOC
              CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)  !CHECK IF THERE IS THE INTERNAL KEYWORD (ULOAD NEEDS A FILE TO READ INTERNAL FROM)
              LLOC = I
              !
              I = Z 
              IF(KEY=='INTERNAL') THEN
                  CALL BL%MAKE_SCRATCH_FILE(.TRUE.)
                  CALL BL%READ_SCRATCH(LINE=LINE)
                  CALL ULOAD(HSHIFT, LLOC, LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH)
              ELSE
                  CALL ULOAD(HSHIFT, LLOC, BL%LINE, BL%IOUT, BL%IU, I, SCRATCH=BL%SCRATCH)
              END IF
              !
              !
              IF(KEY=='INTERNAL') CALL BL%CLOSE_SCRATCH()
              !
          CASE('PROPPRINT')
              WRITE(IOUT,'(A )')' THE PROPPRINT OPTION HAS BEEN SELECTED.'
              WRITE(IOUT,'(A/)')' CHECKING IF FOLDER IS SPECIFIED.'
              !
              LINE = BL%LINE
              CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
              DEALLOCATE(PROPPRINT, STAT=I)
              IF( LINE(ISTART:ISTOP)=='' ) THEN
                                           ALLOCATE(PROPPRINT, SOURCE='./')
              ELSE
                  IF( LINE(ISTOP:ISTOP)=='/'.OR.LINE(ISTOP:ISTOP)=='\' )THEN
                                                                      ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
                  ELSE
                                                                      ISTOP=ISTOP+1
                                                                      LINE(ISTOP:ISTOP)='/'
                                                                      ALLOCATE(PROPPRINT, SOURCE=LINE(ISTART:ISTOP))
                  END IF
              END IF
              WRITE(IOUT,'(/A/,4A/)') 'AQUIFER PROPERTIES AFTER PARAMETERS HAVE BEEN APPLIED WILL BE WRITEN TO SEPARATE FILES.','THESE FILES WILL BE PLACED IN:', '"'//PROPPRINT//'"'
              !
          CASE('HEAD_DISTANCE_ABOVE_GSE_LIMIT','ABOVE_GSE_LIM')
              CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,ABOVE_GSE_LIM,MSG='FOUND BAS OPTION "HEAD_DISTANCE_ABOVE_GSE_LIMIT"'//NL//'BUT FAILED TO LOAD THE ACTUAL LIMIT (ABOVE_GSE_LIM)')
              !
              WRITE(IOUT,'(1x 1A/,1x 3A/,/1x A/,/1x A/,1x A/)') '"HEAD_DISTANCE_ABOVE_GSE_LIMIT" OPTION FOUND.','HEAD VALUES GREATER THAN ', NUM2STR(ABOVE_GSE_LIM), ' L ABOVE THE GROUND SURFACE ELEVATION (GSE) ARE SET TO IT.','IF HEAD IS CHANGED, THEN CONVERGENCE IS NOT ALLOWED.','NOTE THAT IF THE GSE IS NOT SPECIFIED BY THE DIS OR FMP,','     THEN THE TOP OF THE UPPER MOST ACTIVE CELL IS USED.'
              !
          CASE('HEAD_DISTANCE_ABOVE_GSE_PRINT')
           CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,ABOVE_GSE_PRT_LIM,MSG='FOUND BAS OPTION "HEAD_DISTANCE_ABOVE_GSE_PRINT"'//NL//'BUT FAILED TO LOAD THE SHIFT FACTOR (ABOVE_GSE_PRT_LIM)')
              !
              CALL ABOVE_GSE_PRT%OPEN(BL%LINE,LLOC,IOUT,INBAS)
              CALL ABOVE_GSE_PRT%SET_HEADER('SP  TS   LAY ROW  COL   GSE            HEAD           DATE')
              WRITE(IOUT,'(1x 1A/,1x 3A/,1x 2A/)') '"HEAD_DISTANCE_ABOVE_GSE_PRINT" OPTION FOUND.','AFTER TIME STEP CONVERGES ANY HEAD VALUES GREATER THAN ',NUM2STR(ABOVE_GSE_PRT_LIM), ' L', 'ABOVE THE GROUND ','SURFACE ELEVATION (GSE) ARE PRINTED TO A GENERIC_OUTPUT FILE.'
              !
          CASE('DEALLOCATE_MULT')
              DEALLOCATE_MULT = TRUE
              !
          CASE('NOCBC')
              NOCBC=2
              WRITE(IOUT,'(A/)')' NOCBC OPTION SELECTED. NO CBC FLOWS WILL BE WRITTEN.'
              !
          CASE('NOCBCPACK')
              NOCBC=1
              WRITE(IOUT,'(A/)')' NOCBCPACK OPTION SELECTED. ONLY FLOW PACKAGE CBC FLOWS WILL BE WRITTEN.'
          CASE('CBC_EVERY_TIMESTEP')
              NOCBC=-1
              WRITE(IOUT,'(A/)')' CBC_EVERY_TIMESTEP OPTION SELECTED. THE CBC WILL BE WRITTEN TO EVERY TIME STEP.'
          CASE('CBC_LAST_TIMESTEP')
              NOCBC=-1
              WRITE(IOUT,'(A/)')' CBC_EVERY_TIMESTEP OPTION SELECTED. THE CBC WILL BE WRITTEN TO EVERY TIME STEP.'
              !
          CASE('NO_DIM_CHECK', 'NODIMCHECK')
                  WARN_DIM = FALSE
                  WRITE(IOUT,'(2A/)')' NO_DIM_CHECK OPTION FOUND. DELR, DELC,',' AND CELL THICKNESS WILL ONLY CHECK FOR NEGATIVE VALUES.'
          CASE('NO_CONVERGENCE_STOP', 'NO_FAILED_CONVERGENCE_STOP')
                  NO_CONVERGENCE_STOP = TRUE
           WRITE(IOUT,'(A/)') ' NO_CONVERGENCE_STOP OPTION TURNED ON'
          CASE('STOPERROR','STOP_ERROR')
              CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,STOPER,MSG='FOUND BAS OPTION "STOPERROR"'//NL//'BUT FAILED TO LOAD ACCEPTIBLE BUDGET PERCENT ERROR')
              WRITE(IOUT,'(2A/)') ' STOPERROR OPTION TURNED ON WITH MAX ALLOWED BUDGET PERCENT ERROR BEING: ',NUM2STR(STOPER)
              !
          CASE('PERCENTERROR')
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PDIFFPRT,MSG='FOUND BAS OPTION "PERCENTERROR"'//NL//'BUT FAILED TO LOAD ACTUAL PERECENT ERROR VALUE')
              WRITE(IOUT,'(1x,3A/)') 'RATE PERCENT ERROR WILL BE PRINTED FOR EVERY TIME STEP THAT EXCEDES ',NUM2STR(PDIFFPRT),'%'
              !
          CASE('MAXBUDGET')
              CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,MXBUD,MSG='FOUND BAS OPTION "MAXBUDGET"'//NL//'BUT FAILED TO LOAD ACTUAL MAXIMUM BUDGET VALUE (MXBUD)')
              WRITE(IOUT,'(1x 2A/)')'MAXBUDGET OPTION SET. THE MAXIMUM NUMBER OF BUDGET ENTRIES IS CHANGED FROM 100 TO', NUM2STR(MXBUD)
              !
          CASE('MAXPARAM')
              WRITE(IOUT,'(3(/,1x,A)/)') 'MAXPARAMETER ARRAY STORAGE OPTION SELECTED (MAXPARAM)',  &
                                         'NOW READING IN MXPAR, MXCLST, and MXINST',               &
                                         'THESE VALUSE MUST BE >0, SET TO 1 IF NOT USED'
              !
           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,MXPAR ,MSG='FOUND BAS OPTION "MAXPARAM"'//NL//'BUT FAILED TO LOAD MXPAR')
           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,MXCLST,MSG='FOUND BAS OPTION "MAXPARAM"'//NL//'BUT FAILED TO LOAD MXCLST')
           CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,MXINST,MSG='FOUND BAS OPTION "MAXPARAM"'//NL//'BUT FAILED TO LOAD MXINST')
              IF(MXPAR<1)THEN
                  CALL WRN%ADD(BLN//BL%LINE//BLN//'MXPAR<1; VALUE CHANGED TO MXPAR=1'//BLN)
                  MXPAR=1
              END IF
              IF(MXCLST<1)THEN
                  CALL WRN%ADD(BLN//BL%LINE//BLN//'MXCLST<1; VALUE CHANGED TO MXCLST=1'//BLN)
                  MXCLST=1
              END IF
              IF(MXINST<1)THEN
                  CALL WRN%ADD(BLN//BL%LINE//BLN//'MXINST<1; VALUE CHANGED TO MXINST=1'//BLN)
                  MXINST=1
              END IF
              !
          CASE('RESIDUAL_ERROR_ARRAY_THRESHOLD')
              CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,PRNT_RES_LIM,MSG='FAILED TO LOAD INTEGER AFTER RESIDUAL_ERROR_ARRAY_THRESHOLD KEYWORD. THIS INTEGER REPRESENTS THE PERCENT ERROR THRESHOLD BEFORE PRINTING RESIDUAL_ERROR_ARRAY')
              !
          CASE('RESIDUAL_ERROR_ARRAY')
              CALL PRNT_RES%OPEN(BL%LINE,LLOC,IOUT,INBAS)
              WRITE(IOUT,'(1x,2A/)')'RESIDUAL_ERROR_ARRAY OPTION SET. NLAY 2D ARRAYS OF FLOW RESIDUAL ERRORS WILL BE WRITTEN TO FILE UNIT: ', NUM2STR(PRNT_RES%IU)
              !
          CASE('CUMULATIVE_RESIDUAL_ERROR_ARRAY')
              CALL PRNT_RES_CUM%OPEN(BL%LINE,LLOC,IOUT,INBAS)
              ALLOCATE(PRNT_RES_CUM_ARR(NCOL,NROW,NLAY), SOURCE=DZ)
              WRITE(IOUT,'(1x,2A/)')'CUMULATIVE_RESIDUAL_ERROR_ARRAY OPTION SET. A CUMULATIVE 2D ARRAY OF FLOW RESIDUAL VOLUME ERRORS WILL BE WRITTEN TO FILE UNIT: ', NUM2STR(PRNT_RES_CUM%IU)
              !
          CASE('ITERATION_INFO','INTERATION_INFO')
              CALL INTER_INFO%OPEN(BL%LINE,LLOC,IOUT,INBAS,SPLITMAXCOUNT=Z)
              LINE = '    SP    TS  ITER  TS_LENGTH       VOLUME_ERROR   RATE_ERROR     RAT_PERROR'
              !
              IF(HAS_STARTDATE) LINE = TRIM(LINE)//'     DATE'
              !
              CALL INTER_INFO%SET_HEADER(TRIM(LINE))
              !CALL INTER_INFO%SET_HEADER('    SP    TS  ITER  TS_LENGTH      VOLUME_ERROR   RATE_ERROR     RAT_PERROR     DATE')
          CASE('BUDGETDB')
              CALL BUDGETDB%OPEN(BL%LINE,LLOC,IOUT,INBAS, SPLITMAXCOUNT=Z)
              WRITE(IOUT,'(1x,2A/)')'BUDGET DATABASE OPTION SET. A DATABASE FRIENDLY FILE WILL BE WRITTEN TO FILE UNIT: ', NUM2STR(BUDGETDB%IU)
              !
          CASE('TIME_INFO')
              CALL TIME_INFO%OPEN(BL%LINE,LLOC,IOUT,INBAS,NOBINARY=TRUE)
              WRITE(IOUT,'(1x,2A/)')'TIME_INFO OPTION FOUND, TIME STEP TIME INFORMATION WILL BE WRITTEN TO FILE UNIT: ', NUM2STR(TIME_INFO%IU)
              !
          CASE('SUPER_NAMES', 'SUPERNAMES')
              CALL SUPER_NAMES_IN%OPEN(BL%LINE,LLOC,IOUT,INBAS)
              WRITE(IOUT,'(1x,A/)')'SUPER_NAMES OPTION FOUND, WILL LOAD A LIST OF GLOBAL NAMES THAT CAN BE USED IN OTHER PACKAGES'
              !
          CASE('INPUT_CHECK', 'INPUTCHECK')
              SPSTART = NPER+1
              SPEND   = SPSTART
              INPUT_CHECK = TRUE
              !
          CASE('PAUSE')
              USE_PAUSE = TRUE
              !
          CASE('FASTFORWARD')
              !
              CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP) !STARTING DATE
              IF( IS_INTEGER(BL%LINE(ISTART:ISTOP)) ) THEN
                  !
                  CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,SPSTART,TRUE,MSG='FASTFORWARD ERROR -- '//'FAILED TO IDENTIFY STARTING STRESS PERIOD')
              ELSEIF( .NOT. HAS_STARTDATE ) THEN
                  CALL STOP_ERROR(BL%LINE,INBAS,IOUT,                                                                          &
                                            'FASTFORWARD ERROR -- FAILED TO IDENTIFY A STARTING STRESSS PERIOD No.'//NL//                &
                                            'AND THERE IS NOT A STARTING DATE SPECIFIED IN THE DIS (viz. "STARTDATE" KEYWORD)'//NL//     &
                                            'TO MAKE OneWater DATE AWARE'//NL//'AND ALLOW THE USE OF CALENDAR DATES AS AN INPUT TO THE FASTWORD FEATURE')
              ELSE
                  CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
                  IF(  DATE%NOT_SET() ) CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,'FASTFORWARD ERROR -- FAILED TO IDENTIFY STARTING STRESS PERIOD No.'//NL//'OR STARTING CALENDAR DATE'//NL//'FOR FASTFORWARD TO IDENTIFY STARTING POINT.')
                  !
                  CALL DATE_TO_SP(DATE,SPSTART)
              END IF
              !
              CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP) !ENDING DATE
              IF( IS_INTEGER(BL%LINE(ISTART:ISTOP)) ) THEN
                  !
                  CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,SPEND,TRUE,MSG='FASTFORWARD ERROR -- '//'FAILED TO IDENTIFY ENDING STRESS PERIOD')
              ELSE
                  CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
                  IF(  DATE%IS_SET() ) THEN
                      CALL DATE_TO_SP(DATE,SPEND)
                  ELSE
                      SPEND = NPER
                      CALL WARNING_MESSAGE(BL%LINE,INBAS,IOUT,                                                                     &
                                             'MINOR WARNING: FASTFORWARD -- FAILED TO IDENTIFY AN ENDING STRESSS PERIOD No.'//NL//   &
                                             'NOR AN ENDING CALENDAR DATE'//NL//                                                     &
                                             'SO IT WILL BE ASSUMED TO HAVE AN ENDING AT STRESSS PERIOD AT NPER')
                  END IF
              END IF
              WRITE(IOUT,'(/3A,//33x 2A,/33x 2A,//A/)')                            &
                    REPEAT('#',35),                                                &
                    '...FASTFORWARD OPTION TURNED ON...',REPEAT('#',35),           &
                    '     THE STARTING STRESS PERIOD WILL BE: ', NUM2STR(SPSTART), &
                    '     THE ENDING   STRESS PERIOD WILL BE: ', NUM2STR(SPEND),   &
                    REPEAT('#',104)
          CASE DEFAULT
                     IF(HAS_OPT_LINE .AND. KEY == BLNK) EXIT  !Reached end of old Options Line
                     !
                     CALL WRN%ADD(BL%LINE//BLN)
          END SELECT
          !
          IF(NO_OPT_LINE) CALL BL%NEXT()
       END DO
       !
       IF (WRN%RAISED) CALL WRN%CHECK('BAS OPTION WARNING.'//NL//                                                  &
                                      'FAILED TO APPLY SOME OF THE OPTIONS FOUND WITHIN THE OPTIONS BLOCK.'//NL//  &
                                      'THEY WERE NOT RECONIZED AS ONE OF THE BAS PACKAGE OPTIONS.'//NL//           &
                                      'THE FOLLOWING ARE THE OPTIONS THAT WILL BE IGNORED:',                       &
                                      INBAS,IOUT)
    END IF
    !
    IF(NO_CONVERGENCE_STOP) STOPER = 1E30 !SET TO VERY LARGE VALUE SO CODE NEVER STOPS
  END SUBROUTINE
  !  
  SUBROUTINE SETUP_STARTDATE(IOUT, STARTING_DATE)
    INTEGER,             INTENT(IN   ):: IOUT
    TYPE(DATE_OPERATOR), INTENT(INOUT):: STARTING_DATE
    INTEGER:: I, N
    !
    IF (STARTING_DATE%NOT_SET()) THEN
         ALLOCATE(DATE_SP(1))
         ALLOCATE(DATE_SP(1)%TS(0:0))
         CALL DATE_SP(1)%TS(Z)%INIT()
    END IF
    !
    IF(STARTING_DATE%IS_SET()) THEN
          !
          HAS_STARTDATE = TRUE
          USE_LEAP_YR=TRUE
          REALTIM=STARTING_DATE%DYEAR
          REALTIM_PER=REALTIM
          !
          WRITE(IOUT,'(/A,/15x A,/A,A)')'   "STARTDATE" OPTION ACTIVATED',                                                                                          &
                                        'A CALENDAR DATE WILL BE USED FOR TIME TRACKING AND DECIMAL YEAR CALCULATION AND PROPAGATED ALONG WITH THE SIMULATED TIME', &
                                        'WITH A STARTING DATE AND TIME OF ', STARTING_DATE%STR(' ')
          WRITE(IOUT,'(/A)') 'THE DECIMAL YEAR CALCULATION WILL TAKE INTO ACCOUNT LEAP YEARS AND MAKE A CORRECTION FOR THEM.'
          !REALTIM=STARTING_DATE%DYEAR
          !
          IF(ITMUNI==5)  CALL STOP_ERROR(OUTPUT=IOUT,MSG='DIS ERROR: "STARTDATE" Option does not work with time units of years (ITMUNI=5).'//NL//'Instead use the "STARTTIME" option followed a starting decimal year. For example,'//NL//'"STARTIME '//NUM2STR(STARTING_DATE%DYEAR)//'"')
          !
          ALLOCATE(DATE_SP(NPER))
          DO N=1,NPER
              ALLOCATE(DATE_SP(N)%TS( 0:NSTP(N) ))
          END DO
          !
          N = 1
          DATE_SP(N)%TS(Z) = STARTING_DATE
          DO I=1, NSTP(N)-1
              DATE_SP(N)%TS(I) = DATE_SP(N)%TS(I-1) + DELT_TO_DAY(SPTIM(N)%DT(I),ITMUNI)
          END DO
          I = NSTP(N)
          DATE_SP(N)%TS(I) = DATE_SP(N)%TS(Z) + DELT_TO_DAY(PERLEN(N),ITMUNI)
          !
          DO N=2,NPER
            DATE_SP(N)%TS(Z) = DATE_SP(N-1)%TS( NSTP(N-1) )
            DO I=1, NSTP(N)-1
                DATE_SP(N)%TS(I) = DATE_SP(N)%TS(I-1) + DELT_TO_DAY(SPTIM(N)%DT(I),ITMUNI)
            END DO
                I = NSTP(N)
                DATE_SP(N)%TS(I) = DATE_SP(N)%TS(Z) + DELT_TO_DAY(PERLEN(N),ITMUNI)
          END DO
          !
    ELSEIF(REALTIM.GE.0D0)THEN
          WRITE(IOUT,'(/A,/15x A,/A,F9.4)')'   "STARTTIME" OPTION ACTIVATED',                                                    &
                                        'A DECIMAL YEAR CALCULATION WILL BE MADE AND PROPAGATED ALONG WITH THE SIMULATED TIME',  &
                                        'WITH A STARTING DECIMAL YEAR OF ', REALTIM
          IF(USE_LEAP_YR)THEN
                         WRITE(IOUT,'(/A)') 'THE DECIMAL YEAR CALCULATION WILL TAKE INTO ACCOUNT LEAP YEARS AND MAKE A CORRECTION FOR THEM.'
          ELSE
            WRITE(IOUT,'(/A,//2A/)') 'THE DECIMAL YEAR CALCULATION ASSUMES THERE ARE 365.2425 DAYS IN A YEAR.',                       &
                                     '   NOTE THAT THIS NEGATES ANY CALENDAR DATE FEATURES BECAUSE LEAP YEARS ARE NOT ACCOUNTED FOR.'
          END IF
    END IF
    !
  END SUBROUTINE
END MODULE

MODULE MNW2_OUPUT!, ONLY: PRNT_MNW2_SUB, PRNT_MNW2_INOUT_SUB, PRNT_MNW2_NODE_SUB
  USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN 
  USE CONSTANTS
  USE GENERIC_OUTPUT_FILE_INSTRUCTION
  !USE GLOBAL,       ONLY:NCOL,NROW,NLAY,NBOTM,LBOTM,BOTM,IBOUND,HNEW,LAYHDT
  USE GWFBASMODULE, ONLY:DELT
  USE NUM2STR_INTERFACE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: PRNT_MNW2_SUB, PRNT_MNW2_INOUT_SUB, PRNT_MNW2_NODE_SUB
  !
  CONTAINS
  !
  SUBROUTINE PRNT_MNW2_SUB(FL, DATE, PER, STP, NMNWVL, MNWMAX, NODTOT, MXNODE, LW, MNW2, MNWNOD, WELLID)
    TYPE(GENERIC_OUTPUT_FILE),                   INTENT(IN):: FL
    CHARACTER(19),                               INTENT(IN):: DATE
    INTEGER,                                     INTENT(IN):: PER, STP, NMNWVL,MNWMAX, NODTOT, MXNODE, LW
    DOUBLE PRECISION,  DIMENSION(NMNWVL,MNWMAX), INTENT(IN):: MNW2
    DOUBLE PRECISION,  DIMENSION(    34,NODTOT), INTENT(IN):: MNWNOD
    CHARACTER(LEN=20), DIMENSION(     MNWMAX+1), INTENT(IN):: WELLID
    !
    INTEGER:: I, J, FIRST_NODE, LAST_NODE
    CHARACTER(15):: DT, ZER
    DOUBLE PRECISION :: Qini, Q, Hw, NaN
    !
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = 'NaN'; ZER = ADJUSTR(ZER)
    NaN = IEEE_VALUE(Q, IEEE_QUIET_NAN)
    !
    DO I=ONE, MNWMAX
       !
       FIRST_NODE = NINT( MNW2(4,I) )
       !
       IF(MNW2(1,I) < 0.5D0) THEN
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I), NaN, NaN, NaN
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP,   DT, WELLID(I)(:LW), ZER, ZER, ZER
           END IF
       ELSE
           LAST_NODE = NINT( MNW2(4,I) + ABS(MNW2(2,I)) - UNO )
           !
           Qini = MNW2(5 ,I)
           Hw   = MNW2(17,I)
           !
           Q = DZ
           DO J=FIRST_NODE, LAST_NODE;  Q = Q + MNWNOD(4,J)
           END DO
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I), Qini, Q, Hw
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP, DT, WELLID(I)(:LW), NUM2STR(Qini), NUM2STR(Q), NUM2STR(Hw)
           END IF
       END IF
    END DO
    !
  END SUBROUTINE
  !
  SUBROUTINE PRNT_MNW2_NODE_SUB(FL, DATE, PER, STP, NMNWVL, MNWMAX, NODTOT, MXNODE, LW, MNW2, MNWNOD, WELLID)
    TYPE(GENERIC_OUTPUT_FILE),                   INTENT(IN):: FL
    CHARACTER(19),                               INTENT(IN):: DATE
    INTEGER,                                     INTENT(IN):: PER, STP, NMNWVL,MNWMAX, NODTOT, MXNODE, LW
    DOUBLE PRECISION,  DIMENSION(NMNWVL,MNWMAX), INTENT(IN):: MNW2
    DOUBLE PRECISION,  DIMENSION(    34,NODTOT), INTENT(IN):: MNWNOD
    CHARACTER(LEN=20), DIMENSION(     MNWMAX+1), INTENT(IN):: WELLID
    !
    INTEGER:: I, J, FIRST_NODE, LAST_NODE, INOD
    CHARACTER(15):: DT, ZER, NaNc
    DOUBLE PRECISION :: Qini, Q, NaN
    !
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = '0.0'; ZER = ADJUSTR(ZER)
    NaNc= 'NaN'; NaNc= ADJUSTR(ZER)
    NaN = IEEE_VALUE(Q, IEEE_QUIET_NAN)
    ! 
    !        'WELLID   PUMPING_RATE_INI '// SEQ2STR('NOD_', MXNOD,20,PAD=I)
    DO I=ONE, MNWMAX
       !
       FIRST_NODE = NINT( MNW2(4,I) )
       !
       IF(MNW2(1,I) < 0.5D0) THEN
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I),  NaN, ( NaN, J=1, MXNODE)
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP, DT,   WELLID(I)(:LW), NaNc, (NaNc, J=1, MXNODE)
           END IF
       ELSE
           LAST_NODE = NINT( MNW2(4,I) + ABS(MNW2(2,I)) - UNO )
           !
           Qini = MNW2(5 ,I)
           !
           INOD = LAST_NODE - FIRST_NODE + TWO
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I), Qini, (MNWNOD(4,J), J=FIRST_NODE, LAST_NODE), (DZ, J=INOD, MXNODE)
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP, DT, WELLID(I)(:LW), NUM2STR(Qini), (NUM2STR(MNWNOD(4,J)), J=FIRST_NODE, LAST_NODE), (ZER, J=INOD, MXNODE)
           END IF
       END IF
    END DO
    !
   ! MNW2(NMNWVL,MNWMAX)
   ! MNWNOD(34,NODTOT)
    !
  END SUBROUTINE
  !
  SUBROUTINE PRNT_MNW2_INOUT_SUB(FL, DATE, PER, STP, NMNWVL, MNWMAX, NODTOT, MXNODE, LW, MNW2, MNWNOD, WELLID)
    TYPE(GENERIC_OUTPUT_FILE),                   INTENT(IN):: FL
    CHARACTER(19),                               INTENT(IN):: DATE
    INTEGER,                                     INTENT(IN):: PER, STP, NMNWVL,MNWMAX, NODTOT, MXNODE, LW
    DOUBLE PRECISION,  DIMENSION(NMNWVL,MNWMAX), INTENT(IN):: MNW2
    DOUBLE PRECISION,  DIMENSION(    34,NODTOT), INTENT(IN):: MNWNOD
    CHARACTER(LEN=20), DIMENSION(     MNWMAX+1), INTENT(IN):: WELLID
    !
    INTEGER:: I, J, FIRST_NODE, LAST_NODE
    CHARACTER(15):: DT, ZER
    DOUBLE PRECISION :: Qini, Q, Qin, Qout, Hw, NaN
    !
    !
    DT = NUM2STR(DELT)
    DT = ADJUSTR(DT)
    ZER = 'NaN'; ZER = ADJUSTR(ZER)
    NaN = IEEE_VALUE(Q, IEEE_QUIET_NAN)
    !
    DO I=ONE, MNWMAX
       !
       FIRST_NODE = NINT( MNW2(4,I) )
       !
       IF(MNW2(1,I) < 0.5D0) THEN
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I), NaN, NaN, NaN, NaN, NaN
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP,   DT, WELLID(I)(:LW), ZER, ZER, ZER, ZER, ZER
           END IF
       ELSE
           LAST_NODE = NINT( MNW2(4,I) + ABS(MNW2(2,I)) - UNO )
           !
           Qini = MNW2(5 ,I)
           Hw   = MNW2(17,I)
           !
           Q    = DZ
           Qin  = DZ
           Qout = DZ
           DO J=FIRST_NODE, LAST_NODE;  
                                      Q = Q + MNWNOD(4,J)
                                      !
                                      IF(MNWNOD(4,J) > DZ) THEN
                                          Qin  =  Qin + MNWNOD(4,J)
                                      ELSE
                                          Qout = Qout - MNWNOD(4,J)
                                      END IF
           END DO
           !
           IF(FL%BINARY) THEN
               WRITE(FL%IU                   ) DATE, PER, STP, DELT, WELLID(I), Qini, Q, Qin, Qout, Hw
           ELSE
               WRITE(FL%IU,'(A, 2I7, A, 2x A, *(2x A15))') DATE, PER, STP, DT, WELLID(I)(:LW), NUM2STR(Qini), NUM2STR(Q), NUM2STR(Qin), NUM2STR(Qout), NUM2STR(Hw)
           END IF
       END IF
    END DO
    !
  END SUBROUTINE
END MODULE
!
MODULE MNW2_FUNCT
  USE CONSTANTS, ONLY: DZ, UNO, NEARZERO_15
  IMPLICIT NONE
  PUBLIC:: MNW2_COMPOSITE_HEAD
  PRIVATE
  CONTAINS
  !
  PURE FUNCTION MNW2_COMPOSITE_HEAD(IW, MNW2, MNWNOD, HNEW, HDRY) RESULT(H)
    INTEGER,                                 INTENT(IN):: IW
    DOUBLE PRECISION, DIMENSION(:,:  ), CONTIGUOUS, INTENT(IN):: MNW2, MNWNOD
    DOUBLE PRECISION, DIMENSION(:,:,:), CONTIGUOUS, INTENT(IN):: HNEW
    DOUBLE PRECISION,                     OPTIONAL, INTENT(IN):: HDRY
    DOUBLE PRECISION:: H
    !
    INTEGER:: I, IL, IR, IC, F, L
    DOUBLE PRECISION:: csum
    !
    F = NINT( MNW2(4,IW) )
    L = NINT( MNW2(4,IW) + ABS(MNW2(2,IW)) - UNO )
    !
    csum = DZ
    DO I=F, L
        IF(MNWNOD(14,I) > NEARZERO_15) csum  = csum  + MNWNOD(14,I)
    END DO
    !
    IF (csum > NEARZERO_15) THEN
        csum = UNO/csum
        H = DZ
        DO I=F, L
            IF(MNWNOD(14,I) > NEARZERO_15) THEN
                  IL = NINT( MNWNOD(1,I) )             
                  IR = NINT( MNWNOD(2,I) )             
                  IC = NINT( MNWNOD(3,I) )
                  !
                  H  = H + HNEW(IC,IR,IL)*MNWNOD(14,I)*csum
            END IF
        END DO
    ELSE
        IF (PRESENT(HDRY)) THEN
            H = HDRY
        ELSE
            H = -1E30
        END IF
    END IF  
  END FUNCTION
END MODULE
!        CASE('ADVANCED_DAMPING') ! BY_STRESS_PERIOD InputFile or OUTER_START
!           WRITE(IOUT,'(2A,/A)')' FOUND OPTION "ADVANCED_DAMPING" ',
!     +     'NOW LOUDING DAMPING DIFFERENCE TOLERANCE,',
!     +     'THEN STARTING OUTER ITERATION OR KEYWORD "BY_STRESS_PERIOD"'
!           CALL GET_NUMBER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!     +     BAS_ADAMP_TOL,MSG='FOUND BAS OPTION "ADVANCED_DAMPING"'//NL//
!     + 'BUT FAILED TO LOAD THE ADVANCED DAMPING HEAD DIFFERENCE '//
!     + 'TOLERANCE (BAS_ADAMP_TOL)')
!
!            CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
!            IF( BL%LINE(ISTART:ISTOP) == 'BY_STRESS_PERIOD') THEN
!                 CALL ADAMP_INPUT%OPEN(BL%LINE,LLOC,IOUT,INBAS,
!     +                                               NO_INTERNAL=TRUE)
!            ELSE
!                 CALL GET_INTEGER(BL%LINE,LLOC,ISTART,ISTOP,IOUT,INBAS,
!     +   BAS_ADAMP,NO_PARSE_WORD=TRUE,
!     +   MSG='FOUND BAS OPTION "ADVANCED_DAMPING"'//NL//
!     + 'BUT FAILED TO LOAD THE STARTING OUTER ITERATION (OUTER_START)'//
!     + 'OR KEYWORD "BY_STRESS_PERIOD"')
!                 IF( BAS_ADAMP < 24 ) BAS_ADAMP = 24
!            END IF
!            ALLOCATE(HED_CHNG2(NCOL,NROW,NLAY))
!            ALLOCATE(HED_CHNG3(NCOL,NROW,NLAY))
!            ALLOCATE(HED_LOCK(NCOL,NROW,NLAY), SOURCE=Z)
!            SELECT CASE(LENUNI)
!            CASE(1  ); BAS_ADAMP_TOL2 =  3.28D0 !ft
!            CASE(0,2); BAS_ADAMP_TOL2 =  1.00D0 ! m
!            CASE(3  ); BAS_ADAMP_TOL2 =  1.00D2 !cm
!            END SELECT
    
    
!        CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,INBAS)
!        !
!        READ(BL%LINE(ISTART:ISTOP), INTFMT(BL%LINE(ISTART:ISTOP)),
!     +       IOSTAT=IERR) SPSTART
!        !
!        IF (IERR.NE.0) THEN
!          CALL DATE%INIT( BL%LINE(ISTART:ISTOP), 0.001D0 )
!          IF (DATE%NOT_SET()) CALL STOP_ERROR(
!     +            TRIM(BL%LINE),INBAS,IOUT,'FASTFORWARD ERROR -- '//
!     +            'FAILED TO IDENTIFY STARTING STRESS PERIOD')
!      IF( DATE_SP(1)%TS(0)%NOT_SET() .AND. DATE%IS_SET() )    !PARSED DATE FROM STRING BUT NO STARTING DATE
!     +    CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,
!     +    'FASTFORWARD ERROR -- START DATE SPECIFIED '//
!     +    'BUT DIS DOES NOT HAVE THE "STARTDATE" KEYWORD TO '//
!     +    'MAKE OneWater DATE AWARE')
!          !
!          CALL DATE_TO_SP(DATE,SPSTART)
!          !!
!          !N = UBOUND( DATE_SP(1)%TS,1 )
!          !IF     ( DATE <  DATE_SP(1)%TS(N) ) THEN
!          !                                          SPSTART=1
!          !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
!          !                                          SPSTART=NPER
!          !ELSE
!          ! DO I=2, NPER
!          !  N = UBOUND( DATE_SP(I)%TS,1 )
!          !  IF( DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
!          !                                          SPSTART=I
!          !                                          EXIT
!          !  END IF
!          ! END DO
!          !END IF
!        END IF
!        !
!        CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
!        READ(BL%LINE(ISTART:ISTOP),INTFMT(BL%LINE(ISTART:ISTOP)),
!     +       IOSTAT=IERR)I
!        CALL DATE%INIT( BL%LINE(ISTART:ISTOP) )   !, 0.01D0
!        !
!      IF( DATE_SP(1)%TS(0)%NOT_SET() .AND. DATE%IS_SET() )    !DATE%DAY = -999 IF FAILED TO PARSE DATE FROM STRING
!     +  CALL STOP_ERROR(TRIM(BL%LINE),INBAS,IOUT,
!     +  'FASTFORWARD ERROR -- END DATE SPECIFIED '//
!     +  'BUT DIS DOES NOT HAVE THE "STARTDATE" KEYWORD TO '//
!     +  'MAKE OneWater DATE AWARE')
!        !
!        IF(IERR==0 .AND. BL%LINE(ISTART:ISTOP).NE.' ' ) THEN         ! FORTRAN SETS ' ' TO ZERO ON READ
!                                                    SPEND=I
!        ELSEIF ( LLOC==LEN(BL%LINE) ) THEN                           !NOTHING AT THE END OF THE LINE
!                                                    SPEND=NPER
!        ELSE
!                                             CALL DATE_TO_SP(DATE,SPEND)
!                                             IF(SPEND<0) SPEND=NPER
!        END IF
!        !
!        !N = UBOUND( DATE_SP(1)%TS,1 )
!        !IF(IERR==0 .AND. LINE(ISTART:ISTOP).NE.' ' ) THEN               ! FORTRAN SETS ' ' TO ZERO ON READ
!        !                                            SPEND=I
!        !ELSEIF ( DATE%DAY==-999.OR. LLOC==LEN(LINE) ) THEN
!        !                                            SPEND=NPER
!        !ELSEIF ( DATE <  DATE_SP(1)%TS(N) ) THEN
!        !                                            SPEND=1
!        !ELSEIF ( DATE >= DATE_SP(NPER)%TS(0) ) THEN
!        !                                            SPEND=NPER
!        !ELSE  !IERR.NE.0
!        !   DO I=1, NPER
!        !     N = UBOUND( DATE_SP(I)%TS,1 )
!        !     IF(DATE_SP(I)%TS(0)<=DATE .AND. DATE<DATE_SP(I)%TS(N) )THEN
!        !                                             SPEND=I
!        !                                             EXIT
!        !     END IF
!        !   END DO
!        !END IF
!        !
    
    
    
    
          !
!        CASE('LISTSPLIT')                   !seb ADD NEW OPTION TO SPECIFY PRINT OUT OF ERRORS THAT EXCEDE A MINIMUM PRECENT
!             CALL URWORD(BL%LINE,LLOC,ISTART,ISTOP,2,N,R,IOUT,INBAS)
!             LISTSPLIT%MAXSIZE = N
!             WRITE(IOUT,'(1x,4A)')
!     +         'LIST WILL BE SPLIT TO A NEW FILE WHEN FILE SIZE IS ',
!     +           'APPROXIMATELY ',NUM2STR(N),' MB'