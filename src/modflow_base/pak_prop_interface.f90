MODULE PAK_PROP_INTERFACE
  USE CONSTANTS,           ONLY: Z, ONE, TRUE, FALSE, NL
  USE UTIL_INTERFACE,      ONLY: READ_TO_DATA, FILE_IO_ERROR, STOP_ERROR, &
                                 UPPER, PARSE_WORD_UP, PARSE_WORD, COMMENT_INDEX,   &
                                 GET_INTEGER, GET_NUMBER, GET_WARN
  USE NUM2STR_INTERFACE,   ONLY: NUM2STR
  USE GENERIC_OPEN_INTERFACE
  IMPLICIT NONE
  !
  PRIVATE:: Z, ONE, TRUE, FALSE, NL
  PRIVATE:: PAK_PROP_TRANSFER, PAK_PROPTAB_TRANSFER
  PRIVATE:: READ_TO_DATA,      FILE_IO_ERROR,       STOP_ERROR
  PRIVATE:: NUM2STR,           UPPER,               GENERIC_OPEN
  PRIVATE:: PARSE_WORD,        GET_INTEGER,         GET_NUMBER,         COMMENT_INDEX,  PARSE_WORD_UP
  !
  TYPE PAK_PROP
    INTEGER::LAY,ROW,COL                              !BNDS(5+1+NAUX,MXBND) BND(6)=CBCFLOW, BND(1:3)=[LAY,ROW,COL]
    REAL   :: CBCFLOW
    REAL,   DIMENSION(:),ALLOCATABLE:: VAL
    INTEGER,DIMENSION(:),ALLOCATABLE:: AUX
    CHARACTER(16)       ,ALLOCATABLE:: BUDGET_GROUP != '           WELLS'
  END TYPE
  !
  TYPE, EXTENDS (PAK_PROP)::PAK_PROPTAB
    CHARACTER(:),    ALLOCATABLE:: TABEQN  
    DOUBLE PRECISION,ALLOCATABLE:: TABEQNRES
    CHARACTER(20),   ALLOCATABLE:: TABNAM
    REAL,            ALLOCATABLE:: TSFAC
    INTEGER:: TABIDX = Z
  END TYPE PAK_PROPTAB
  !
  !INTERFACE ASSIGNMENT (=)
  !    MODULE PROCEDURE PAK_PROP_TRANSFER
  !    MODULE PROCEDURE PAK_PROPTAB_TRANSFER
  !END INTERFACE
  !
CONTAINS
!
!SUBROUTINE THAT TRANSFERS INFORMATION FROM ONE PAK_PROP/PAK_PROPTAB TYPE TO ANOTHER.
!THIS IS IMPLICITLY DONE WITH AN EQUAL SIGN DO TO INTERFACE ASSIGNMENT
SUBROUTINE  PAK_PROP_TRANSFER(OUTVAR,INVAR)
  TYPE(PAK_PROP),INTENT(INOUT)::OUTVAR
  TYPE(PAK_PROP),INTENT(IN   )::       INVAR
  !
  OUTVAR%LAY=INVAR%LAY
  OUTVAR%ROW=INVAR%ROW
  OUTVAR%COL=INVAR%COL
  OUTVAR%CBCFLOW=INVAR%CBCFLOW
  !IF( SIZE(OUTVAR%VAL)/=SIZE(INVAR%VAL)) THEN
  !      IF(ALLOCATED(OUTVAR%VAL)         ) DEALLOCATE(OUTVAR%VAL         )
  !      IF(ALLOCATED(OUTVAR%AUX)         ) DEALLOCATE(OUTVAR%AUX         )
  !      IF(ALLOCATED(OUTVAR%BUDGET_GROUP)) DEALLOCATE(OUTVAR%BUDGET_GROUP)
  !      ALLOCATE(OUTVAR%VAL         , MOLD = )
  !      ALLOCATE(OUTVAR%AUX         , MOLD = )
  !      ALLOCATE(OUTVAR%BUDGET_GROUP, MOLD = )
  !END IF
  !
  IF(ALLOCATED(INVAR%VAL)) OUTVAR%VAL=INVAR%VAL
  IF(ALLOCATED(INVAR%AUX)) OUTVAR%AUX=INVAR%AUX
  !
  IF(ALLOCATED(INVAR%BUDGET_GROUP)) OUTVAR%BUDGET_GROUP=INVAR%BUDGET_GROUP
  !
END SUBROUTINE
!
SUBROUTINE  PAK_PROPTAB_TRANSFER(OUTVAR,INVAR)
  TYPE(PAK_PROPTAB),INTENT(INOUT)::OUTVAR
  TYPE(PAK_PROPTAB),INTENT(IN   )::INVAR
  !
  CALL PAK_PROP_TRANSFER( OUTVAR%PAK_PROP, INVAR%PAK_PROP )
  !
  IF( ALLOCATED(OUTVAR%TABNAM )) THEN
    OUTVAR%TABNAM=INVAR%TABNAM
    OUTVAR%TABIDX=INVAR%TABIDX
    OUTVAR%TSFAC =INVAR%TSFAC 
        IF(ALLOCATED(OUTVAR%TABEQNRES)) THEN
                     OUTVAR%TABEQN=INVAR%TABEQN
        END IF
  END IF
  !OUTVAR%PAK_PROP=INVAR%PAK_PROP                                      !THIS COPYS OVER THE PAK_PROP PORTION OF THE DATA TYPE
  !OUTVAR%TABNAM=INVAR%TABNAM
  !OUTVAR%TABIDX=INVAR%TABIDX
  !OUTVAR%TSFAC =INVAR%TSFAC 
  !
END SUBROUTINE
!
SUBROUTINE ULSTRDSTRUCT(NLST,RLST,LSTBEG,LDIM,MXLST,INPACK,IOUT,          &
                        LABEL,CAUX,NCAUX,NAUX,IFREFM,ISCLOC1,ISCLOC2,IPRFLG)
!     ******************************************************************
!     Read and print a list.  NAUX of the values in the list are
!     optional -- auxiliary data. This assumes structure shape of array
! NLST:   NUMBER OF ENTRIES TO BE READ IN
! RLST:   STRUCTURE THAT IS A CLASS OF PAK_PROP THAT WILL READ IN DATA
! LSTBEG: FIRST LOCATION IN RLIST THAT DATA IS READ INTO
! LDIM:   LEADING DIMENSION OF VALUE ARRAY (%VAL) IN RLIST  (i.e. RLIST%VAL(LDIM))
! MXLIST: SIZE OF RLIST (i.e. RLIST(MXLIST))
! INPACK: UNIT NUMBER OF INPUT FILE/PACKAGE THAT IS BEING READ
! IOUT:   UNIT NUMBER OF FILE TO WRITE RECORD INFORMATION (i.e. THE LIST FILE) 
! LABEL:  HEADER THAT IS PRINTED TO IOUT
! CAUX:   CHARACTER ARRAY CONTAINING THE AUX FLAGS TO LOOK FOR
! NCAUX:  DIM OF CAUX
! NAUX:   NUMBER AND DIMENSION OF THE AUXILIARY VARIABLES (i.e. RLST%AUX(NAUX))
!     ******************************************************************
  USE CONSTANTS,   ONLY: NL
  USE OPENSPEC,    ONLY: FORM, ACCESS
  USE GLOBAL,      ONLY:NCOL,NROW,NLAY,DATAFILES
  INTEGER::NLST,LSTBEG,LDIM,MXLST,INPACK,IPRFLG
  INTEGER::IOUT,NAUX,NCAUX,IFREFM,ISCLOC1,ISCLOC2
  CLASS(PAK_PROP),DIMENSION(:),INTENT(INOUT)::RLST
  CHARACTER*(*) LABEL
  CHARACTER(16),DIMENSION(NCAUX):: CAUX
  CONTIGUOUS:: RLST
  !DIMENSION RLIST(LDIM,MXLIST)
  CHARACTER(700):: LINE,FNAME
  CHARACTER(20)::  FMTARG, ACCARG
  CHARACTER(20)::  FILEFMT
  CHARACTER(40)::  FMT1,FMT2
  INTEGER:: IN,ICLOSE,IBINARY
  INTEGER::LLOC,ISTART,ISTOP,ILOC,I,J,K,II,JJ,N,NN
  REAL:: SFAC
  DOUBLE PRECISION:: DTMP
  LOGICAL:: LVAL
  !INTEGER,PARAMETER::NUNOPN=99
  CHARACTER(9):: FORM_CHCK
  LOGICAL:: NOSHIFT
!     ------------------------------------------------------------------
!
!1------If the list is empty, return.
!     IF (NLST.EQ.Z) RETURN
!
!
  NOSHIFT = IFREFM.EQ.Z
!2------Check for and decode EXTERNAL and OPEN/CLOSE records.
  IN=INPACK
  ICLOSE=Z
  IBINARY=Z
  CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
  SFAC=1.
  LLOC=ONE
  CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
  IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,INPACK,IN,MSG='ULSTRDSTRUCT ERROR: FOUND KEYWORD EXTERNAL, WHICH SHOULD BE FOLLOWED BY A UNIT NUMBER TO LOAD DATA FROM.')  !COUNT
!       TEST IF EXTERNAL FILE IS OPEN
      INQUIRE( UNIT=IN, OPENED=LVAL )
      IF ( LVAL.EQV. FALSE )  CALL FILE_IO_ERROR(UNIT=IN,LINE=LINE,INFILE=INPACK,OUTPUT=IOUT,MSG='EXTERNAL UNIT IS NOT OPEN')
!       TEST IF OPEN EXTERNAL FILE IS FORMATTED OR UNFORMATTED/BINARY
!       SEE FORM VARIABLE IN openspec.inc
      FMTARG=FORM
      INQUIRE( UNIT=IN, FORM=FILEFMT )
      IF ( FILEFMT.EQ.FMTARG ) THEN
        IBINARY=ONE
      END IF
      IF(IPRFLG.EQ.ONE) THEN
                          IF ( IBINARY.NE.ONE ) THEN
                               WRITE(IOUT,'(A,I5)') ' Reading list on unit ', IN
                          ELSE
                               WRITE(IOUT,'(A,I5)') ' Reading list on binary unit ', IN
                          END IF
      END IF
      IF (IBINARY.NE.ONE) CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
  ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
         FNAME=LINE(ISTART:ISTOP)
         IN=Z
!          TEST IF OPEN\CLOSE FILE EXISTS
         INQUIRE( FILE=FNAME, EXIST=LVAL )
         IF ( LVAL.EQV. FALSE ) CALL FILE_IO_ERROR(FNAME=FNAME,LINE=LINE,INFILE=INPACK,OUTPUT=IOUT,MSG='Specified OPEN/CLOSE file does not exist')
         CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
         IF (LINE(ISTART:ISTOP).EQ.'(BINARY)') THEN
           IBINARY=ONE
           IF(IPRFLG.EQ.ONE) WRITE(IOUT,'(1X,/1X,A,/1X,A)') 'OPENING BINARY FILE :',FNAME
           FMTARG=FORM
           ACCARG=ACCESS
           CALL GENERIC_OPEN(FNAME=FNAME, IU=IN, IOUT=IOUT, ACTION='READ', FORM=FMTARG, ACCESS=ACCARG, STATUS='OLD', LINE=LINE, INFILE=INPACK)
         ELSE
           IF(IPRFLG.EQ.ONE) WRITE(IOUT,'(1X,/1X,A,/1X,A)') 'OPENING FILE :',FNAME
           CALL GENERIC_OPEN(FNAME=FNAME, IU=IN, IOUT=IOUT, ACTION='READ', STATUS='OLD', LINE=LINE, INFILE=INPACK)
         END IF
         ICLOSE=ONE
         IF (IBINARY.NE.ONE) CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
  ELSE IF(LINE(ISTART:ISTOP).EQ.'DATAFILE') THEN
         CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
         FNAME=LINE(ISTART:ISTOP)
!          TEST IF OPEN\CLOSE FILE EXISTS
         INQUIRE( FILE=FNAME, EXIST=LVAL )
         IF ( LVAL.EQV. FALSE ) CALL FILE_IO_ERROR(FNAME=FNAME,LINE=LINE,INFILE=INPACK,OUTPUT=IOUT,MSG='Specified DATAFILE file does not exist')
         INQUIRE(FILE=FNAME, OPENED=LVAL, NUMBER=IN, FORM=FORM_CHCK)
         IF(LVAL) THEN
              IF( FORM_CHCK .NE. 'FORMATTED')  IBINARY=ONE
              IF(IPRFLG.EQ.ONE) WRITE(IOUT,'(1X,/1X,A,/1X,A)') 'DATAFILE KEYWORD FOUND WITH OPENED FILE, RESUMING LOAD FROM CURRENT POSITION FOR FILE',FNAME
         ELSE
             IN=Z
             CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
             IF (LINE(ISTART:ISTOP).EQ.'(BINARY)') THEN
               IBINARY=ONE
               IF(IPRFLG.EQ.ONE) WRITE(IOUT,'(1X,/1X,A,/1X,A)') 'OPENING BINARY FILE :',FNAME
               FMTARG=FORM
               ACCARG=ACCESS
               CALL GENERIC_OPEN(FNAME=FNAME, IU=IN, IOUT=IOUT, ACTION='READ', FORM=FMTARG, ACCESS=ACCARG, STATUS='OLD', LINE=LINE, INFILE=INPACK)
             ELSE
               IF(IPRFLG.EQ.ONE) WRITE(IOUT,'(1X,/1X,A,/1X,A)') 'OPENING FILE :',FNAME
               CALL GENERIC_OPEN(FNAME=FNAME, IU=IN, IOUT=IOUT, ACTION='READ', STATUS='OLD', LINE=LINE, INFILE=INPACK)
             END IF
             ICLOSE=ONE
             IF (IBINARY.NE.ONE) CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
             !
             CALL DATAFILES%ADD(IN)
         END IF
  END IF
!
!3------Check for SFAC record in ascii records.
  IF (IBINARY.NE.ONE) THEN
     LLOC=ONE
     CALL PARSE_WORD(LINE,LLOC,ISTART,ISTOP)
     IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,DTMP,MSG='ULSTRDSTRUCT ERROR: FOUND KEYWORD "SFAC",  BUT FAILED TO LOAD THE FLOATING POINT NUMBER AFTER IT.')
         SFAC = SNGL(DTMP)
         IF(IPRFLG.EQ.ONE) THEN
             WRITE(IOUT,'(A,ES12.5)') 'LIST SCALING FACTOR=',SFAC
             IF(ISCLOC1.EQ.ISCLOC2) THEN
                  WRITE(IOUT,'(A,I3,A)') '(THE SCALE FACTOR WAS APPLIED TO FIELD',ISCLOC1,')'
             ELSE
                  WRITE(IOUT,'(2(A,I3))') '(THE SCALE FACTOR WAS APPLIED TO FIELDS',ISCLOC1,'-',ISCLOC2
             END IF
         ENDIF
         CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
     END IF
  END IF
!
!3------Write a label for the list if the list will be printed.
  IF(IPRFLG.EQ.ONE) THEN
     WRITE(IOUT,'(1X)')
     CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
  END IF
!
!4------Setup indices for reading the list
   N=NLST+LSTBEG-ONE
!
!4A-----READ THE LIST -- BINARY OR ASCII
  IF (IBINARY.NE.Z) THEN
    DO II=LSTBEG,N
       READ(IN) RLST(II)%LAY,RLST(II)%ROW,RLST(II)%COL,(RLST(II)%VAL(JJ),JJ=1,LDIM)
    END DO
  ELSE
!
!5------READ AN ASCII LIST
    READ_LIST: DO II=LSTBEG,N
!5A-----Read a line into the buffer.  (The first line has already been
!5A-----read to scan for EXTERNAL and SFAC records.)
       IF(II.NE.LSTBEG) CALL READ_TO_DATA(LINE, IN, NOSHIFT=NOSHIFT)
!
!5B-----Get the non-optional values from the line.
       IF(IFREFM.EQ.Z) THEN
             READ(LINE,'(3I10,*(F10.0))', IOSTAT=NN) K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM)
             LLOC=10*(LDIM+3)+1
             IF(NN.NE.Z) CALL STOP_ERROR(LINE,INPACK,IOUT,MSG='FAILED TO LOAD SET OF INPUT NUMBERS WITH NON-FREE FORMAT LOAD. PEASE CHECK IF YOU HAVE ALL THE NUMBERS AND THEY MEET THE CORRECT FORMATTING OR USE FREE FORMAT.')
       ELSE
             LLOC=ONE
             CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,K,MSG='ULSTRDSTRUCT ERROR: FAILED TO LOAD LAYER NUMBER.')
             CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,I,MSG='ULSTRDSTRUCT ERROR: FAILED TO LOAD ROW NUMBER.')
             CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,J,MSG='ULSTRDSTRUCT ERROR: FAILED TO LOAD COLUMN NUMBER.')
             DO JJ=1,LDIM
                         CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,DTMP,MSG='ULSTRDSTRUCT ERROR: FAILED TO LOAD FLOATING POINT NUMBER AFTER LAYER, ROW, COLUMN.'//NL//'INPUT EXPECTS '//NUM2STR(LDIM)//' FLOATING POINT NUMBERS AFTER LAYER, ROW, COLUMN,'//NL//'BUT FAILED TO LOAD THE '//NUM2STR(JJ)//' FLOATING POINT NUMBER ON THE CURRENT INPUT LINE.')
                         RLST(II)%VAL(JJ) = SNGL(DTMP)
             END DO
       END IF
       RLST(II)%ROW=I
       RLST(II)%COL=J
       RLST(II)%LAY=K
       !
       IF(ALLOCATED( RLST(II)%BUDGET_GROUP )) THEN
           CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
           RLST(II)%BUDGET_GROUP = LINE(ISTART:ISTOP)
       END IF
!
!5E-----Get the optional values from the line
       IF(NAUX.GT.Z) THEN
          DO JJ=1,NAUX                                                      !seb ADDED -IOUT TO PREVENT READ FAILURE WHEN NO AUX IS PROVIDED
                      !RLST(II)%AUX(JJ)=0
                      CALL GET_INTEGER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,RLST(II)%AUX(JJ),MSG='ULSTRDSTRUCT ERROR: EXPECTED TO LOAD '//NUM2STR(NAUX)//' TOTAL AUXILIARY VARIABLES,'//NL//'BUT FAILED TO LOAD THE '//NUM2STR(JJ)//' POSITION AUXILIARY FLAG.')
          END DO
       END IF
!
!-----Get the optional values tablefile name from the line
       SELECT TYPE (RLST)
       CLASS IS (PAK_PROPTAB)
          RLST(II)%TABIDX=Z
          IF(ALLOCATED( RLST(II)%TABNAM )) THEN
             I = COMMENT_INDEX(LINE)
             CALL PARSE_WORD_UP(LINE(:I),LLOC,ISTART,ISTOP)  !I= ONE LESS THAN # LOCATION
             !
             IF (LINE(ISTART:ISTOP) ==' ' .OR. ISTOP<ISTART) THEN               !NO TABFILE SO SKIP READING THE TSFAC -- PARSE_WORD SETS LINE(ISTART:ISTOP) = ' ' IF REACHED END OF STRING
                 RLST(II)%TSFAC=1.0E0
                 RLST(II)%TABNAM='     NO_TABFILE     '
                 CYCLE
             ELSE
                 RLST(II)%TABNAM=LINE(ISTART:ISTOP)
                 RLST(II)%TABNAM=ADJUSTL(RLST(II)%TABNAM)
             END IF
             CALL UPPER(RLST(II)%TABNAM)
             CALL GET_NUMBER(LINE,LLOC,ISTART,ISTOP,IOUT,IN,DTMP,MSG='NOSTOP')
             IF (DTMP.NE.DTMP) THEN
                 RLST(II)%TSFAC=1.0E0
                 RLST(II)%TABNAM='     NO_TABFILE     '
                 IF(ALLOCATED(RLST(II)%TABEQNRES)) RLST(II)%TABEQN=''
             ELSEIF(ALLOCATED(RLST(II)%TABEQNRES)) THEN
                 CALL PARSE_WORD_UP(LINE(:I),LLOC,ISTART,ISTOP)
                 IF (LINE(ISTART:ISTOP) ==' ') THEN
                    RLST(II)%TABEQN=''
                 ELSE
                    RLST(II)%TABEQN=LINE(ISTART:ISTOP)
                 END IF
             END IF
             IF (DTMP == DTMP) RLST(II)%TSFAC = SNGL(DTMP)
          END IF
       END SELECT
    END DO READ_LIST
  ENDIF 
!
!6----SCALE THE DATA AND CHECK 
  DO II=LSTBEG,N
      I=RLST(II)%ROW
      J=RLST(II)%COL
      K=RLST(II)%LAY
!
!6A------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO ILOC=ISCLOC1,ISCLOC2
           RLST(II)%VAL(ILOC)=RLST(II)%VAL(ILOC)*SFAC
      END DO
!
!6B-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(IPRFLG.EQ.ONE)THEN
                         WRITE(FMT1,*)LDIM
                         WRITE(FMT2,*)NAUX
                         FMT1=ADJUSTL(FMT1);  FMT2=ADJUSTL(FMT2)
                         FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8,'//TRIM(FMT2)//'I10)'
                         FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8)'
                         IF(NAUX.GT.Z)THEN   
                                WRITE(IOUT,FMT2) NN,K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM),(RLST(II)%AUX(JJ),JJ=1,NAUX) 
                         ELSE
                                WRITE(IOUT,FMT1) NN,K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM),(RLST(II)%AUX(JJ),JJ=1,NAUX) 
                         END IF
      END IF
!
!6C-----Check for illegal grid location
      IF(K.LT.ONE .OR. K.GT.NLAY) THEN
          CALL STOP_ERROR(INFILE=INPACK,MSG='Layer number in list is outside of the grid',OUTPUT=IOUT)
      END IF
      IF(I.LT.ONE .OR. I.GT.NROW) THEN
         CALL STOP_ERROR(INFILE=INPACK,MSG='Row number in list is outside of the grid',OUTPUT=IOUT)
      END IF
      IF(J.LT.ONE .OR. J.GT.NCOL) THEN
         CALL STOP_ERROR(INFILE=INPACK,MSG='Column number in list is outside of the grid',OUTPUT=IOUT)
      END IF
  END DO
!
!7------Done reading the list.  If file is open/close, close it.
  IF(ICLOSE.NE.Z) CLOSE(UNIT=IN)
!
END SUBROUTINE
!
SUBROUTINE UPARLSTSUBSTRUC(IN,PACK,IOUTU,PTYP,RLST,LDIM,MXDIM,  &
                           MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX,IPRT)
!     ******************************************************************
!     Read a list parameter name, look it up in the list of parameters,
!     and substitute values into active part of package array.
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
  USE PARAMMODULE
  INTEGER:: IOUTU,IN,LDIM,MXDIM,MXLST,NTOT
  INTEGER:: IPVL1,IPVL2,NCAUX,NAUX,IPRT
  CLASS(PAK_PROP),DIMENSION(:),INTENT(INOUT)::RLST
  CHARACTER*(*) LABEL
  CHARACTER(16),DIMENSION(NCAUX):: CAUX
  CHARACTER*(*) PACK,PTYP
  CONTIGUOUS:: RLST
  !DIMENSION RLIST(LDIM,MXLST)
  CHARACTER(700):: LINE
  CHARACTER(MXNAMLEN):: CTMP1,CTMP2,CTMP3,CTMP4                     !seb CHANGED FROM CHAR LEN OF 10
  CHARACTER(40)::  FMT1,FMT2
  INTEGER:: NUMINST,NLST,NI,KI,LLOC,ILOC
  INTEGER:: IL,IR,IC,IP,I,II,III,JJ,ISTART,ISTOP,IPVL,IOUT
!     ------------------------------------------------------------------
!
!1------The LIST file unit is the absolute value of IOUTU.  
!1------Read the parameter name.
  !IOUT = ABS(IOUTU)
  IOUT = IOUTU
  CALL READ_TO_DATA(LINE, IN)
  LLOC=ONE
  CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
  WRITE(IOUT,'(2A)') ' Parameter:  ',LINE(ISTART:ISTOP)
  IF(LINE(ISTART:ISTOP).EQ.' ') CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT, MSG=' Blank parameter name in the '//PACK//' file.')
!
!2------Find the parameter in the list of parameters.
  CTMP1=LINE(ISTART:ISTOP)
  CALL UPPER(CTMP1)
  PARAM: DO IP=1,IPSUM
     CTMP2=PARNAM(IP)
     CALL UPPER(CTMP2)
     IF(CTMP1.EQ.CTMP2) THEN
         IF(PARTYP(IP).NE.PTYP) CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT, MSG='Parameter type conflict:'//NL//'Named parameter: '//PARNAM(IP)//' was defined as type: '//PARTYP(IP)//NL//'However, this parameter is used in the '//PACK//' file, so it should be type: '//PTYP//' file.')
!
!3------Set indices to point to the cells that correspond to the
!3------specified parameter.  If the parameter is time varying, set the
!3------indices to the specified instance.
         NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
         NUMINST=IPLOC(3,IP)
         ILOC=IPLOC(4,IP)
         NI=ONE
         IF(NUMINST.GT.Z) THEN
             NLST=NLST/NUMINST
             CALL PARSE_WORD_UP(LINE,LLOC,ISTART,ISTOP)
             CTMP3=LINE(ISTART:ISTOP)
             IF(CTMP3.EQ.' ') CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT, MSG='Blank instance name in the '//PACK//' file for parameter '//PARNAM(IP))
             IF(IPRT.NE.Z) WRITE(IOUT,'(A)') '    Instance:  '//CTMP3
             CALL UPPER(CTMP3)
             INSTANCE: DO KI=1,NUMINST
                             CTMP4=INAME(ILOC+KI-1)
                             CALL UPPER(CTMP4)
                             IF   (CTMP3.EQ.CTMP4) THEN
                                  NI=KI
                                  EXIT INSTANCE
                             ENDIF
                             IF(KI==NUMINST) CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT, MSG=' The '//PACK//' file specifies undefined instance "'//CTMP3//'" for parameter '//PARNAM(IP))
             END DO INSTANCE
         ENDIF
!
!4------Check that the parameter is not already active.
          IF (IACTIVE(IP).GT.Z) CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT,  MSG='PARAMETER "'//PARNAM(IP)//'" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD'//NL//' -- STOP EXECUTION (UPARLSTSUBSTRUC)')
!
!5------Set the active flag.
         IACTIVE(IP)=NI
!
!6------Accumulate the total number of active cells in the list.
         NTOT=NTOT+NLST
         IF(NTOT.GT.MXLST) CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT,  MSG='THE NUMBER OF ACTIVE LIST ENTRIES ('//NUM2STR(NTOT)//')'//NL//'IS GREATER THAN THE MAXIMUM ALLOWED ('//NUM2STR(MXLST)//')')
!
!7------Write label for list values if IOUTU is positive.
         IF(IPRT.NE.Z) THEN    
           CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
         ENDIF
!
!8------Copy the values from the paramter location into the front part
!8------of the list where the currently active list is kept.
         COPY: DO I=1,NLST
            II=NTOT-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            !RLST(II)=RLST(III)                                          !TRANSFER ALL DATA TYPE INFORMATION OVER. THIS MAKES USE OF THE INTERFACE ASSIGNMENT(=). UNFORTUNETLY BECAUSE SUBROUTINE WAS DEFINED WITH CLASS IT WILL ONLY USE THE CONBND ASSIGNMNET AND NOT THE CONBNDTAB ONE
!
            SELECT TYPE (RLST)
            TYPE IS (PAK_PROP)
                       CALL PAK_PROP_TRANSFER( RLST(II),RLST(III) )     !TRANSFER ALL DATA TYPE INFORMATION OVER. THIS MAKES USE OF THE INTERFACE ASSIGNMENT(=). UNFORTUNETLY BECAUSE SUBROUTINE WAS DEFINED WITH CLASS IT WILL ONLY USE THE CONBND ASSIGNMNET AND NOT THE CONBNDTAB ONE
            TYPE IS (PAK_PROPTAB)
                       CALL PAK_PROPTAB_TRANSFER( RLST(II),RLST(III) )
             !IF( ALLOCATED(RLST(II)%TABNAM )) THEN
             !  RLST(II)%TABNAM=RLST(III)%TABNAM
             !  RLST(II)%TABIDX=RLST(III)%TABIDX
             !  RLST(II)%TSFAC =RLST(III)%TSFAC 
             !      IF(ALLOCATED(RLST(II)%TABEQNRES)) THEN
             !                   RLST(II)%TABEQN=RLST(III)%TABEQN
             !      END IF
             !END IF
            END SELECT
!
!8A-----Scale the RLIST values from IPVL1 to IPVL2 by the parameter
!8A-----value.
            DO IPVL=IPVL1,IPVL2
                              RLST(II)%VAL(IPVL)=RLST(III)%VAL(IPVL)*B(IP)
            END DO
            IL=RLST(II)%LAY
            IR=RLST(II)%ROW
            IC=RLST(II)%COL
            IF (IPRT.NE.Z) THEN
                     WRITE(FMT1,*)LDIM
                     WRITE(FMT2,*)NAUX
                     FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8,'//TRIM(FMT2)//'I10)'
                     FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.8)'
                     IF(NAUX.GT.Z)THEN   
                                       WRITE(IOUT,FMT2) II,IL,IR,IC,(RLST(II)%VAL(JJ),JJ=1,LDIM),(RLST(II)%AUX(JJ),JJ=1,NAUX) 
                     ELSE
                                       WRITE(IOUT,FMT1) II,IL,IR,IC,(RLST(II)%VAL(JJ),JJ=1,LDIM),(RLST(II)%AUX(JJ),JJ=1,NAUX) 
                     END IF  
              END IF
       END DO COPY
!
!8B------After moving the data, return.
          RETURN
        END IF
  END DO PARAM
!
!9------All parameter names have been checked without finding the
!9------parameter. Write an error message and stop.
      CALL STOP_ERROR(INFILE=IN, OUTPUT=IOUT, MSG=' The '//PACK//' file specifies an undefined parameter:'//LINE(ISTART:ISTOP))
!
END SUBROUTINE
!
END MODULE