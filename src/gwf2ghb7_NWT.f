      MODULE CONDBND_INTERFACE
        TYPE CONDBND
          INTEGER::LAY,ROW,COL                              !BNDS(5+1+NAUX,MXBND) BND(6)=CBCFLOW, BND(1:3)=[LAY,ROW,COL]
          REAL   :: CBCFLOW
          REAL,   DIMENSION(:),ALLOCATABLE::VAL
          INTEGER,DIMENSION(:),ALLOCATABLE::AUX
        END TYPE
        !
        TYPE, EXTENDS (CONDBND)::CONDBNDTAB
          CHARACTER(20),   ALLOCATABLE:: TABNAM
          REAL,            ALLOCATABLE:: TSFAC
          INTEGER:: TABIDX
      END TYPE CONDBNDTAB
      !
      INTERFACE ASSIGNMENT (=)
          MODULE PROCEDURE CONDBND_TRANSFER
          MODULE PROCEDURE CONDBNDTAB_TRANSFER
      END INTERFACE
      !
      CONTAINS
      !
      !SUBROUTINE THAT TRANSFERS INFORMATION FROM ONE CONDBND/CONDBNDTAB TYPE TO ANOTHER.
      !THIS IS IMPLICITLY DONE WITH AN EQUAL SIGN DO TO INTERFACE ASSIGNMENT
      SUBROUTINE  CONDBND_TRANSFER(OUTVAR,INVAR)
      TYPE(CONDBND),INTENT(INOUT)::OUTVAR
      TYPE(CONDBND),INTENT(IN   )::       INVAR
      !
      OUTVAR%LAY=INVAR%LAY
      OUTVAR%ROW=INVAR%ROW
      OUTVAR%COL=INVAR%COL
      OUTVAR%CBCFLOW=INVAR%CBCFLOW
      !
      IF(ALLOCATED(INVAR%VAL)) OUTVAR%VAL=INVAR%VAL
      IF(ALLOCATED(INVAR%AUX)) OUTVAR%AUX=INVAR%AUX
      !
      END SUBROUTINE
      !
      SUBROUTINE  CONDBNDTAB_TRANSFER(OUTVAR,INVAR)
      TYPE(CONDBNDTAB),INTENT(INOUT)::OUTVAR
      TYPE(CONDBNDTAB),INTENT(IN   )::       INVAR
      !
      OUTVAR%CONDBND=INVAR%CONDBND                                      !THIS COPYS OVER THE CONDBND PORTION OF THE DATA TYPE
      OUTVAR%TABNAM=INVAR%TABNAM
      OUTVAR%TABIDX=INVAR%TABIDX
      OUTVAR%TSFAC =INVAR%TSFAC 
      !
      END SUBROUTINE
      !
      SUBROUTINE ULSTRDSTRUCT(NLST,RLST,LSTBEG,LDIM,MXLST,INPACK,IOUT,
     1              LABEL,CAUX,NCAUX,NAUX,IFREFM,ISCLOC1,ISCLOC2,IPRFLG)
C     ******************************************************************
C     Read and print a list.  NAUX of the values in the list are
C     optional -- auxiliary data. This assumes structure shape of array
C NLST:   NUMBER OF ENTRIES TO BE READ IN
C RLST:   STRUCTURE THAT IS A CLASS OF CONDBND THAT WILL READ IN DATA
C LSTBEG: FIRST LOCATION IN RLIST THAT DATA IS READ INTO
C LDIM:   LEADING DIMENSION OF VALUE ARRAY (%VAL) IN RLIST  (i.e. RLIST%VAL(LDIM))
C MXLIST: SIZE OF RLIST (i.e. RLIST(MXLIST))
C INPACK: UNIT NUMBER OF INPUT FILE/PACKAGE THAT IS BEING READ
C IOUT:   UNIT NUMBER OF FILE TO WRITE RECORD INFORMATION (i.e. THE LIST FILE) 
C LABEL:  HEADER THAT IS PRINTED TO IOUT
C CAUX:   CHARACTER ARRAY CONTAINING THE AUX FLAGS TO LOOK FOR
C NCAUX:  DIM OF CAUX
C NAUX:   NUMBER AND DIMENSION OF THE AUXILARY VARIABLES (i.e. RLST%AUX(NAUX))
C     ******************************************************************
      USE GLOBAL,      ONLY:LSTCHK,NCOL,NROW,NLAY
      IMPLICIT NONE
      INTEGER::NLST,LSTBEG,LDIM,MXLST,INPACK,IPRFLG
      INTEGER::IOUT,NAUX,NCAUX,IFREFM,ISCLOC1,ISCLOC2
      CLASS(CONDBND),DIMENSION(:),INTENT(INOUT)::RLST
      CHARACTER*(*) LABEL
      CHARACTER(16),DIMENSION(NCAUX):: CAUX
      !DIMENSION RLIST(LDIM,MXLIST)
      CHARACTER(700):: LINE,FNAME
      CHARACTER(20)::  FMTARG, ACCARG
      CHARACTER(20)::  FILEFMT
      CHARACTER(30)::  CERR
      CHARACTER(40)::  FMT1,FMT2
      INTEGER:: IN,ICLOSE,IBINARY,IDUM
      INTEGER::LLOC,ISTART,ISTOP,ILOC,I,J,K,II,JJ,N,NN
      REAL:: SFAC,R
      LOGICAL:: LVAL
      INTEGER,PARAMETER::NUNOPN=99
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1------If the list is empty, return.
      IF (NLST.EQ.0) RETURN
C
C
C2------Check for and decode EXTERNAL and OPEN/CLOSE records.
      IN=INPACK
      ICLOSE=0
      IBINARY=0
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
C          TEST IF EXTERNAL FILE IS OPEN
         INQUIRE( UNIT=IN, OPENED=LVAL )
         IF ( LVAL.EQV. .FALSE. ) THEN
           WRITE ( CERR,110 ) IN
  110    FORMAT('External unit ', I4,' is not open')     
           IF(LSTCHK(1)) WRITE ( IOUT,'(1X,A)' ) CERR
           CALL USTOP(CERR)
         END IF
C          TEST IF OPEN EXTERNAL FILE IS FORMATTED OR UNFORMATTED/BINARY
C          SEE FORM VARIABLE IN openspec.inc
         FMTARG=FORM
         INQUIRE( UNIT=IN, FORM=FILEFMT )
         IF ( FILEFMT.EQ.FMTARG ) THEN
           IBINARY=1
         END IF
         IF(IPRFLG.EQ.1.AND.LSTCHK(3)) THEN
           IF ( IBINARY.NE.1 ) THEN
             WRITE(IOUT,111) IN
  111        FORMAT(1X,'Reading list on unit ',I4)
           ELSE
             WRITE(IOUT,1111) IN
 1111        FORMAT(1X,'Reading list on binary unit ',I4)
           END IF
         END IF
         IF (IBINARY.NE.1) READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
C          TEST IF OPEN\CLOSE FILE EXISTS
         INQUIRE( FILE=FNAME, EXIST=LVAL )
         IF ( LVAL.EQV. .FALSE. ) THEN
           IF(LSTCHK(1))WRITE ( IOUT,112 ) LINE(ISTART:ISTOP)
  112      FORMAT('Specified OPEN/CLOSE file ',(A),' does not exist')
           CALL USTOP('Specified OPEN/CLOSE file does not exist')
         END IF
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         IF (LINE(ISTART:ISTOP).EQ.'(BINARY)') THEN
           IBINARY=1
           IF(IPRFLG.EQ.1.AND.LSTCHK(3))WRITE(IOUT,1115) IN,FNAME
 1115      FORMAT(1X,/1X,'OPENING BINARY FILE ON UNIT ',I4,':',/1X,A)
           FMTARG=FORM
           ACCARG=ACCESS
           OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1),FORM=FMTARG,
     1          ACCESS=ACCARG,STATUS='OLD')
         ELSE
           IF(IPRFLG.EQ.1.AND.LSTCHK(3)) WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         END IF
         ICLOSE=1
         IF (IBINARY.NE.1) READ(IN,'(A)') LINE
      END IF
C
C3------Check for SFAC record in ascii records.
      IF (IBINARY.NE.1) THEN
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
          IF(IPRFLG.EQ.1) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,116) SFAC
            ENDIF
  116       FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
            IF(ISCLOC1.EQ.ISCLOC2) THEN
               IF(LSTCHK(3)) THEN
                 WRITE(IOUT,113) ISCLOC1
               ENDIF
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
            ELSE
               IF(LSTCHK(3)) THEN
                 WRITE(IOUT,114) ISCLOC1,ISCLOC2
               ENDIF
  114          FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1            I2,'-',I2,')')
            END IF
          ENDIF
          READ(IN,'(A)') LINE
        END IF
      END IF
C
C3------Write a label for the list if the list will be printed.
      IF(IPRFLG.EQ.1) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
         ENDIF
      END IF
C
C4------Setup indices for reading the list
      N=NLST+LSTBEG-1
C
C4A-----READ THE LIST -- BINARY OR ASCII
      IF (IBINARY.NE.0) THEN
        READ(IN) ((RLST(II)%VAL(JJ),JJ=1,LDIM),II=LSTBEG,N)
      ELSE
C
C5------READ AN ASCII LIST
        DO 240 II=LSTBEG,N
C
C5A-----Read a line into the buffer.  (The first line has already been
C5A-----read to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C5B-----Get the non-optional values from the line.
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(3I10,9F10.0)') K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM)
         LLOC=10*(LDIM+3)+1
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
         DO JJ=1,LDIM
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLST(II)%VAL(JJ),
     +                IOUT,IN)
         END DO
      END IF
      RLST(II)%ROW=I
      RLST(II)%COL=J
      RLST(II)%LAY=K
C
C5E-----Get the optional values from the line
      IF(NAUX.GT.0) THEN
      DO JJ=1,NAUX                                                      !seb ADDED -IOUT TO PREVENT READ FAILURE WHEN NO AUX IS PROVIDED
       RLST(II)%AUX(JJ)=0
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,RLST(II)%AUX(JJ),R,-IOUT,IN)
      END DO
      END IF
C
C-----Get the optional values tablefile name from the line
      SELECT TYPE (RLST)
      CLASS IS (CONDBNDTAB)
         RLST(II)%TABIDX=0
         IF(ALLOCATED( RLST(II)%TABNAM )) THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,R,IOUT,IN)
         RLST(II)%TABNAM=LINE(ISTART:ISTOP)
         RLST(II)%TABNAM=ADJUSTL(RLST(II)%TABNAM)
         CALL UPCASE(RLST(II)%TABNAM)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,RLST(II)%TSFAC,-1,IN)
         IF (LINE(LEN(LINE):LEN(LINE)).EQ.'E') THEN
             RLST(II)%TSFAC=1.0E0
             RLST(II)%TABNAM='     NO_TABFILE     '
         END IF
         END IF
      END SELECT
240     CONTINUE
      ENDIF 
C
C6----SCALE THE DATA AND CHECK 
      DO 250 II=LSTBEG,N
      I=RLST(II)%ROW
      J=RLST(II)%COL
      K=RLST(II)%LAY
C
C6A------Scale fields ISCLOC1-ISCLOC2 by SFAC
      DO ILOC=ISCLOC1,ISCLOC2
        RLST(II)%VAL(ILOC)=RLST(II)%VAL(ILOC)*SFAC
      END DO
C
C6B-----Write the values that were read if IPRFLG is 1.
      NN=II-LSTBEG+1
      IF(LSTCHK(3) .AND. IPRFLG.EQ.1)THEN
       WRITE(FMT1,*)LDIM
       WRITE(FMT2,*)NAUX
       FMT1=ADJUSTL(FMT1);  FMT2=ADJUSTL(FMT2)
       FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.4,'//TRIM(FMT2)//'I5)'
       FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.4)'
       IF(NAUX.GT.0)THEN   
        WRITE(IOUT,FMT2) NN,K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM),
     2                  (RLST(II)%AUX(JJ),JJ=1,NAUX) 
       ELSE
        WRITE(IOUT,FMT1) NN,K,I,J,(RLST(II)%VAL(JJ),JJ=1,LDIM),
     2                  (RLST(II)%AUX(JJ),JJ=1,NAUX) 
       END IF
      END IF
C
C6C-----Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         ENDIF
         CALL USTOP(' ')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' Row number in list is outside of the grid'
         ENDIF
         CALL USTOP(' ')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' Column number in list is outside of the grid'
         ENDIF
         CALL USTOP(' ')
      END IF
  250 CONTINUE
C
C7------Done reading the list.  If file is open/close, close it.
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE UPARLSTSUBSTRUC(IN,PACK,IOUTU,PTYP,RLST,LDIM,MXDIM,
     1           MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX)
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GLOBAL, ONLY:LSTCHK
      IMPLICIT NONE
      INTEGER::IOUTU,IN,LDIM,MXDIM,MXLST,NTOT,IPVL1,IPVL2,NCAUX,NAUX
      CLASS(CONDBND),DIMENSION(:),INTENT(INOUT)::RLST
      CHARACTER*(*) LABEL
      CHARACTER(16),DIMENSION(NCAUX):: CAUX
      CHARACTER*(*) PACK,PTYP
      !DIMENSION RLIST(LDIM,MXLST)
      CHARACTER(700):: LINE
      CHARACTER(10):: CTMP1,CTMP2,CTMP3,CTMP4
      CHARACTER(40)::  FMT1,FMT2
      INTEGER:: IDUM,NUMINST,NLST,NI,KI,LLOC,ILOC
      INTEGER:: IL,IR,IC,IP,I,II,III,JJ,ISTART,ISTOP,IPVL,IOUT
      REAL::RDUM
C     ------------------------------------------------------------------
C
C1------The LIST file unit is the absolute value of IOUTU.  
C1------Read the parameter name.
      IOUT = ABS(IOUTU)
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1) LINE(ISTART:ISTOP)
      ENDIF
    1 FORMAT(/,' Parameter:  ',A)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        ENDIF
        CALL USTOP(' ')
      END IF
C
C2------Find the parameter in the list of parameters.
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,IPSUM
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,11) PARNAM(IP),PARTYP(IP),PACK,PTYP
            ENDIF
   11       FORMAT(1X,'Parameter type conflict:',/
     1        1X,'Named parameter:',A,' was defined as type:',A,/
     2        1X,'However, this parameter is used in the ',A,
     3          ' file, so it should be type:',A)
            CALL USTOP(' ')
          END IF
C
C3------Set indices to point to the cells that correspond to the
C3------specified parameter.  If the parameter is time varying, set the
C3------indices to the specified instance.
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST.GT.0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,15)PACK,PARNAM(IP)
              ENDIF
   15         FORMAT(/,1X,'Blank instance name in the ',A,
     &               ' file for parameter ',A)
              CALL USTOP(' ')
            ENDIF
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,17) CTMP3
            ENDIF
   17       FORMAT(3X,'Instance:  ',A)
            CALL UPCASE(CTMP3)
            DO 50 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 55
              ENDIF
   50       CONTINUE
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,53) PACK,CTMP3,PARNAM(IP)
            ENDIF
   53       FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &             A,'" for parameter ',A)
            CALL USTOP(' ')
   55       CONTINUE
          ENDIF
C
C4------Check that the parameter is not already active.
          IF (IACTIVE(IP).GT.0) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,73) PARNAM(IP)
            ENDIF
   73       FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &          ' -- STOP EXECUTION (UPARLSTSUBSTRUC)')
            CALL USTOP(' ')
          ENDIF
C
C5------Set the active flag.
          IACTIVE(IP)=NI
C
C6------Accumulate the total number of active cells in the list.
          NTOT=NTOT+NLST
          IF(NTOT.GT.MXLST) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,83) NTOT,MXLST
            ENDIF
   83       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     1       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
            CALL USTOP(' ')
          END IF
C
C7------Write label for list values if IOUTU is positive.
          IF(LSTCHK(3)) THEN    
            IF (IOUTU.GT.0) CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
          ENDIF
C
C8------Copy the values from the paramter location into the front part
C8------of the list where the currently active list is kept.
          DO 90 I=1,NLST
            II=NTOT-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            RLST(II)=RLST(III)                                          !TRANSFER ALL DATA TYPE INFORMATION OVER. THIS MAKES USE OF THE INTERFACE ASSIGNMENT(=). UNFORTUNETLY BECAUSE SUBROUTINE WAS DEFINED WITH CLASS IT WILL ONLY USE THE CONBND ASSIGNMNET AND NOT THE CONBNDTAB ONE
C
            SELECT TYPE (RLST)
            CLASS IS (CONDBNDTAB)
             IF( ALLOCATED(RLST(II)%TABNAM )) THEN
               RLST(II)%TABNAM=RLST(III)%TABNAM
               RLST(II)%TABIDX=RLST(III)%TABIDX
               RLST(II)%TSFAC =RLST(III)%TSFAC 
             END IF
            END SELECT
C
C8A-----Scale the RLIST values from IPVL1 to IPVL2 by the parameter
C8A-----value.
            DO 86 IPVL=IPVL1,IPVL2
              RLST(II)%VAL(IPVL)=RLST(III)%VAL(IPVL)*B(IP)
   86       CONTINUE
            IL=RLST(II)%LAY
            IR=RLST(II)%ROW
            IC=RLST(II)%COL
            IF(LSTCHK(3)) THEN
              IF (IOUTU.GT.0) THEN
       WRITE(FMT1,*)LDIM
       WRITE(FMT2,*)NAUX
       FMT2='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.4,'//TRIM(FMT2)//'I5)'
       FMT1='(1X,I6,I7,I7,I7,'//TRIM(FMT1)//'G16.4)'
       IF(NAUX.GT.0)THEN   
        WRITE(IOUT,FMT2) II,IL,IR,IC,(RLST(II)%VAL(JJ),JJ=1,LDIM),
     2                  (RLST(II)%AUX(JJ),JJ=1,NAUX) 
       ELSE
        WRITE(IOUT,FMT1) II,IL,IR,IC,(RLST(II)%VAL(JJ),JJ=1,LDIM),
     2                  (RLST(II)%AUX(JJ),JJ=1,NAUX) 
       END IF  
              END IF
            ENDIF
   90     CONTINUE
C
C8B------After moving the data, return.
          RETURN
        END IF
  100 CONTINUE
C
C9------All parameter names have been checked without finding the
C9------parameter. Write an error message and stop.
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,*) ' The ',PACK,
     1   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      ENDIF
      CALL USTOP(' ')
C
      END SUBROUTINE
      !
      END MODULE
      !
      MODULE GWFGHBMODULE
        USE CONDBND_INTERFACE
        USE TABLEFILE_INTERFACE,  ONLY: TABFILETYPE
        TYPE(TABFILETYPE),POINTER,SAVE::GHBTABFILE
        TYPE(CONDBNDTAB),DIMENSION(:),SAVE,POINTER,CONTIGUOUS:: GHBDATA
        INTEGER,SAVE,POINTER  ::NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB
        INTEGER,SAVE,POINTER  ::NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB
        CHARACTER(LEN=16),SAVE,DIMENSION(:),  POINTER,CONTIGUOUS::GHBAUX
        !REAL,             SAVE,DIMENSION(:,:),POINTER,CONTIGUOUS::BNDS
        !
      TYPE GWFGHBTYPE
        TYPE(TABFILETYPE),POINTER::GHBTABFILE
        INTEGER,POINTER  ::NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB
        INTEGER,POINTER  ::NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB
        CHARACTER(LEN=16), DIMENSION(:),   POINTER,CONTIGUOUS::GHBAUX
        !REAL,              DIMENSION(:,:), POINTER,CONTIGUOUS::BNDS
        TYPE(CONDBNDTAB),DIMENSION(:),POINTER,CONTIGUOUS:: GHBDATA
      END TYPE
      TYPE(GWFGHBTYPE), SAVE:: GWFGHBDAT(10)
      END MODULE GWFGHBMODULE


      SUBROUTINE GWF2GHB7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ PARAMETER DEFINITIONS FOR GHB
C     PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFGHBMODULE, ONLY:NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB,NPGHB,
     1                      IGHBPB,NNPGHB,GHBAUX,GHBTABFILE,
     2                      GHBDATA,ULSTRDSTRUCT,IPRTGHBTAB
      USE TABLEFILE_INTERFACE,ONLY: TABFILEPARSE
      IMPLICIT NONE
C
      INTEGER:: IN,IGRID
C LOCAL
      CHARACTER(700):: LINE
      CHARACTER(75)::  LABEL
      INTEGER::NAUX,MXPB,MXACTB
      INTEGER::N,M,NUMINST,NINLST,NLST,LSTSUM,LSTBEG
      INTEGER::LLOC,ISTART,ISTOP,I,J,K,IP
      REAL::R
C     ------------------------------------------------------------------
      ALLOCATE(NBOUND,MXBND,NGHBVL,IGHBCB,IPRGHB)
      ALLOCATE(NPGHB,IGHBPB,NNPGHB,IPRTGHBTAB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NBOUND.
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1)IN
      ENDIF
    1 FORMAT(1X,/1X,'GHB -- GENERAL-HEAD BOUNDARY PACKAGE, VERSION 7',
     1   ', 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
      NBOUND=0
      NNPGHB=0
      IPRTGHBTAB=0
C
C2------READ MAXIMUM NUMBER OF GHB'S AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPGHB,MXPB)
      ALLOCATE(GHBTABFILE)
      CALL TABFILEPARSE(IN,IOUT,LINE,GHBTABFILE)
      !
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTB,IGHBCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTB,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGHBCB,R,IOUT,IN)
      END IF
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,3) MXACTB
      ENDIF
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE GHB CELLS AT ONE TIME')
      IF(LSTCHK(3)) THEN
        IF(IGHBCB.LT.0) WRITE(IOUT,7)
      ENDIF
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(LSTCHK(3)) THEN
        IF(IGHBCB.GT.0) WRITE(IOUT,8) IGHBCB
      ENDIF
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C3------READ AUXILIARY VARIABLES AND PRINT OPTION.
      ALLOCATE (GHBAUX(20))
      NAUX=0
      IPRGHB=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            GHBAUX(NAUX)=LINE(ISTART:ISTOP)
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,12) GHBAUX(NAUX)
            ENDIF
   12       FORMAT(1X,'AUXILIARY GHB VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,13)
         ENDIF
   13    FORMAT(1X,'LISTS OF GENERAL-HEAD BOUNDARY CELLS WILL NOT BE',
     &          ' PRINTED')
         IPRGHB = 0
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'TABPRINT') THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,14)
         ENDIF
   14    FORMAT(1X,'GHB RESULTS FROM TABFILE CALCULATIONS WILL BE ',
     &          'PRINTED TO LIST')
         IPRTGHBTAB = 1
         GO TO 10
      END IF
C3A-----USING STRUCTURE ARRAY THERE ARE ONLY TWO VALUES READ IN
C3A-----FIRST VALUE IS THE BHEAD AND SECOND VALUE IS THE CONDUCTANCE OR CONDFACT.
      NGHBVL=2
C
C4------ALLOCATE SPACE FOR THE BNDS ARRAY.
      IGHBPB=MXACTB+1
      MXBND=MXACTB+MXPB
      !ALLOCATE (BNDS(NGHBVL,MXBND))
      ALLOCATE (GHBDATA(MXBND))
      DO I=1,MXBND
                      ALLOCATE(GHBDATA(I)%VAL(NGHBVL))   !BNDS(NGHBVL,MXBND) <=> GHBDATA(MXBND)%VAL(NGHBVL)
        IF(NAUX.GT.0) ALLOCATE(GHBDATA(I)%AUX(NAUX))
        IF(GHBTABFILE%NTAB.GT.0)THEN
                      ALLOCATE(GHBDATA(I)%TABNAM   )
                      ALLOCATE(GHBDATA(I)%TSFAC    )
                      !ALLOCATE(GHBDATA(I)%TABIDX   )  !had to comment out cause the ANY function would not work on allocatable TABIDX
        END IF
      END DO
C
C-------READ NAMED PARAMETERS.
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1000) NPGHB
      ENDIF
 1000 FORMAT(1X,//1X,I5,' GHB parameters')
      LABEL=
     +  'BOUND. NO. LAYER   ROW   COL     BHEAD      COND FACTOR    AUX'
      IF(NPGHB.GT.0) THEN
        LSTSUM=IGHBPB
        DO 120 K=1,NPGHB
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXBND,IN,IOUT,IP,'GHB','GHB',1,
     &                  NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.EQ.0) THEN
C5A-----READ LIST OF CELLS WITHOUT INSTANCES.
            CALL ULSTRDSTRUCT(NLST,GHBDATA,LSTBEG,NGHBVL,MXBND,IN,IOUT,
     1              LABEL,GHBAUX,20,NAUX,IFREFM,1,1,IPRGHB)
          ELSE
C5B-----READ INSTANCES
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRGHB)
            CALL ULSTRDSTRUCT(NINLST,GHBDATA,LSTBEG,NGHBVL,MXBND,IN,IOUT
     1             ,LABEL,GHBAUX,20,NAUX,IFREFM,1,1,IPRGHB)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
          END IF
C IF A TABFILE HAS BEEN SPECIFIED BUILD INDEX FOR PARAMETER VARIABLES TO WHAT 
C TABFILE IS ASSOCIATED WITH THE PARAMETER
        IF(NPGHB.GT.0 .AND. GHBTABFILE%NTAB.GT.0)THEN
          DO I=IGHBPB,LSTSUM
            GHBDATA(I)%TABIDX=0
            IF(GHBDATA(I)%TABNAM.NE.'     NO_TABFILE     ') THEN
              DO J=1,GHBTABFILE%NTAB
                IF(GHBDATA(I)%TABNAM.EQ.GHBTABFILE%TABNAM(J)) THEN
                   GHBDATA(I)%TABIDX=J
                   EXIT
                END IF
              END DO
              IF(GHBDATA(I)%TABIDX.EQ.0)
     +                          GHBDATA(I)%TABNAM=' TABFILE_NOT_FOUND  '
            END IF
          END DO
        END IF
C
C6------RETURN
      CALL SGWF2GHB7PSV(IGRID)
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7RP(IN,IGRID,IUNITNWT)
C     ******************************************************************
C     READ GHB HEAD, CONDUCTANCE AND BOTTOM ELEVATION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,BOTM,LBOTM,
     1                       IBOUND
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFGHBMODULE, ONLY:NBOUND,MXBND,NGHBVL,IPRGHB,NPGHB,
     1                       IGHBPB,NNPGHB,GHBAUX,
     2                   GHBDATA,GHBTABFILE,ULSTRDSTRUCT,UPARLSTSUBSTRUC
      IMPLICIT NONE
      INTEGER:: IN,IGRID,IUNITNWT
C LOCAL
      CHARACTER(700)::LINE
      CHARACTER(75):: LABEL
      INTEGER::NAUX,MXACTB
      INTEGER::ITMP,NP,N,I,J,K,IR,IC,IL,IOUTU
      REAL::HB,BOT
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
C1------READ ITMP (NUMBER OF GHB'S OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF(NPGHB.GT.0) THEN
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(2I10)') ITMP,NP
         ELSE
            READ(IN,*) ITMP,NP
         END IF
      ELSE
         NP=0
         IF(IFREFM.EQ.0) THEN
            READ(IN,'(I10)') ITMP
         ELSE
            READ(IN,*) ITMP
         END IF
      END IF
C
C------CALCULATE SOME CONSTANTS
      NAUX=0
      IF(ALLOCATED(GHBDATA(1)%AUX)) NAUX=UBOUND(GHBDATA(1)%AUX,1)
      IOUTU = IOUT
      IF (IPRGHB.EQ.0) IOUTU=-IOUT
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER GHB'S.
      IF(ITMP.LT.0) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,7)
         ENDIF
    7    FORMAT(1X,/1X,
     1   'REUSING NON-PARAMETER GHB CELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPGHB=ITMP
      END IF
C
C3------IF THERE ARE NEW NON-PARAMETER GHB'S, READ THEM.
      MXACTB=IGHBPB-1
      LABEL=
     +    'BOUND. NO. LAYER   ROW   COL     BHEAD      CONDUCTANCE  AUX'
      IF(ITMP.GT.0) THEN
         IF(NNPGHB.GT.MXACTB) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,99) NNPGHB,MXACTB
            ENDIF
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE GHB CELLS (',I6,
     1                     ') IS GREATER THAN MXACTB(',I6,')')
            CALL USTOP(' ')
         END IF
      IF(IPRGHB.EQ.1 .AND. LSTCHK(3)) WRITE(IOUT,'(/ 2A)') 
     +      ' GHB PACKAGE STRESS PERIOD INFORMATION ',
     +      '[BEFORE APPLICATION OF TABFILES]'
         CALL ULSTRDSTRUCT(NNPGHB,GHBDATA,1,NGHBVL,MXBND,IN,IOUT,
     1              LABEL,GHBAUX,20,NAUX,IFREFM,1,1,IPRGHB)
C IF A TABFILE HAS BEEN SPECIFIED BUILD INDEX FOR NONPARAMETER VARIABLES TO WHAT 
C TABFILE IS ASSOCIATED WITH THE PARAMETER
        IF(GHBTABFILE%NTAB.GT.0)THEN
         DO I=1,NNPGHB
          GHBDATA(I)%TABIDX=0
           IF(GHBDATA(I)%TABNAM.NE.'     NO_TABFILE     ') THEN
             DO J=1,GHBTABFILE%NTAB
               IF(GHBDATA(I)%TABNAM.EQ.GHBTABFILE%TABNAM(J)) THEN
                  GHBDATA(I)%TABIDX=J
                  EXIT
               END IF
             END DO
             IF(GHBDATA(I)%TABIDX.EQ.0)
     +                         GHBDATA(I)%TABNAM=' TABFILE_NOT_FOUND  '
           END IF
         END DO
        END IF
      END IF
      NBOUND=NNPGHB
C
C1C-----IF THERE ARE ACTIVE GHB PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('GHB')
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
!         CALL UPARLSTSUB(IN,'GHB',IOUTU,'GHB',BNDS,NGHBVL,MXBND,NREAD,
!     1                MXACTB,NBOUND,5,5,
!     2      'BOUND. NO. LAYER   ROW   COL     STAGE      CONDUCTANCE',
!     3            GHBAUX,20,NAUX)
         CALL UPARLSTSUBSTRUC(IN,'GHB',IOUTU,'GHB',GHBDATA,NGHBVL,MXBND,
     1           MXACTB,NBOUND,2,2,LABEL,GHBAUX,20,NAUX)
   30    CONTINUE
      END IF
C4
      DO 100 I=1,NBOUND
C
C5------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY.
      IL=GHBDATA(I)%LAY
      IR=GHBDATA(I)%ROW
      IC=GHBDATA(I)%COL
C
C6------IF THE CELL IS EXTERNAL THEN SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C7------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.
      HB=GHBDATA(I)%VAL(1)
      BOT=BOTM(IC,IR,LBOTM(IL))
ccrth modified to only stop for NWT solver because ghb heads below model cell bottom is generally OK ADDED NTAB CHECK
      IF ( HB.LT.BOT.and. IUNITNWT.gt.0 .and. GHBTABFILE%NTAB.EQ.0) THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,103)IC,IR,IL
        ENDIF
        !CALL USTOP(' ')
      END IF
  103 FORMAT(/,'GHB HEAD SET TO BELOW CELL BOTTOM. MODEL WARNING. ',
     +                'CELL WITH ERROR (IC,IR,IL): ',3I5,/)

  100 CONTINUE
C
C3------PRINT NUMBER OF GHB'S IN CURRENT STRESS PERIOD.
      IF(LSTCHK(3)) THEN
        WRITE (IOUT,101) NBOUND
      ENDIF
  101 FORMAT(1X,/1X,I6,' GHB CELLS')
C
C8------RETURN.
  260 RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7AD(IGRID)
C     ******************************************************************
C     IF TABFILES ARE PRESENT APPLY THEM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      !USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,BOTM
      USE GLOBAL,       ONLY:IOUT,BOTM,LBOTM,LSTCHK
      USE GWFBASMODULE, ONLY:TOTIM,REALTIM
      USE GWFGHBMODULE, ONLY:NBOUND,IPRTGHBTAB,GHBDATA,GHBTABFILE
      USE CONDBND_INTERFACE
      USE TABLEFILE_INTERFACE
      USE CVT2STR, ONLY: NUM2STR
      IMPLICIT NONE
      INTEGER::IGRID
C     ------------------------------------------------------------------
      REAL::TABINP,BHEAD,COND,TSFAC,BOT
      INTEGER I,J,K,IC,IR,IL
      INTEGER::TABIDX
      !CHARACTER(20)::TABNAM
      !REAL,EXTERNAL::TABFILEINTERP
C
      CALL SGWF2GHB7PNT(IGRID)
C     
      IF(GHBTABFILE%NTAB.EQ.0) RETURN   ! NO TABFILES FOR GHB
      !
C PRINT OUT TABFILE OUTPUT HEAD
      IF(IPRTGHBTAB.NE.0)THEN
       IF(GHBTABFILE%SIMTIME)THEN
         WRITE(IOUT,'(/ 2A )') 
     + 'GHB PACKAGE TABFILE PRINT OUT AT SIMULTED TIME ',NUM2STR(TOTIM)
       ELSE
         WRITE(IOUT,'(/ A,A,A,F11.5)') 
     + 'GHB PACKAGE TABFILE PRINT OUT AT SIMULTED TIME ',NUM2STR(TOTIM),
     +   ' AND DECIMAL YEAR (REALTIME) ',REALTIM
       END IF
       IF(ANY(GHBDATA(1:NBOUND)%TABIDX.GT.0)) THEN
         WRITE(IOUT,'(3A)')'   ID   LAY   ROW   COL         BHEAD ',
     +        '    CONDUCTANCE              TSFAC  TABNAM             ',
     +        ' TABINPOLATED'
       ELSE
         WRITE(IOUT,'(A)')'NO TABFILES SPECIFIED THIS STRESS PERIOD'
         WRITE(IOUT,'(A)')'IF YOU DID SPECIFY A TABNAM FOR A GHB CELL,'
         WRITE(IOUT,'(A)')'IT WAS NOT MATCHED TO A TABFILE AND IGNORED'
       END IF
      END IF
      !
C 3 CASES
C SINGLE VALUE IN TIME STEP, USE ONLY 1 VALUE
C MULTIPLE VALUES IN TIME STEP, TIME WEIGHTED AVERAGE
C NO VALUES IN TIME STEP, LINEAR INTERPOLATE CLOSEST VALUES BEYOND TIME STEP
      GHBTABFILE%TAB%USEVAL=.FALSE.                                     !TELL TABFILEINTERP TO SEARCH FOR NEW TIME AND THEN START REUSING VALUES
      !
      DO I=1,NBOUND
        IF (GHBDATA(I)%TABIDX.EQ.0) CYCLE
        !
        TABIDX=GHBDATA(I)%TABIDX
        !
        CALL TABFILEINTERP( GHBTABFILE%TAB(TABIDX), GHBTABFILE%SIMTIME) !CALCULATE CURRENT TABFILE VALUE AND STORE IT IN GHBTABFILE%TAB(TABIDX)%VAL
        !
        TABINP=GHBTABFILE%TAB(TABIDX)%VAL
        !
        GHBDATA(I)%VAL(1) = GHBDATA(I)%TSFAC * TABINP                   !ONLY THE FIRST VALUE (BHEAD) IS UPDATED WITH TABFILE
      END DO
      !
C PRINT OUT TABFILE OUTPUT
      IF(IPRTGHBTAB.NE.0.AND.ANY(GHBDATA(1:NBOUND)%TABIDX.GT.0))THEN
       DO I=1,NBOUND
         IL=GHBDATA(I)%LAY;  IR=GHBDATA(I)%ROW;  IC=GHBDATA(I)%COL
         BHEAD=GHBDATA(I)%VAL(1)
         COND =GHBDATA(I)%VAL(2)
         TABIDX=GHBDATA(I)%TABIDX
         IF(TABIDX.GT.0)THEN
           TABINP=GHBTABFILE%TAB(TABIDX)%VAL
           TSFAC=GHBDATA(I)%TSFAC
           WRITE(IOUT,'(I5,3I6,2x,2(1PG16.9),ES15.8,2x,A,1PG16.9)')
     +           I,IL,IR,IC,BHEAD,COND,TSFAC,GHBDATA(I)%TABNAM,TABINP
         ELSE
           TSFAC=0E0
           WRITE(IOUT,'(I5,3I6,2x,2(1PG16.9),ES15.8,2x,A)')
     +           I,IL,IR,IC,BHEAD,COND,TSFAC,GHBDATA(I)%TABNAM
         END IF
       END DO
      END IF
      !
      !CHECK IF ANY GHB CELLS ARE LESS THEN CELL BOTTOM AND USING NWT. SEND WARNING TO IOUT
      DO I=1,NBOUND
        IL=GHBDATA(I)%LAY;  IR=GHBDATA(I)%ROW;  IC=GHBDATA(I)%COL
        BHEAD=GHBDATA(I)%VAL(1)
        BOT=BOTM(IC,IR,LBOTM(IL))
        IF(BHEAD.LT.BOT.AND.GHBTABFILE%NTAB.EQ.0.AND.LSTCHK(1)) THEN
                                                WRITE(IOUT,103) IL,IR,IC
        END IF
  103   FORMAT('WARNING: GHB HEAD SET TO BELOW CELL BOTTOM AT CELL ',
     +                '(IL,IR,IC): ',3I6)    
      END DO
C
C4------RETURN
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7FM(IGRID)
C     ******************************************************************
C     ADD GHB TERMS TO RHS AND HCOF
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,RHS,HCOF,botm,lbotm
      USE GWFGHBMODULE, ONLY:NBOUND,GHBDATA
      IMPLICIT NONE
      INTEGER:: IGRID
C LOCAL
      INTEGER:: IR,IC,IL,L
      REAL::HB,C
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
C1------IF NBOUND<=0 THEN THERE ARE NO GENERAL HEAD BOUNDS. RETURN.
      IF(NBOUND.LE.0) RETURN
C
C2------PROCESS EACH ENTRY IN THE GENERAL HEAD BOUND LIST (BNDS).
      DO 100 L=1,NBOUND
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY.
      IL=GHBDATA(L)%LAY
      IR=GHBDATA(L)%ROW
      IC=GHBDATA(L)%COL
C
C4------IF THE CELL IS EXTERNAL THEN SKIP IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C5------SINCE THE CELL IS INTERNAL GET THE BOUNDARY DATA.
      HB=GHBDATA(L)%VAL(1)
      C =GHBDATA(L)%VAL(2)
C
C6------ADD TERMS TO RHS AND HCOF.
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*HB
  100 CONTINUE
C
C7------RETURN.
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR GHB
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,
     1                      botm, lbotm
      USE GLOBAL,      ONLY:LSTCHK
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFGHBMODULE,ONLY:NBOUND,IGHBCB,NGHBVL,GHBAUX,GHBDATA
      IMPLICIT NONE
      INTEGER:: KSTP,KPER,IGRID
C LOCAL
C
      DOUBLE PRECISION CCGHB,CHB,RATIN,RATOUT,RRATE
      CHARACTER*16 TEXT
      DATA TEXT /' HEAD DEP BOUNDS'/
C
      INTEGER::NAUX,IBD,IBDLBL,IR,IC,IL,L
      REAL:: ZERO,HB,C
      REAL:: RATE,RIN,ROUT
      
      
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATOUT=ZERO
      RATIN=ZERO
      IBD=0
      IF(IGHBCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IGHBCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C CALCULATE NAUX
      NAUX=0
      IF(ALLOCATED(GHBDATA(1)%AUX)) NAUX=UBOUND(GHBDATA(1)%AUX,1)
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,GHBAUX,IGHBCB,NCOL,NROW,NLAY,
     1          NBOUND,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF NO BOUNDARIES, SKIP FLOW CALCULATIONS.
      IF(NBOUND.EQ.0) GO TO 200
C
C5------LOOP THROUGH EACH BOUNDARY CALCULATING FLOW.
      DO 100 L=1,NBOUND
C
C5A-----GET LAYER, ROW AND COLUMN OF EACH GENERAL HEAD BOUNDARY.
      IL=GHBDATA(L)%LAY
      IR=GHBDATA(L)%ROW
      IC=GHBDATA(L)%COL
      RATE=ZERO
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, THEN IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
C
C5C-----GET PARAMETERS FROM BOUNDARY LIST.
      HB=GHBDATA(L)%VAL(1)
      C =GHBDATA(L)%VAL(2)
      CCGHB=C
C
C5D-----CALCULATE THE FOW RATE INTO THE CELL.
      CHB=C*HB
      RRATE=CHB - CCGHB*HNEW(IC,IR,IL)
      RATE=RRATE
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IGHBCB<0).
      IF(IBD.LT.0) THEN
         IF(LSTCHK(3)) THEN
           IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
         ENDIF
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,62) L,IL,IR,IC,RATE
         ENDIF
   62    FORMAT(1X,'BOUNDARY ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',
     1       I5,'   RATE ',1PG15.6)
         IBDLBL=1
      END IF
C
C5F-----ADD RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C5G-----SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.
      IF(RATE.LT.ZERO) THEN
C
C5H------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
        RATOUT=RATOUT-RRATE
      ELSE
C
C5I-----FLOW IS INTO AQIFER; ADD RATE TO RATIN.
        RATIN=RATIN+RRATE
      END IF
C
C5J-----IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW.  ALSO
C5J-----FLOW TO BNDS.
   99 IF(IBD.EQ.2.AND.NAUX.GT.0) CALL UBDSVB(IGHBCB,NCOL,NROW,IC,IR,IL,
     1               RATE, REAL(GHBDATA(L)%AUX),NAUX,NAUX,1,IBOUND,NLAY)
      IF(IBD.EQ.2.AND.NAUX.EQ.0) CALL UBDSVB(IGHBCB,NCOL,NROW,IC,IR,IL,
     1                       RATE, GHBDATA(L)%VAL,                      ![CONSTRUCT DUMMY ARRAY THAT IS NOT USED]
     2                       NGHBVL,NAUX,NGHBVL,IBOUND,NLAY)
      GHBDATA(L)%CBCFLOW=RATE
  100 CONTINUE
C
C6------IF CELL-BY-CELL TERMS WILL BE SAVED AS A 3-D ARRAY, THEN CALL
C6------UTILITY MODULE UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IGHBCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT THE BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE GWF2GHB7DA(IGRID)
C  Deallocate GHB MEMORY
      USE GWFGHBMODULE
C
        DEALLOCATE(GWFGHBDAT(IGRID)%NBOUND    )
        DEALLOCATE(GWFGHBDAT(IGRID)%MXBND     )
        DEALLOCATE(GWFGHBDAT(IGRID)%NGHBVL    )
        DEALLOCATE(GWFGHBDAT(IGRID)%IGHBCB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%IPRGHB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%NPGHB     )
        DEALLOCATE(GWFGHBDAT(IGRID)%IGHBPB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%NNPGHB    )
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBAUX    )
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBTABFILE)
        DEALLOCATE(GWFGHBDAT(IGRID)%IPRTGHBTAB)
        DEALLOCATE(GWFGHBDAT(IGRID)%GHBDATA   )  !NOTE THAT BECAUSE GHBDATA ONLY CONTAINS ONLY ALLOCATABLE ARRAYS AND NORMAL SCALARS WITH NO POINTERS ALL SUBPORTIONS ARE AUTOMATICALLY DEALLOCATED
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        NBOUND    =>NULL()
        MXBND     =>NULL()
        NGHBVL    =>NULL()
        IGHBCB    =>NULL()
        IPRGHB    =>NULL()
        NPGHB     =>NULL()
        IGHBPB    =>NULL()
        NNPGHB    =>NULL()
        GHBAUX    =>NULL()
        GHBTABFILE=>NULL()
        IPRTGHBTAB=>NULL()
        GHBDATA   =>NULL()
      END IF
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SGWF2GHB7PNT(IGRID)
C  Change GHB data to a different grid.
      USE GWFGHBMODULE
C
        NBOUND=>GWFGHBDAT(IGRID)%NBOUND
        MXBND=>GWFGHBDAT(IGRID)%MXBND
        NGHBVL=>GWFGHBDAT(IGRID)%NGHBVL
        IGHBCB=>GWFGHBDAT(IGRID)%IGHBCB
        IPRGHB=>GWFGHBDAT(IGRID)%IPRGHB
        NPGHB=>GWFGHBDAT(IGRID)%NPGHB
        IGHBPB=>GWFGHBDAT(IGRID)%IGHBPB
        NNPGHB=>GWFGHBDAT(IGRID)%NNPGHB
        GHBAUX=>GWFGHBDAT(IGRID)%GHBAUX
        GHBTABFILE=>GWFGHBDAT(IGRID)%GHBTABFILE
        IPRTGHBTAB=>GWFGHBDAT(IGRID)%IPRTGHBTAB
        GHBDATA=>GWFGHBDAT(IGRID)%GHBDATA
C
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE SGWF2GHB7PSV(IGRID)
C  Save GHB data for a grid.
      USE GWFGHBMODULE
C
        GWFGHBDAT(IGRID)%NBOUND=>NBOUND
        GWFGHBDAT(IGRID)%MXBND=>MXBND
        GWFGHBDAT(IGRID)%NGHBVL=>NGHBVL
        GWFGHBDAT(IGRID)%IGHBCB=>IGHBCB
        GWFGHBDAT(IGRID)%IPRGHB=>IPRGHB
        GWFGHBDAT(IGRID)%NPGHB=>NPGHB
        GWFGHBDAT(IGRID)%IGHBPB=>IGHBPB
        GWFGHBDAT(IGRID)%NNPGHB=>NNPGHB
        GWFGHBDAT(IGRID)%GHBAUX=>GHBAUX
        GWFGHBDAT(IGRID)%GHBTABFILE=>GHBTABFILE
        GWFGHBDAT(IGRID)%IPRTGHBTAB=>IPRTGHBTAB
        GWFGHBDAT(IGRID)%GHBDATA=>GHBDATA
C
      RETURN
      END SUBROUTINE
      !


