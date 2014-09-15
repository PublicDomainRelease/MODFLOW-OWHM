C  The hydrograph lables are established by the SE subroutines the
C  first time they are called.  SE is called the first time by the
C  AR subroutine for the BAS, IBS, and SUB Packages.  SE is called
C  the first time by the RP subroutine for the STR and SFR Packages.
C  Subsequent calls to SE subroutines in the Main must be made in
C  the same order as the initial calls.  A HYDMOD package that
C  has an RP subroutine will always have the initial SE call after
C  the initial SE calls for all of the packages that have the
C  inital SE call in the AR subroutine.
      MODULE HYDBASMODULE
        INTEGER,SAVE,       POINTER                ::NHYDTOT
        REAL,   SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::HYDVAL
        CHARACTER(LEN=20),SAVE, POINTER,DIMENSION(:),CONTIGUOUS::HYDLBL
        INTEGER,SAVE,       POINTER             ::IHYDMUN,NHYDBAS
        REAL,   SAVE,       POINTER             ::HYDNOH
        LOGICAL,SAVE,       POINTER,DIMENSION(:)  ,CONTIGUOUS::IBHYDBAS
        LOGICAL,SAVE,       POINTER,DIMENSION(:),CONTIGUOUS::INTRPHYDBAS
        INTEGER,SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::JIKHYDBAS
        REAL,   SAVE,       POINTER,DIMENSION(:,:),CONTIGUOUS::HYDBASWT
        REAL,   SAVE,       POINTER,DIMENSION(:),CONTIGUOUS::HYDBASSTRT
        CHARACTER(LEN=4),SAVE,POINTER,DIMENSION(:),CONTIGUOUS::HYDBASARR
        TYPE HYDBASTYPE
          INTEGER, POINTER                         ::NHYDTOT
          REAL,             POINTER,DIMENSION(:,:),CONTIGUOUS ::HYDVAL
          CHARACTER(LEN=20),POINTER,DIMENSION(:),CONTIGUOUS   ::HYDLBL
          INTEGER,       POINTER                ::IHYDMUN,NHYDBAS
          REAL,          POINTER                ::HYDNOH
          LOGICAL,       POINTER,DIMENSION(:),CONTIGUOUS   ::IBHYDBAS
          LOGICAL,       POINTER,DIMENSION(:),CONTIGUOUS   ::INTRPHYDBAS
          INTEGER,       POINTER,DIMENSION(:,:),CONTIGUOUS ::JIKHYDBAS
          REAL,          POINTER,DIMENSION(:,:),CONTIGUOUS ::HYDBASWT
          REAL,          POINTER,DIMENSION(:),CONTIGUOUS   ::HYDBASSTRT
          CHARACTER(LEN=4),POINTER,DIMENSION(:),CONTIGUOUS ::HYDBASARR
        END TYPE
        TYPE(HYDBASTYPE),   SAVE  :: HYDBASDAT(10)
      END MODULE

      MODULE HYDIBSMODULE
        INTEGER, SAVE,         POINTER                ::NHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDIBS
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDIBS
        INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDIBS
        REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDIBSWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDIBSARR
        TYPE HYDIBSTYPE
          INTEGER,          POINTER                ::NHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDIBS
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDIBS
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDIBS
          REAL,             POINTER,DIMENSION(:,:) ::HYDIBSWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDIBSARR
        END TYPE 
        TYPE(HYDIBSTYPE),  SAVE  ::HYDIBSDAT(10)
      END MODULE

      MODULE HYDSUBMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::IBHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::INTRPHYDSUB
        LOGICAL, SAVE,         POINTER,DIMENSION(:)   ::UPPERLAYER
        INTEGER, SAVE,         POINTER,DIMENSION(:,:) ::JIKHYDSUB
        REAL,    SAVE,         POINTER,DIMENSION(:,:) ::HYDSUBWT
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSUBARR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::IBFACT
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::IBTOP
        TYPE HYDSUBTYPE
          INTEGER,          POINTER                ::NHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::IBHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::INTRPHYDSUB
          LOGICAL,          POINTER,DIMENSION(:)   ::UPPERLAYER
          INTEGER,          POINTER,DIMENSION(:,:) ::JIKHYDSUB
          REAL,             POINTER,DIMENSION(:,:) ::HYDSUBWT
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSUBARR
        END TYPE 
        TYPE(HYDSUBTYPE),  SAVE  ::HYDSUBDAT(10)
      END MODULE

      MODULE HYDSTRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSTR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISTRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSTRARR
        TYPE HYDSTRTYPE
          INTEGER,          POINTER                ::NHYDSTR
          INTEGER,          POINTER,DIMENSION(:)   ::ISTRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSTRARR
        END TYPE
        TYPE(HYDSTRTYPE),   SAVE   ::HYDSTRDAT(10)
      END MODULE

      MODULE HYDSFRMODULE
        INTEGER, SAVE,         POINTER                ::NHYDSFR
        INTEGER, SAVE,         POINTER,DIMENSION(:)   ::ISFRHYD
        CHARACTER(LEN=4),SAVE, POINTER,DIMENSION(:)   ::HYDSFRARR
        TYPE HYDSFRTYPE
          INTEGER,          POINTER                ::NHYDSFR
          INTEGER,          POINTER,DIMENSION(:)   ::ISFRHYD
          CHARACTER(LEN=4), POINTER,DIMENSION(:)   ::HYDSFRARR
        END TYPE
        TYPE(HYDSFRTYPE),   SAVE   ::HYDSFRDAT(10)
      END MODULE

      SUBROUTINE GWF2HYD7BAS7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGRAPH DATA FOR BAS PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL , ONLY: IOUT,NCOL,NROW,STRT,DELR,DELC  !seb added DELR,DELC
      USE GLOBAL,      ONLY:LSTCHK
      USE HYDBASMODULE
      CHARACTER*700 LINE
      CHARACTER*1 INTYP
      CHARACTER*20 HYDBASLBL
C     ------------------------------------------------------------------
      ALLOCATE(NHYDTOT)
      ALLOCATE(IHYDMUN,NHYDBAS,HYDNOH)
      ONE=1.0
      ZERO=0.0
      NHYDTOT=0
C
C1------IDENTIFY PROGRAM.
      IF(LSTCHK(3)) THEN
       WRITE(IOUT,1) IN
      ENDIF
    1 FORMAT(1X,/1X,'HYD -- HYDROGRAPH DATA FOR BAS, SUB, ,',
     1  '& SFR PACKAGES -- VERSION 7.1, 10/25/2009',/
     2  1X,'        INPUT READ FROM UNIT',I3)
C
C4------READ NUMBER OF HYDROGRAPHS AND UNIT FOR SAVING UNFORMATTED
C4------HYDROGRAPH FILE AND NUMERIC FLAG FOR DRY/INACTIVE CELLS
      READ(IN,'(A)') LINE
      LLOC=1
C  Number of hydrographs (NHYDM) specified by the user is ignored --
C    the program initially counts the number of hydrographs (NTOT).
C    Note that there may be less than NHTOT hydrograps because some
C    may be eliminated due to invalid values.
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHYDM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHYDMUN,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,HYDNOH,IOUT,IN)
      IF(LSTCHK(3)) THEN
       WRITE(IOUT,5) IHYDMUN,HYDNOH
      ENDIF
  5   FORMAT(1X,'HYDROGRAPH VALUES WILL BE SAVED ON UNIT:',I4,
     2     /,1X,'HYDROGRAPH VALUES AT DRY CELLS WILL BE:',1PG14.5)
C
C4------COUNT NUMBER OF BAS PACKAGE AND OVERALL HYDROGRAPHS.
      NTOT=0
      NHYDBAS=0
      REWIND(IN)
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      NTOT=NTOT+1
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'BAS') NHYDBAS=NHYDBAS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR ALL TYPES OF HYDROGRAPHS
 19   IF(NTOT.GT.0) THEN
        ALLOCATE(HYDVAL(NTOT,2))
        ALLOCATE(HYDLBL(NTOT))
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,17)
        ENDIF
 17     FORMAT(1X,/1X,
     1   'ARR INTYP  KLAY     XL            YL        ROW   COL       ',
     2   'HYDLBL', / 1X, 73('-'))
      ELSE
        ALLOCATE(HYDVAL(1,2))
        ALLOCATE(HYDLBL(1))
      END IF
C
C  ALLOCATE MEMORY FOR BAS HYDROGRAPH DATA
      IF(NHYDBAS.GT.0) THEN
        ALLOCATE(IBHYDBAS(NHYDBAS))
        ALLOCATE(INTRPHYDBAS(NHYDBAS))
        ALLOCATE(JIKHYDBAS(3,NHYDBAS))
        ALLOCATE(HYDBASWT(4,NHYDBAS))
        ALLOCATE(HYDBASSTRT(NHYDBAS))
        ALLOCATE(HYDBASARR(NHYDBAS))
      ELSE
        ALLOCATE(IBHYDBAS(1))
        ALLOCATE(INTRPHYDBAS(1))
        ALLOCATE(JIKHYDBAS(3,1))
        ALLOCATE(HYDBASWT(4,1))
        ALLOCATE(HYDBASSTRT(1))
        ALLOCATE(HYDBASARR(1))
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,18)
        ENDIF
  18    FORMAT(1X,'NO HYDROGRAPHS FOR BAS PACKAGE')
        GO TO 999
      END IF
      IF(NTOT.LE.0) THEN
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,16)
        ENDIF
  16    FORMAT(1X,'NO HYDROGRAPHS FOR ANY PACKAGE')
        GO TO 999
      END IF
C
C  READ BAS HYDROGRAPH DATA
      NHYDBAS=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 20   READ(IN,'(A)',END=99) LINE
      IF(LINE.EQ.' ') GO TO 20
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'BAS') GO TO 20
C
C Record applies to BAS Package
      NHYDBAS=NHYDBAS+1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      HYDBASARR(NHYDBAS)=LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(1:2),FMT='(A2)') HYDBASARR(NHYDBAS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDBASLBL(3:3),FMT='(A1)') LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDBASLBL(4:6),FMT='(I3.3)')KLAY
      HYDBASLBL(7:20)=LINE(ISTART:ISTOP)
!seb   IF(LSTCHK(3)) THEN
!moved      WRITE(IOUT,23) HYDBASARR(NHYDBAS),INTYP,KLAY,XL,YL,HYDBASLBL
!      ENDIF
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,1PE14.5,2X,A)  !seb ORIGINALLY: 1X,A,1X,A,I7,1PE14.5,E14.5,2X,A
      !seb ADDED CHECK FOR POINTS BEING WITHIN MODEL
      IF(XL<0. .OR. XL>SUM(DELR) .OR. YL<0. .OR. YL>SUM(DELC))THEN
         WRITE(LINE,'(3A, 1X A,1X A,I7,2EN14.5,2X A)') 
     +    NEW_LINE(INTYP),
     +   'HYDMOD ERROR: (XL,YL) BEYOND MODEL BOUNDS FOR THE FOLLOWING:',
     +    NEW_LINE(INTYP),
     +    HYDBASARR(NHYDBAS),INTYP,KLAY,XL,YL,HYDBASLBL
         IF(LSTCHK(1)) WRITE(IOUT,'(A)') TRIM(LINE)
ccrth         CALL USTOP(TRIM(LINE))
      END IF
C
C Determine if head
      IF(HYDBASARR(NHYDBAS).EQ.'HD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C Determine if drawdown
      ELSE IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
         IBHYDBAS(NHYDBAS)=.TRUE.
C
C  Not head or drawdown, so error.
      ELSE
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        ENDIF
         NHYDBAS=NHYDBAS-1
         GO TO 20
      ENDIF
C
C  Find the grid coodrdinates for the cell.
      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
Cseb WRITE OUT HYDMOD POINT INFORMATION
      IF(LSTCHK(3)) WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDBASARR(NHYDBAS),INTYP,KLAY,XL,YL,NR1,NC1,HYDBASLBL
C
C  Check if interpolating between nodes.
      IF(INTYP.EQ.'C') THEN
C
C  Do not interpolate
         INTRPHYDBAS(NHYDBAS)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
           IF(LSTCHK(2)) THEN
            WRITE(IOUT,26) TRIM(LINE)
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         ENDIF
            NHYDBAS=NHYDBAS-1
            GO TO 20
         ENDIF
         JIKHYDBAS(1,NHYDBAS)=NC1
         JIKHYDBAS(2,NHYDBAS)=NR1
         JIKHYDBAS(3,NHYDBAS)=KLAY
         HYDBASWT(1,NHYDBAS)=ONE
         HYDBASWT(2,NHYDBAS)=ZERO
         HYDBASWT(3,NHYDBAS)=ZERO
         HYDBASWT(4,NHYDBAS)=ZERO
      ELSE IF(INTYP.EQ.'I') THEN
C
C  Interpolate between cells
         INTRPHYDBAS(NHYDBAS)=.TRUE.
         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.(NCOL-1)) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,26) TRIM(LINE)
          ENDIF
            NHYDBAS=NHYDBAS-1
            GO TO 20
         ENDIF
         JIKHYDBAS(1,NHYDBAS)=NC2
         JIKHYDBAS(2,NHYDBAS)=NR2
         JIKHYDBAS(3,NHYDBAS)=KLAY
         HYDBASWT(1,NHYDBAS)=W1
         HYDBASWT(2,NHYDBAS)=W2
         HYDBASWT(3,NHYDBAS)=W3
         HYDBASWT(4,NHYDBAS)=W4
      ELSE
C
C  Interpolation coding error.
       IF(LSTCHK(1)) THEN
         WRITE(IOUT,27) TRIM(LINE)
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following record:',/,A)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
       ENDIF
         NHYDBAS=NHYDBAS-1
         GO TO 20
      ENDIF
C
C  If computing drawdown, save the starting head
      IF(HYDBASARR(NHYDBAS).EQ.'DD') THEN
         IF(INTYP.EQ.'I')THEN
            H1=STRT(NC2,NR2,KLAY)
            H2=STRT(NC2+1,NR2,KLAY)
            H3=STRT(NC2+1,NR2-1,KLAY)
            H4=STRT(NC2,NR2-1,KLAY)
            HYDBASSTRT(NHYDBAS)=H1*W1+H2*W2+H3*W3+H4*W4
         ELSEIF(INTYP.EQ.'C')THEN
            HYDBASSTRT(NHYDBAS)=STRT(NC1,NR1,KLAY)
         ENDIF
      ENDIF
C
C  Save the hydrograph label and continue with the next record.
      HYDLBL(NHYDBAS)=HYDBASLBL
      GO TO 20
C
C  End of file after all BAS HYDROGRAPH data have been processed
C  Note that NHYDTOT is accumulated by each package from this point on.
C  NHYDTOT is the total number of valid hydrographs after data are
C  checked for errors.
 99   NHYDTOT=NHYDBAS
C
999   CALL SGWF2HYD7BAS7PSV(IGRID)
      IF(NHYDBAS.GT.0) CALL GWF2HYD7BAS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7IBS7AR(IN,IGRID)
C     ******************************************************************
C     READ IBS PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY
      USE GLOBAL,      ONLY:LSTCHK
      USE HYDBASMODULE
      USE HYDIBSMODULE
      USE GWFIBSMODULE,   ONLY: IBQ,HC,SUB
C
      CHARACTER LINE*80
      CHARACTER HYDIBSLBL*20,PCKG*3,ARR*2,INTYP*1
C     ------------------------------------------------------------------
      ALLOCATE (NHYDIBS)
      ONE=1.0
      ZERO=0.0
      NHYDIBS=0
      REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'IBS') NHYDIBS=NHYDIBS+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR IBS HYDROGRAPH DATA
 19   IF(NHYDIBS.GT.0) THEN
        ALLOCATE(IBHYDIBS(NHYDIBS))
        ALLOCATE(INTRPHYDIBS(NHYDIBS))
        ALLOCATE(JIKHYDIBS(3,NHYDIBS))
        ALLOCATE(HYDIBSWT(4,NHYDIBS))
        ALLOCATE(HYDIBSARR(NHYDIBS))
      ELSE
        ALLOCATE(IBHYDIBS(1))
        ALLOCATE(INTRPHYDIBS(1))
        ALLOCATE(JIKHYDIBS(3,1))
        ALLOCATE(HYDIBSWT(4,1))
        ALLOCATE(HYDIBSARR(1))
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,18)
        ENDIF
  18    FORMAT(1X,'NO HYDROGRAPHS FOR IBS PACKAGE')
        GO TO 999
      END IF
C
C  Read IBS hydrograph data.
      NHYDIBS=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'IBS') GO TO 15
      NHYDIBS=NHYDIBS+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDIBSLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDIBSLBL(4:6),FMT='(I3.3)')KLAY
      HYDIBSLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDIBSARR(NHYDIBS)='HC'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES FROM THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDIBSARR(NHYDIBS)='CP'
         IBHYDIBS(NHYDIBS)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDIBSARR(NHYDIBS)='SB'
         IBHYDIBS(NHYDIBS)=.FALSE.
C  Change the layer number to the number of subsidence layers
         NQ=0
         DO 20 K=1,KLAY
         IF(IBQ(K).GT.0) NQ=NQ+1
 20      CONTINUE
         KLAY=NQ
      ELSE
         IF(LSTCHK(1)) THEN
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         ENDIF
         NHYDIBS=NHYDIBS-1
         GO TO 15
      ENDIF
!seb      IF(LSTCHK(3)) THEN
!      WRITE(IOUT,23) HYDIBSARR(NHYDIBS),INTYP,KLAY,XL,YL,HYDIBSLBL
!      ENDIF
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C  Get the cell location
      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
Cseb WRITE OUT HYDMOD POINT INFORMATION
      IF(LSTCHK(3)) WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDIBSARR(NHYDIBS),INTYP,KLAY,XL,YL,NR1,NC1,HYDIBSLBL
C
      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
         INTRPHYDIBS(NHYDIBS)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
            IF(LSTCHK(2)) THEN
             WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
          ENDIF
            NHYDIBS=NHYDIBS-1
            GO TO 15
         ENDIF
cc    
         JIKHYDIBS(1,NHYDIBS)=NC1
         JIKHYDIBS(2,NHYDIBS)=NR1
         JIKHYDIBS(3,NHYDIBS)=KLAY
         HYDIBSWT(1,NHYDIBS)=ONE
         HYDIBSWT(2,NHYDIBS)=ZERO
         HYDIBSWT(3,NHYDIBS)=ZERO
         HYDIBSWT(4,NHYDIBS)=ZERO
      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
         INTRPHYDIBS(NHYDIBS)=.TRUE.
         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL-1) THEN
          IF(LSTCHK(3)) THEN
           WRITE(IOUT,26) LINE
          ENDIF
            NHYDIBS=NHYDIBS-1
            GO TO 15
         ENDIF
         JIKHYDIBS(1,NHYDIBS)=NC2
         JIKHYDIBS(2,NHYDIBS)=NR2
         JIKHYDIBS(3,NHYDIBS)=KLAY
         HYDIBSWT(1,NHYDIBS)=W1
         HYDIBSWT(2,NHYDIBS)=W2
         HYDIBSWT(3,NHYDIBS)=W3
         HYDIBSWT(4,NHYDIBS)=W4
      ELSE
         IF(LSTCHK(1)) THEN
         WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following hydrograph record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         ENDIF
         NHYDIBS=NHYDIBS-1
         GO TO 15
      ENDIF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDIBS)=HYDIBSLBL
      GO TO 15
C
C  End of file after all IBS hydrograph data have been processed
 99   IF(NHYDIBS.GT.0) THEN
       IF(LSTCHK(3)) THEN
        WRITE(IOUT,108) NHYDIBS
       ENDIF
108     FORMAT(/,' A total of ',I10,' points have been added ',
     & 'for the hydrographs of IBS arrays.')
      END IF
C
 999  CALL SGWF2HYD7IBS7PSV(IGRID)
      IF(NHYDIBS.GT.0) CALL GWF2HYD7IBS7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SUB7AR(IN,IGRID)
C     ******************************************************************
C     READ SUB PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT,NCOL,NROW,NLAY
      USE GLOBAL, ONLY:LSTCHK
      USE HYDBASMODULE
      USE HYDSUBMODULE
      USE GWFSUBMODULE,   ONLY: LN,HC,SUB,SUBE,SUBV,NNDB 
C
      CHARACTER LINE*80
      CHARACTER HYDSUBLBL*20,PCKG*3,ARR*2,INTYP*1
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSUB)
      ONE=1.0
      ZERO=0.0
      NHYDSUB=0
      REWIND(IN)
C
      READ(IN,'(A)',END=19) LINE
 10   READ(IN,'(A)',END=19) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SUB') NHYDSUB=NHYDSUB+1
      GO TO 10
C
C  ALLOCATE MEMORY FOR SUB HYDROGRAPH DATA
 19   IF(NHYDSUB.GT.0) THEN
        ALLOCATE(IBHYDSUB(NHYDSUB))
        ALLOCATE(INTRPHYDSUB(NHYDSUB))
        ALLOCATE(JIKHYDSUB(3,NHYDSUB))
        ALLOCATE(HYDSUBWT(4,NHYDSUB))
        ALLOCATE(HYDSUBARR(NHYDSUB))
        ALLOCATE(IBFACT(NLAY))
        ALLOCATE(IBTOP(NHYDSUB))
        ALLOCATE(UPPERLAYER(NHYDSUB))
      ELSE
        ALLOCATE(IBHYDSUB(1))
        ALLOCATE(INTRPHYDSUB(1))
        ALLOCATE(JIKHYDSUB(3,1))
        ALLOCATE(HYDSUBWT(4,1))
        ALLOCATE(HYDSUBARR(1))
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,18)
        ENDIF
  18    FORMAT(1X,'NO HYDROGRAPHS FOR SUB PACKAGE')
        GO TO 999
      END IF
C
C  Read SUB hydrograph data.
      NHYDSUB=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 15   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'SUB') GO TO 15
      NHYDSUB=NHYDSUB+1
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDSUBLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDSUBLBL(4:6),FMT='(I3.3)')KLAY
      HYDSUBLBL(7:20)=LINE(ISTART:ISTOP)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         HYDSUBARR(NHYDSUB)='HC'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         HYDSUBARR(NHYDSUB)='CP'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE INELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CE') THEN
         HYDSUBARR(NHYDSUB)='CE'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES FROM THE ELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CV') THEN
         HYDSUBARR(NHYDSUB)='CV'
         IBHYDSUB(NHYDSUB)=.TRUE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         HYDSUBARR(NHYDSUB)='SB'
         IBHYDSUB(NHYDSUB)=.FALSE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL INELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SV') THEN
         HYDSUBARR(NHYDSUB)='SV'
         IBHYDSUB(NHYDSUB)=.FALSE.
cc    TIME SERIES ACCUMULATED OVER ALL LAYERS OF THE TOTAL ELASTIC COMPACTION ARRAY
      ELSE IF (ARR.EQ.'SE') THEN
         HYDSUBARR(NHYDSUB)='SE'
         IBHYDSUB(NHYDSUB)=.FALSE.
C  Change the layer number to the number of subsidence layers
         NQSUB=0
         DO 20 K=1,KLAY
         IF(LN(K).GT.0) NQSUB=NQSUB+1
 20      CONTINUE
         KLAY=NQSUB
      ELSE
         IF(LSTCHK(1)) THEN
          WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         ENDIF
         NHYDSUB=NHYDSUB-1
         GO TO 15
      ENDIF
!      IF(LSTCHK(3)) THEN
!       WRITE(IOUT,23) HYDSUBARR(NHYDSUB),INTYP,KLAY,XL,YL,HYDSUBLBL
!      ENDIF
! 23   FORMAT(1X,A,1X,A,I7,1PE14.5,E14.5,2X,A)
C
C  Get the cell location
      CALL SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,X1,X2,Y1,Y2)
Cseb WRITE OUT HYDMOD POINT INFORMATION
      IF(LSTCHK(3)) WRITE(IOUT,'(1X,A,1X,A,I7,2ES14.5,2I6,2X,A)')
     +  HYDSUBARR(NHYDSUB),INTYP,KLAY,XL,YL,NR1,NC1,HYDSUBLBL
C
      IF(INTYP.EQ.'C') THEN
C  Use cell value without interpolation
         INTRPHYDSUB(NHYDSUB)=.FALSE.
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
          IF(LSTCHK(2)) THEN
           WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
          ENDIF
            NHYDSUB=NHYDSUB-1
            GO TO 15
         ENDIF
         JIKHYDSUB(1,NHYDSUB)=NC1                                      !seb moved  out of if block
         JIKHYDSUB(2,NHYDSUB)=NR1
         JIKHYDSUB(3,NHYDSUB)=KLAY
         HYDSUBWT(1,NHYDSUB)=ZERO
         HYDSUBWT(2,NHYDSUB)=ZERO
         HYDSUBWT(3,NHYDSUB)=ZERO
         HYDSUBWT(4,NHYDSUB)=ZERO
       if((NR2.GT.0.and.NR2.LT.NROW).and.(NC2.GT.0.and.NC2.LT.NCOL))then
!seb       JIKHYDSUB(1,NHYDSUB)=NC1
!          JIKHYDSUB(2,NHYDSUB)=NR1
          if((NR1.EQ.NR2).and.(NC1.EQ.NC2))HYDSUBWT(1,NHYDSUB)=ONE
          if((NR1.EQ.NR2).and.(NC1.GT.NC2))HYDSUBWT(2,NHYDSUB)=ONE
          if((NR1.LT.NR2).and.(NC1.GT.NC2))HYDSUBWT(3,NHYDSUB)=ONE
          if((NR1.LT.NR2).and.(NC1.EQ.NC2))HYDSUBWT(4,NHYDSUB)=ONE
       else
!seb       JIKHYDSUB(1,NHYDSUB)=NC1
!          JIKHYDSUB(2,NHYDSUB)=NR1
          HYDSUBWT(1,NHYDSUB)=ONE
       endif
      ELSE IF(INTYP.EQ.'I') THEN
C  Interpolate value between nodes
         INTRPHYDSUB(NHYDSUB)=.TRUE.
         CALL SGWF2HYD7MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL-1) THEN
           IF(LSTCHK(3)) THEN
            WRITE(IOUT,26) LINE
           ENDIF
            NHYDSUB=NHYDSUB-1
            GO TO 15
         ENDIF
         JIKHYDSUB(1,NHYDSUB)=NC2
         JIKHYDSUB(2,NHYDSUB)=NR2
         JIKHYDSUB(3,NHYDSUB)=KLAY
         HYDSUBWT(1,NHYDSUB)=W1
         HYDSUBWT(2,NHYDSUB)=W2
         HYDSUBWT(3,NHYDSUB)=W3
         HYDSUBWT(4,NHYDSUB)=W4
      ELSE
         IF(LSTCHK(1)) THEN
          WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following hydrograph record:',/,A80)
          WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         ENDIF
         NHYDSUB=NHYDSUB-1
         GO TO 15
      ENDIF
C
C  Save the hydrograph label and process the next line in input file
      HYDLBL(NHYDTOT+NHYDSUB)=HYDSUBLBL
      GO TO 15
C
C  End of file after all SUB hydrograph data have been processed
 99   IF(NHYDSUB.GT.0) THEN
       IF(LSTCHK(3)) THEN
        WRITE(IOUT,108) NHYDSUB
       ENDIF
108     FORMAT(/,' A total of ',I10,' points have been added ',
     & 'for the hydrographs of SUB arrays.')
      END IF
C
 999  CALL SGWF2HYD7SUB7PSV(IGRID)
      IF(NHYDSUB.GT.0) CALL GWF2HYD7SUB7SE(2,IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7AR(IN,IGRID)
C     ******************************************************************
C     READ STREAM PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      CHARACTER LINE*80
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSTR)
C  Count number of Stream hydrographs
      NHYDSTR=0
      REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STR') NHYDSTR=NHYDSTR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSTR.GT.0) THEN
        ALLOCATE (ISTRHYD(NHYDSTR))
        ALLOCATE (HYDSTRARR(NHYDSTR))
      ELSE
        ALLOCATE (ISTRHYD(1))
        ALLOCATE (HYDSTRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7STR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT
      USE GLOBAL,   ONLY:LSTCHK
      USE HYDBASMODULE
      USE HYDSTRMODULE
      USE GWFSTRMODULE, ONLY: ISTRM,STRM,NSTREM
      CHARACTER HYDSTRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7STR7PNT(IGRID)
C
      IF(NSTREM.LT.1)THEN
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,8)
  8      FORMAT(' No Active Streams in this Model')
         WRITE(IOUT,'(1X,A)')
     1        'Stream hydrograph records will be ignored.'
        ENDIF
         NHYDSTR=0
         RETURN
      END IF
C
C
C ------Read STR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after STRRP
      IF(KPER.EQ.1) THEN
        NUMSTR=0
        REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'STR') GO TO 20
C
C ------PROCESS A STR HYDROGRAPH RECORD
        NUMSTR=NUMSTR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        HYDSTRARR(NUMSTR)=LINE(ISTART:ISTOP)
        WRITE(HYDSTRLBL(1:2),FMT='(A2)') HYDSTRARR(NUMSTR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        WRITE(HYDSTRLBL(3:5),FMT='(I3.3)') INT(XL)
        WRITE(HYDSTRLBL(6:8),FMT='(I3.3)') INT(YL)
        HYDSTRLBL(9:20)=LINE(ISTART:ISTOP)
C
        IF(HYDSTRARR(NUMSTR).NE.'ST' .AND.
     1      HYDSTRARR(NUMSTR).NE.'SO' .AND.
     2      HYDSTRARR(NUMSTR).NE.'SI' .AND.
     3      HYDSTRARR(NUMSTR).NE.'SA') THEN
          IF(LSTCHK(1)) THEN
           WRITE(IOUT,25) LINE
 25        FORMAT(' Invalid streamflow array was found on the following'
     &     ,' record:',/,A80)
           WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
          ENDIF
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTREM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISTRHYD(NUMSTR)=N
            GO TO 35
          END IF
 30     CONTINUE
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*)
     1      ' Hydrograph specified for non-existent stream reach'
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        ENDIF
        NUMSTR=NUMSTR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
           IF(LSTCHK(1)) THEN
            WRITE(IOUT,39) LINE
 39        FORMAT(' Invalid interpolation type was found on the ',
     &     'following record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           ENDIF
           NUMSTR=NUMSTR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSTR)=HYDSTRLBL
        GO TO 20
C
C ------End of input file.
 99     IF(LSTCHK(3)) THEN     
         WRITE(IOUT,100) NUMSTR
100     FORMAT(/,' A total of ',I10,' hydrographs have been added ',
     &   'for STR arrays.')
        ENDIF
        NHYDSTR=NUMSTR
C
C  Create initial values for stream hydrographs
        IF(NHYDSTR.GT.0) CALL GWF2HYD7STR7SE(2,IGRID)
      END IF
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7AR(IN,IGRID)
C     ******************************************************************
C     READ SFR2 PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: IOUT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      CHARACTER LINE*80
C     ------------------------------------------------------------------
      ALLOCATE (NHYDSFR)
C  Count number of Stream hydrographs
      NHYDSFR=0
      REWIND(IN)
      READ(IN,'(A)',END=15) LINE
 10   READ(IN,'(A)',END=15) LINE
      IF(LINE.EQ.' ') GO TO 10
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFR') NHYDSFR=NHYDSFR+1
      GO TO 10
C
C  Allocate memory
 15   IF(NHYDSFR.GT.0) THEN
        ALLOCATE (ISFRHYD(NHYDSFR))
        ALLOCATE (HYDSFRARR(NHYDSFR))
      ELSE
        ALLOCATE (ISFRHYD(1))
        ALLOCATE (HYDSFRARR(1))
      END IF
C
C ------RETURN.
      CALL SGWF2HYD7SFR7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT
      USE GLOBAL,   ONLY:LSTCHK
      USE HYDBASMODULE
      USE HYDSFRMODULE
      USE GWFSFRMODULE, ONLY: ISTRM,STRM,NSTRM
      USE CVT2STR, ONLY: NUM2STR
      CHARACTER HYDSFRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SFR7PNT(IGRID)
C
      IF(NSTRM.LT.1)THEN
        IF(LSTCHK(3)) THEN
         WRITE(IOUT,8)
  8      FORMAT(' No Active Streams in this Model')
         WRITE(IOUT,'(1X,A)')
     1        'Stream hydrograph records will be ignored.'
        ENDIF
         NHYDSFR=0
         RETURN
      END IF
C
C
C ------Read SFR hydrograph data
C ------Reading is done here (rather than in AR) because stream data are
C ------not available until the first time step is initiated.
C ---- Reading could be done in a special RP routine right after SFRRP
      IF(KPER.EQ.1) THEN
        NUMSFR=0
        REWIND(IN)
        READ(IN,'(A)',END=99) LINE
 20     READ(IN,'(A)',END=99) LINE
        IF(LINE.EQ.' ') GO TO 20
        HYDSFRLBL=''  !seb START REBUILDING NEW HYDLABEL
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).NE.'SFR') GO TO 20
C
C ------PROCESS A SFR HYDROGRAPH RECORD
        NUMSFR=NUMSFR+1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        HYDSFRARR(NUMSFR)=LINE(ISTART:ISTOP)
        WRITE(HYDSFRLBL(1:2),FMT='(A2)') HYDSFRARR(NUMSFR)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        INTYP=LINE(ISTART:ISTOP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        LEN_XL=LEN( NUM2STR(INT(XL)) )                                  !seb CHECK FOR OVERAGE OF SIZE
        LEN_YL=LEN( NUM2STR(INT(YL)) )
        IF(LEN_XL.LE.3 .AND. LEN_YL.LE.3) THEN
          WRITE(HYDSFRLBL(3:5),FMT='(I3.3)') INT(XL)          !XL = SEGMENT #
          WRITE(HYDSFRLBL(6:8),FMT='(I3.3)') INT(YL)          !YL = REACH #
          HYDSFRLBL(9:20)=LINE(ISTART:ISTOP)
        ELSE
          HYDSFRLBL=HYDSFRLBL(1:2) // 
     +        NUM2STR(INT(XL)) // NUM2STR(INT(YL)) // LINE(ISTART:ISTOP)
         !
         IF(LSTCHK(2)) WRITE(IOUT,'(//A, / 3x 3A, / 3x 4A, / 3x 2A,//)')
     +    'HYDMOD WARNING: SPECIFIED SFR SEGMENT AND/OR REACH '//
     +                                  'CONTAINS MORE THAN 3 DIGITS.',
     +    'THE UNIQUE ID ',LINE(ISTART:ISTOP),' MAYBE TRUNCATED ',
     +    'FOR SFR SEGMENT ', NUM2STR(INT(XL)), 
     +    ' AND REACH ', NUM2STR(INT(YL)), 
     +    'THE RESULTING ID IS: ', TRIM(HYDSFRLBL)
        END IF
C
        IF(HYDSFRARR(NUMSFR).NE.'ST' .AND.
     1      HYDSFRARR(NUMSFR).NE.'SO' .AND.
     2      HYDSFRARR(NUMSFR).NE.'SI' .AND.
     3      HYDSFRARR(NUMSFR).NE.'SA') THEN
           IF(LSTCHK(1)) THEN
            WRITE(IOUT,25) LINE
 25        FORMAT(' Invalid SFR array was found on the following'
     &     ,' record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           ENDIF
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------Look through all reaches to find the hydrograph reach.
        DO 30 N=1,NSTRM
 26       ISEG=ISTRM(4,N)
          IRCH=ISTRM(5,N)
C    XL contains the SEGMENT number and YL contains the REACH number. 
          IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
            ISFRHYD(NUMSFR)=N
            GO TO 35
          END IF
 30     CONTINUE
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*)
     1      ' Hydrograph specified for non-existent stream reach'
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        ENDIF
        NUMSFR=NUMSFR-1
        GO TO 20
C
C ------The interpolation type must be 'C'
 35     IF(INTYP.NE.'C') THEN
           IF(LSTCHK(1)) THEN
            WRITE(IOUT,39) LINE
 39        FORMAT(' Invalid interpolation type was found on the ',
     &     'following record:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
           ENDIF
           NUMSFR=NUMSFR-1
           GO TO 20
        ENDIF
C
C ------SAVE THE LABEL AND PROCESS NEXT RECORD.
        HYDLBL(NHYDTOT+NUMSFR)=HYDSFRLBL
        GO TO 20
C
C ------End of input file.
 99     IF(LSTCHK(3)) THEN     
         WRITE(IOUT,100) NUMSFR
        ENDIF
100     FORMAT(/,' A total of ',I10,' hydrographs have been added ',
     &   'for SFR arrays.')
        NHYDSFR=NUMSFR
C
C  Create initial values for stream hydrographs
        IF(NHYDSFR.GT.0) CALL GWF2HYD7SFR7SE(2,IGRID)
      END IF
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7BAS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR BAS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: HNEW,IBOUND,STRT,IOUT,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
C     ------------------------------------------------------------------
      CALL SGWF2HYD7BAS7PNT(IGRID)
C
C -----Return if no BAS hydrographs.
      NHYDTOT=NHYDBAS
      IF(NHYDBAS.LE.0) RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDBAS
      NN=N
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      J=JIKHYDBAS(1,N)
      I=JIKHYDBAS(2,N)
      K=JIKHYDBAS(3,N)
C
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
      IF(IBHYDBAS(N)) THEN
         IB1=IBOUND(J,I,K)
         IF(LAYHDT(K).NE.0) THEN
            IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K))) IB1=0                 !seb CHECK TO SEE IF THE CELL IS UPW DRY
         END IF
         IBFACT=IB1
         IF(INTRPHYDBAS(N)) THEN
            IB2=IBOUND(J+1,I,K)
            IB3=IBOUND(J+1,I-1,K)
            IB4=IBOUND(J,I-1,K)
            IF(LAYHDT(K).ne.0)then
            IF( HNEW(J+1,I,  K).LE.BOTM(J+1,I,  LBOTM(K)) ) IB2=0       !seb ADDED CHECK FOR UPW DRY CELLS
            IF( HNEW(J+1,I-1,K).LE.BOTM(J+1,I-1,LBOTM(K)) ) IB3=0
            IF( HNEW(J,  I-1,K).LE.BOTM(J,  I-1,LBOTM(K)) ) IB4=0
            END IF
            !
            IBFACT=IB1*IB2*IB3*IB4
         ENDIF
      ENDIF
C
C -----Check if hydrograph value is HEAD.
      IF(HYDBASARR(N).EQ.'HD') THEN
         IF(IBHYDBAS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(N,IHYDLOC)=HYDNOH
         ELSE
            HYDVAL(N,IHYDLOC)=SHYD7WTAVG(NN)
         ENDIF
C
C -----Hydrograph value is DRAWDOWN if NOT HEAD
      ELSE
         IF(IBHYDBAS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(N,IHYDLOC)=HYDNOH
         ELSE
            HYDVAL(N,IHYDLOC)=HYDBASSTRT(N) - SHYD7WTAVG(NN)
         ENDIF
      ENDIF
   50 CONTINUE
C
C ------ RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7IBS7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR IBS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: HNEW,IBOUND,IOUT,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
      USE HYDIBSMODULE
      USE GWFIBSMODULE,   ONLY: HC,SUB
C     ------------------------------------------------------------------
      CALL SGWF2HYD7IBS7PNT(IGRID)
C
C -----Return if no IBS hydrographs.
      IF(NHYDIBS.LE.0) RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDIBS
      NN=N
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      J=JIKHYDIBS(1,N)
      I=JIKHYDIBS(2,N)
      K=JIKHYDIBS(3,N)
C
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
      IF(IBHYDIBS(N)) THEN
         IB1=IBOUND(J,I,K)
         IF(LAYHDT(K).ne.0)then
            IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K))) IB1=0                 !seb CHECK TO SEE IF THE CELL IS UPW DRY
         END IF
         IBFACT=IB1
         IF(INTRPHYDIBS(N)) THEN
            IB2=IBOUND(J+1,I,K)
            IB3=IBOUND(J+1,I-1,K)
            IB4=IBOUND(J,I-1,K)
            IF(LAYHDT(K).ne.0)then
              IF( HNEW(J+1,I,  K).LE.BOTM(J+1,I,  LBOTM(K)) ) IB2=0     !seb ADDED CHECK FOR UPW DRY CELLS
              IF( HNEW(J+1,I-1,K).LE.BOTM(J+1,I-1,LBOTM(K)) ) IB3=0
              IF( HNEW(J,  I-1,K).LE.BOTM(J,  I-1,LBOTM(K)) ) IB4=0
            END IF
            !
            IBFACT=IB1*IB2*IB3*IB4
         ENDIF
      ENDIF
C
C -----Check if hydrograph value is Preconsolidation Head.
      IF(HYDIBSARR(N).EQ.'HC') THEN
         IF(IBHYDIBS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH                   
         ELSE
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            K=JIKHYDIBS(3,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            HYDVAL(NHYDTOT+N,IHYDLOC)=
     1 HC(J,I,K)*W1+HC(J+1,I,K)*W2+HC(J+1,I-1,K)*W3+HC(J,I-1,K)*W4
         ENDIF
C
C -----Check if hydrograph value is compaction
       ELSE IF(HYDIBSARR(N).EQ.'CP') THEN
         IF(IBHYDIBS(N) .AND. IBFACT.EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
         ELSE
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            K=JIKHYDIBS(3,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            HYDVAL(NHYDTOT+N,IHYDLOC)=
     1 SUB(J,I,K)*W1+SUB(J+1,I,K)*W2+SUB(J+1,I-1,K)*W3+SUB(J,I-1,K)*W4
         ENDIF
C
C -----Hydrograph value must be subsidence (not HC and not CP)
      ELSE
         KLAY=JIKHYDIBS(3,N)
         TOTL=0.
         DO 40 K=1,KLAY
            J=JIKHYDIBS(1,N)
            I=JIKHYDIBS(2,N)
            W1=HYDIBSWT(1,N)
            W2=HYDIBSWT(2,N)
            W3=HYDIBSWT(3,N)
            W4=HYDIBSWT(4,N)
            TOTL=TOTL+
     1 SUB(J,I,K)*W1+SUB(J+1,I,K)*W2+SUB(J+1,I-1,K)*W3+SUB(J,I-1,K)*W4
   40    CONTINUE
         HYDVAL(NHYDTOT+N,IHYDLOC)=TOTL
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDIBS
C
C ------ RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SUB7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR SUB
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: HNEW,IBOUND,IOUT,nrow,ncol,BOTM,LBOTM,LSTCHK,
     +  LAYHDT
      USE HYDBASMODULE
      USE HYDSUBMODULE
      USE GWFSUBMODULE,   ONLY: LN,HC,SUB,SUBE,SUBV,NNDB 
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SUB7PNT(IGRID)
C
C -----Return if no SUB hydrographs.
      IF(NHYDSUB.LE.0) RETURN
C
!seb      IK=0             !ADDED BY RTH, BUT NOT INCREMENTED IN SUBRTRACTION AT END OF SUBROUTINE
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NHYDSUB
      NN=N
      IBTOT=1
      UPPERLAYER(N)=.TRUE.
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      J=JIKHYDSUB(1,N)
      I=JIKHYDSUB(2,N)
      K=JIKHYDSUB(3,N)
      IBTOP(n)=0
      KLAY=K
C
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
         IB1=IBOUND(J,I,K)
         IBFACT(1)=IB1
        IF(HYDSUBARR(N).EQ.'HC'.or.HYDSUBARR(N).EQ.'CE'.or.
     &       HYDSUBARR(N).EQ.'CV'.or.HYDSUBARR(N).EQ.'CP')THEN
            IB2=IBOUND(J+1,I,K)
            IB3=IBOUND(J+1,I-1,K)
            IB4=IBOUND(J,I-1,K)
            IF(LAYHDT(K).NE.0) THEN
             IF(HNEW(J,I,K).LE.BOTM(J,I,LBOTM(K))) IB1=0                !seb CHECK TO SEE IF THE CELL IS UPW DRY
             IF( HNEW(J+1,I,  K).LE.BOTM(J+1,I,  LBOTM(K)) ) IB2=0      !seb ADDED CHECK FOR UPW DRY CELLS
             IF( HNEW(J+1,I-1,K).LE.BOTM(J+1,I-1,LBOTM(K)) ) IB3=0
             IF( HNEW(J,  I-1,K).LE.BOTM(J,  I-1,LBOTM(K)) ) IB4=0
            END IF
          if(INTRPHYDSUB(N))then
            IBFACT(1)=IB1*IB2*IB3*IB4
          else
            IBFACT(K)=IB1                                               !seb CHANGED FROM '=IB3'
          endif       
            IF(IBFACT(1).gt.0)IBFACT(1)=1
            IF(IBFACT(1).le.0)IBFACT(1)=0
        ELSEIF(HYDSUBARR(N).EQ.'SB'.or.HYDSUBARR(N).EQ.'SE'.or.
     &       HYDSUBARR(N).EQ.'SV') THEN
         IBTOT=0                                                        !seb INTIALIZE
         DO 38 K=1,KLAY
            IBFACT(K)=0
            IB1=IBOUND(J,I,K)
            IB2=IBOUND(J+1,I,K)
            IB3=IBOUND(J+1,I-1,K)
            IB4=IBOUND(J,I-1,K)
            IF(LAYHDT(K).NE.0) THEN                                     !seb ADDED CHECK FOR UPW DRY CELLS
              IF( HNEW(J  ,I,  K).LE.BOTM(J  ,I,  LBOTM(K)) ) IB1=0     
              IF( HNEW(J+1,I,  K).LE.BOTM(J+1,I,  LBOTM(K)) ) IB2=0
              IF( HNEW(J+1,I-1,K).LE.BOTM(J+1,I-1,LBOTM(K)) ) IB3=0
              IF( HNEW(J,  I-1,K).LE.BOTM(J,  I-1,LBOTM(K)) ) IB4=0
            END IF
         if(INTRPHYDSUB(N))then
            IBFACT(K)=IB1*IB2*IB3*IB4 
           IF(IBFACT(K).GT.0.and.(UPPERLAYER(N)))then
            IBTOP(N)=K
            UPPERLAYER(N)=.FALSE.
!seb           ELSEIF(IBFACT(K).LE.0)then
!seb            IBTOT=0
           ENDIF
           IF(IBFACT(K).GT.0.and.(.not.(UPPERLAYER(N))))then
               IBTOT=1
               IPTOP=K
               EXIT
           endif
!seb           IF(IBFACT(K).LE.0.and.(.not.(UPPERLAYER(N))))IBTOT=0
         else
           IBFACT(K)=IB1         
           IF(IBFACT(K).GT.0) THEN
            IBTOP(N)=K
            IBTOT=1
            EXIT
           END IF
           IF(K.EQ.KLAY)THEN
             IF(SUM(IBFACT).EQ.0)THEN
              IF(LSTCHK(1)) THEN
               WRITE(IOUT,37) N,I,J,K
 37            FORMAT(' HYDMOD SUB ERROR: Subsidence Point number ',I6,
     +         ' IN MODEL CELL WITH ROW, COL, LAYER',
     +         3I6,' DOES NOT HAVE A LAYER WITH A NONZERO IBOUND')
ccrth               CALL USTOP('HYDMOD SUB ERROR: MODEL CELL DOES NOT HAVE'//
ccrth     +                    ' A LAYER WITH A NONZERO IBOUND.')
ccrth              UPPERLAYER(N)=.FALSE. 
ccrth              IK=IK+1
              ENDIF
             END IF
           END IF
         endif
  38     CONTINUE
        ENDIF
C
C -----Check if hydrograph value is Preconsolidation Head.
      IF(HYDSUBARR(N).EQ.'HC') THEN
         IF(IBHYDSUB(N) .AND. IBFACT(1).EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH                   
         ELSE
            J=JIKHYDSUB(1,N)
            I=JIKHYDSUB(2,N)
            K=JIKHYDSUB(3,N)
            W1=HYDSUBWT(1,N)
            W2=HYDSUBWT(2,N)
            W3=HYDSUBWT(3,N)
            W4=HYDSUBWT(4,N)
            icell1 = ((K-1)*nrow*ncol)+((I-1)*ncol)+J
            icell2 = ((K-1)*nrow*ncol)+((I-1)*ncol)+(J+1)
            icell3 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J+1)
            icell4 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J)
            if(icell3.le.0.or.icell3.gt.(K*nrow*ncol))icell3=icell1
            if(icell4.le.0.or.icell3.gt.(K*nrow*ncol))icell4=icell1
            HYDVAL(NHYDTOT+N,IHYDLOC)=                            
     1 HC(icell1)*W1+HC(icell2)*W2+HC(icell3)*W3+HC(icell4)*W4
         ENDIF
C
C -----Check if hydrograph value is compaction
       ELSE IF(HYDSUBARR(N).EQ.'CP'.or.HYDSUBARR(N).EQ.'CE'.or.
     &        HYDSUBARR(N).EQ.'CV') THEN
         IF(IBHYDSUB(N) .AND. IBFACT(1).EQ.0) THEN
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
         ELSE
            J=JIKHYDSUB(1,N)
            I=JIKHYDSUB(2,N)
            K=JIKHYDSUB(3,N)
            W1=HYDSUBWT(1,N)
            W2=HYDSUBWT(2,N)
            W3=HYDSUBWT(3,N)
            W4=HYDSUBWT(4,N)
            icell1 = ((K-1)*nrow*ncol)+((I-1)*ncol)+J
            icell2 = ((K-1)*nrow*ncol)+((I-1)*ncol)+(J+1)
            icell3 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J+1)
            icell4 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J)
            if(icell3.le.0.or.icell3.gt.(K*nrow*ncol))icell3=icell1
            if(icell4.le.0.or.icell3.gt.(K*nrow*ncol))icell4=icell1
            IF(HYDSUBARR(N).EQ.'CP')HYDVAL(NHYDTOT+N,IHYDLOC)=                          
     1 SUB(icell1)*W1+SUB(icell2)*W2+SUB(icell3)*W3+SUB(icell4)*W4
            IF(HYDSUBARR(N).EQ.'CE')HYDVAL(NHYDTOT+N,IHYDLOC)=                          
     1 SUBE(icell1)*W1+SUBE(icell2)*W2+SUBE(icell3)*W3+SUBE(icell4)*W4
            IF(HYDSUBARR(N).EQ.'CV')HYDVAL(NHYDTOT+N,IHYDLOC)=                          
     1 SUBV(icell1)*W1+SUBV(icell2)*W2+SUBV(icell3)*W3+SUBV(icell4)*W4
         END IF
C
C -----Hydrograph value must be subsidence (not HC and not CP)
      ELSE IF(HYDSUBARR(N).EQ.'SB'.or.HYDSUBARR(N).EQ.'SE'.or.
     &       HYDSUBARR(N).EQ.'SV') THEN
         KLAY=JIKHYDSUB(3,N)
         TOTL=0.
cc         IF(IBHYDSUB(N).AND.(IBTOT.EQ.0.and.(INTRPHYDSUB(N)))) THEN
         IF(IBTOT.EQ.0. .OR. IBTOP(N).EQ.0) THEN                       !seb ADDED '.OR. IBTOP(N).EQ.0' TO CHECK IF THERE ARE NO LAYERS TO SUM OVER
            HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
         ELSE
          II=IBTOP(N)
          DO 40 K=II,KLAY
            J=JIKHYDSUB(1,N)
            I=JIKHYDSUB(2,N)
            W1=HYDSUBWT(1,N)
            W2=HYDSUBWT(2,N)
            W3=HYDSUBWT(3,N)
            W4=HYDSUBWT(4,N)
            icell1 = ((K-1)*nrow*ncol)+((I-1)*ncol)+J
            icell2 = ((K-1)*nrow*ncol)+((I-1)*ncol)+(J+1)
            icell3 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J+1)
            icell4 = ((K-1)*nrow*ncol)+((I-2)*ncol)+(J)
            if(icell3.le.0.or.icell3.gt.(K*nrow*ncol))icell3=icell1
            if(icell4.le.0.or.icell3.gt.(K*nrow*ncol))icell4=icell1
            IF(HYDSUBARR(N).EQ.'SB'.and.IBTOT.gt.0)TOTL=TOTL+
     1 SUB(icell1)*W1+SUB(icell2)*W2+SUB(icell3)*W3+SUB(icell4)*W4
            IF(HYDSUBARR(N).EQ.'SE'.and.IBTOT.gt.0)TOTL=TOTL+
     1 SUBE(icell1)*W1+SUBE(icell2)*W2+SUBE(icell3)*W3+SUBE(icell4)*W4
            IF(HYDSUBARR(N).EQ.'SV'.and.IBTOT.gt.0)TOTL=TOTL+
     1 SUBV(icell1)*W1+SUBV(icell2)*W2+SUBV(icell3)*W3+SUBV(icell4)*W4
   40    CONTINUE
         HYDVAL(NHYDTOT+N,IHYDLOC)=TOTL 
        ENDIF                      
       ENDIF
   50 CONTINUE
!seb      NHYDSUB=NHYDSUB-IK
      NHYDTOT=NHYDTOT+NHYDSUB
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7STR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT,HNEW,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
      USE HYDSTRMODULE
      USE GWFSTRMODULE, ONLY: ISTRM,STRM,NSTREM
      CHARACTER HYDSTRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7STR7PNT(IGRID)
C
C ------Return if no STR hydrographs.
      IF(NHYDSTR.LE.0) RETURN
C
C ------Calculate value for each stream hydrograph.
      DO 50 N=1,NHYDSTR
      ISTR=ISTRHYD(N)
C
C ------Check for the hydrograph type.
cc    ARR/IRR designate the streamflow option with 
cc    ST==> stream stage, SO==> out of reach,
cc    SI==> into reach, and SA==> into aquifer.
      IF(HYDSTRARR(N).EQ.'ST') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(2,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SO') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(9,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SI') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(10,ISTR)
      ELSE IF(HYDSTRARR(N).EQ.'SA') THEN
         K=ISTRM(1,ISTR)
         I=ISTRM(2,ISTR)
         J=ISTRM(3,ISTR)
         IF(    IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).EQ.0 ) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
        ELSEIF (IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).NE.0 .AND. 
     +                        HNEW(J,I,K) .GT. BOTM(J,I,LBOTM(K))) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISTR)
         ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH
         END IF
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSTR
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7SFR7SE(IHYDLOC,IGRID)
C     ******************************************************************
C     COMPUTE HYDROGRAPH RECORDS FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IBOUND,IOUT,HNEW,BOTM,LBOTM,LAYHDT
      USE HYDBASMODULE
      USE HYDSFRMODULE
      USE GWFSFRMODULE, ONLY: ISTRM,STRM,NSTRM
      CHARACTER HYDSFRLBL*20,LINE*80,INTYP*1
C     ------------------------------------------------------------------
      CALL SGWF2HYD7SFR7PNT(IGRID)
C
C ------Return if no SFR hydrographs.
      IF(NHYDSFR.LE.0) RETURN
C
C ------Calculate value for each stream hydrograph.
      DO 50 N=1,NHYDSFR
      ISFR=ISFRHYD(N)
C
C ------Check for the hydrograph type.
cc    ARR/IRR designate the streamflow option with 
cc    ST==> stream stage, SO==> out of reach,
cc    SI==> into reach, and SA==> into aquifer.
      IF(HYDSFRARR(N).EQ.'ST') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(15,ISFR)        
      ELSE IF(HYDSFRARR(N).EQ.'SO') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(9,ISFR)           
      ELSE IF(HYDSFRARR(N).EQ.'SI') THEN
         HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(10,ISFR)            
      ELSE IF(HYDSFRARR(N).EQ.'SA') THEN
         K=ISTRM(1,ISFR)
         I=ISTRM(2,ISFR)
         J=ISTRM(3,ISFR)
         IF(    IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).EQ.0) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISFR)          
        ELSEIF (IBOUND(J,I,K).NE.0 .AND. LAYHDT(K).NE.0 .AND. 
     +                        HNEW(J,I,K) .GT. BOTM(J,I,LBOTM(K)) ) THEN
           HYDVAL(NHYDTOT+N,IHYDLOC)=STRM(11,ISFR)          
         ELSE
           HYDVAL(NHYDTOT+N,IHYDLOC)=HYDNOH                    
         END IF
      ENDIF
   50 CONTINUE
      NHYDTOT=NHYDTOT+NHYDSFR
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2HYD7BAS7OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     WRITE HYDROGRAPH DATA FOR ONE TIME STEP
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:ITMUNI,IOUT
      USE GLOBAL,   ONLY:LSTCHK
      USE GWFBASMODULE, ONLY: TOTIM
      USE HYDBASMODULE
C     ------------------------------------------------------------------
      CALL SGWF2HYD7BAS7PNT(IGRID)
C
C1------RETURN IF NO HYDROGRAPH RECORDS.
      IF(NHYDTOT.LE.0) RETURN
C
C2------IF THIS IS THE FIRST TIME IN THE SIMULATION, WRITE HEADER RECORD.
      IF(KPER.EQ.1 .AND. KSTP.EQ.1) THEN
       IF(LSTCHK(3)) THEN
        WRITE(IOUT,130) NHYDTOT
       ENDIF
 130    FORMAT(/,1X,'A TOTAL OF ',I10,' HYDROGRAPH POINTS HAVE BEEN ',
     1        'PREPARED.')
        NDECDIG=PRECISION(TOTIM)
        IF(NDECDIG.GT.9) THEN
          WRITE(IHYDMUN) -NHYDTOT,ITMUNI
        ELSE
          WRITE(IHYDMUN) NHYDTOT,ITMUNI
        END IF
        WRITE(IHYDMUN) 'TIME',(HYDLBL(N),N=1,NHYDTOT)
        WRITE(IHYDMUN) 0.0,(HYDVAL(N,2),N=1,NHYDTOT)
      ENDIF
C
C3------WRITE HYDROGRAPH RECORD FOR ONE TIME STEP.
      WRITE(IHYDMUN) TOTIM,(HYDVAL(N,1),N=1,NHYDTOT)
C
C4------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2HYD7GRDLOC(XL,YL,NR1,NC1,NR2,NC2,XX1,XX2,YY1,YY2)
C     ******************************************************************
C     LOCATE CELLS FOR HYDROGRAPH POINTS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: NCOL,NROW,DELR,DELC
C     ------------------------------------------------------------------
      ZERO=0.
      HALF=0.5
      XX1=ZERO
      XX2=ZERO
      YY1=ZERO
      YY2=ZERO
      X1=ZERO
      IF(XL.LT.X1) THEN
         NC1=0
         NC2=0
         GO TO 100
      ENDIF
      XCB=ZERO
      XCF=DELR(1)*HALF
      DO 10 N=1,NCOL
      X2=X1+DELR(N)
      XC=XCF
      DXF=ZERO
      IF(N.LT.NCOL) DXF=DELR(N+1)*HALF
      XCF=X2+DXF
      IF(XL.LE.X2) THEN
         NC1=N
         IF(XL.LT.XC) THEN
            NC2=N-1
            XX1=XCB
            XX2=XC
         ELSE
            NC2=N
            XX1=XC
            XX2=XCF
         ENDIF
         GO TO 100
      ENDIF
      X1=X2
      XCB=XC
 10   CONTINUE
      NC1=NCOL+1
      NC2=NCOL+1
 100  Y1=ZERO
      YCB=ZERO
      YCF=DELC(NROW)*HALF
      IF(YL.LT.Y1) THEN
         NR1=NROW+1
         NR2=NROW+1
         RETURN
      ENDIF
      DO 110 N=NROW,1,-1
      Y2=Y1+DELC(N)
      YC=YCF
      DYF=ZERO
      IF(N.GT.1) DYF=DELC(N-1)*HALF
      YCF=Y2+DYF
      IF(YL.LE.Y2) THEN
         NR1=N
         IF(YL.LT.YC) THEN
            NR2=N+1
            YY1=YCB
            YY2=YC
         ELSE
            NR2=N
            YY1=YC
            YY2=YCF
         ENDIF
         RETURN
      ENDIF
      Y1=Y2
      YCB=YC
 110  CONTINUE
      NC1=0
      NC2=0
      RETURN
      END
      FUNCTION SHYD7WTAVG(N)
C     ******************************************************************
C     COMPUTE WEIGHTED AVERAGE OF HEAD
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY: NCOL,NROW,NLAY,HNEW
      USE HYDBASMODULE, ONLY: JIKHYDBAS,HYDBASWT
      DOUBLE PRECISION W1,W2,W3,W4,HTOT
C     ------------------------------------------------------------------
      J=JIKHYDBAS(1,N)
      I=JIKHYDBAS(2,N)
      K=JIKHYDBAS(3,N)
      W1=HYDBASWT(1,N)
      W2=HYDBASWT(2,N)
      W3=HYDBASWT(3,N)
      W4=HYDBASWT(4,N)
      HTOT=HNEW(J,I,K)*W1
      if(W2.gt.0.)HTOT=HTOT+HNEW(J+1,I,K)*W2
      if(W3.gt.0.)HTOT=HTOT+HNEW(J+1,I-1,K)*W3
      if(W4.gt.0.)HTOT=HTOT+HNEW(J,I-1,K)*W4
      SHYD7WTAVG=HTOT
      RETURN
      END
      SUBROUTINE SGWF2HYD7MW(X0,Y0,X1,X2,Y1,Y2,W1,W2,W3,W4)
C     ******************************************************************
C     COMPUTE WEIGHTS FOR BILINEAR INTERPOLATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DX=(X0-X1)/(X2-X1)
      DY=(Y0-Y1)/(Y2-Y1)
      DXY=DX*DY
      W1=1-DX-DY+DXY
      W2=DX-DXY
      W3=DXY
      W4=DY-DXY
      RETURN
      END
      SUBROUTINE GWF2HYD7DA(IGRID)
C     ******************************************************************
C     SUBROUTINE TO DEALLOCATE ALL HYDMOD VARIABLES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY: IUNIT
C     ------------------------------------------------------------------
C1------DEALLOCATE HYDBAS VARIABLES      
      CALL SGWF2HYD7BAS7DA(IGRID)
C      
C2------CHECK IF HYDMOD IS USED WITH OTHER PACKAGES.  IF SO, DEALLOCATE
      IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0) 
     1                   CALL SGWF2HYD7IBS7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0) 
     1                   CALL SGWF2HYD7SUB7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0) 
     1                   CALL SGWF2HYD7STR7DA(IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0) 
     1                   CALL SGWF2HYD7SFR7DA(IGRID)
C3
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7DA(IGRID)
C  Deallocate HYD BAS memory
      USE HYDBASMODULE
      INTEGER:: IERR
C
      DEALLOCATE(HYDBASDAT(IGRID)%NHYDTOT,     STAT = IERR)
      IF(ASSOCIATED(HYDBASDAT(IGRID)%HYDVAL))
     1    DEALLOCATE(HYDBASDAT(IGRID)%HYDVAL)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDLBL,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%IHYDMUN,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%NHYDBAS,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDNOH,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%IBHYDBAS,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%INTRPHYDBAS,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%JIKHYDBAS,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDBASWT,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDBASSTRT,     STAT = IERR)
      DEALLOCATE(HYDBASDAT(IGRID)%HYDBASARR,     STAT = IERR)
C
C NULLIFY LOCAL POINTERS
      IF(IGRID.EQ.1) THEN
         NHYDTOT    =>NULL()
         HYDVAL     =>NULL()
         HYDLBL     =>NULL()
         IHYDMUN    =>NULL()
         NHYDBAS    =>NULL()
         HYDNOH     =>NULL()
         IBHYDBAS   =>NULL()
         INTRPHYDBAS=>NULL()
         JIKHYDBAS  =>NULL()
         HYDBASWT   =>NULL()
         HYDBASSTRT =>NULL()
         HYDBASARR  =>NULL()
      END IF    

      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PNT(IGRID)
C  Change HYD BAS data to a different grid.
      USE HYDBASMODULE
C
      NHYDTOT=>HYDBASDAT(IGRID)%NHYDTOT
      HYDVAL=>HYDBASDAT(IGRID)%HYDVAL
      HYDLBL=>HYDBASDAT(IGRID)%HYDLBL
      IHYDMUN=>HYDBASDAT(IGRID)%IHYDMUN
      NHYDBAS=>HYDBASDAT(IGRID)%NHYDBAS
      HYDNOH=>HYDBASDAT(IGRID)%HYDNOH
      IBHYDBAS=>HYDBASDAT(IGRID)%IBHYDBAS
      INTRPHYDBAS=>HYDBASDAT(IGRID)%INTRPHYDBAS
      JIKHYDBAS=>HYDBASDAT(IGRID)%JIKHYDBAS
      HYDBASWT=>HYDBASDAT(IGRID)%HYDBASWT
      HYDBASSTRT=>HYDBASDAT(IGRID)%HYDBASSTRT
      HYDBASARR=>HYDBASDAT(IGRID)%HYDBASARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7BAS7PSV(IGRID)
C  Save HYD BAS data for a grid.
      USE HYDBASMODULE
C
      HYDBASDAT(IGRID)%NHYDTOT=>NHYDTOT
      HYDBASDAT(IGRID)%HYDVAL=>HYDVAL
      HYDBASDAT(IGRID)%HYDLBL=>HYDLBL
      HYDBASDAT(IGRID)%IHYDMUN=>IHYDMUN
      HYDBASDAT(IGRID)%NHYDBAS=>NHYDBAS
      HYDBASDAT(IGRID)%HYDNOH=>HYDNOH
      HYDBASDAT(IGRID)%IBHYDBAS=>IBHYDBAS
      HYDBASDAT(IGRID)%INTRPHYDBAS=>INTRPHYDBAS
      HYDBASDAT(IGRID)%JIKHYDBAS=>JIKHYDBAS
      HYDBASDAT(IGRID)%HYDBASWT=>HYDBASWT
      HYDBASDAT(IGRID)%HYDBASSTRT=>HYDBASSTRT
      HYDBASDAT(IGRID)%HYDBASARR=>HYDBASARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7DA(IGRID)
C  Deallocate HYD IBS memory
      USE HYDIBSMODULE
C
      DEALLOCATE(HYDIBSDAT(IGRID)%NHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%IBHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%INTRPHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%JIKHYDIBS)
      DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSWT)
      DEALLOCATE(HYDIBSDAT(IGRID)%HYDIBSARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PNT(IGRID)
C  Change HYD IBS data to a different grid.
      USE HYDIBSMODULE
C
      NHYDIBS=>HYDIBSDAT(IGRID)%NHYDIBS
      IBHYDIBS=>HYDIBSDAT(IGRID)%IBHYDIBS
      INTRPHYDIBS=>HYDIBSDAT(IGRID)%INTRPHYDIBS
      JIKHYDIBS=>HYDIBSDAT(IGRID)%JIKHYDIBS
      HYDIBSWT=>HYDIBSDAT(IGRID)%HYDIBSWT
      HYDIBSARR=>HYDIBSDAT(IGRID)%HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7IBS7PSV(IGRID)
C  Save HYD IBS data for a grid.
      USE HYDIBSMODULE
C
      HYDIBSDAT(IGRID)%NHYDIBS=>NHYDIBS
      HYDIBSDAT(IGRID)%IBHYDIBS=>IBHYDIBS
      HYDIBSDAT(IGRID)%INTRPHYDIBS=>INTRPHYDIBS
      HYDIBSDAT(IGRID)%JIKHYDIBS=>JIKHYDIBS
      HYDIBSDAT(IGRID)%HYDIBSWT=>HYDIBSWT
      HYDIBSDAT(IGRID)%HYDIBSARR=>HYDIBSARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7DA(IGRID)
C  Deallocate HYD SUB memory
      USE HYDSUBMODULE
      INTEGER:: IERR
C
      DEALLOCATE(HYDSUBDAT(IGRID)%NHYDSUB,     STAT = IERR)
      IF(ASSOCIATED (HYDSUBDAT(IGRID)%IBHYDSUB))  !rth=> deallocation error needs fixing
     & DEALLOCATE(HYDSUBDAT(IGRID)%IBHYDSUB)
      DEALLOCATE(HYDSUBDAT(IGRID)%INTRPHYDSUB,     STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%JIKHYDSUB,     STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%HYDSUBWT,     STAT = IERR)
      DEALLOCATE(HYDSUBDAT(IGRID)%HYDSUBARR,     STAT = IERR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PNT(IGRID)
C  Change HYD SUB data to a different grid.
      USE HYDSUBMODULE
C
      NHYDSUB=>HYDSUBDAT(IGRID)%NHYDSUB
      IBHYDSUB=>HYDSUBDAT(IGRID)%IBHYDSUB
      INTRPHYDSUB=>HYDSUBDAT(IGRID)%INTRPHYDSUB
      JIKHYDSUB=>HYDSUBDAT(IGRID)%JIKHYDSUB
      HYDSUBWT=>HYDSUBDAT(IGRID)%HYDSUBWT
      HYDSUBARR=>HYDSUBDAT(IGRID)%HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SUB7PSV(IGRID)
C  Save HYD SUB data for a grid.
      USE HYDSUBMODULE
C
      HYDSUBDAT(IGRID)%NHYDSUB=>NHYDSUB
      HYDSUBDAT(IGRID)%IBHYDSUB=>IBHYDSUB
      HYDSUBDAT(IGRID)%INTRPHYDSUB=>INTRPHYDSUB
      HYDSUBDAT(IGRID)%JIKHYDSUB=>JIKHYDSUB
      HYDSUBDAT(IGRID)%HYDSUBWT=>HYDSUBWT
      HYDSUBDAT(IGRID)%HYDSUBARR=>HYDSUBARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7DA(IGRID)
C  Deallocate HYD STR memory
      USE HYDSTRMODULE
C
      DEALLOCATE(HYDSTRDAT(IGRID)%NHYDSTR)
      DEALLOCATE(HYDSTRDAT(IGRID)%ISTRHYD)
      DEALLOCATE(HYDSTRDAT(IGRID)%HYDSTRARR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PNT(IGRID)
C  Change HYD STR data to a different grid.
      USE HYDSTRMODULE
C
      NHYDSTR=>HYDSTRDAT(IGRID)%NHYDSTR
      ISTRHYD=>HYDSTRDAT(IGRID)%ISTRHYD
      HYDSTRARR=>HYDSTRDAT(IGRID)%HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7STR7PSV(IGRID)
C  Save HYD STR data for a grid.
      USE HYDSTRMODULE
C
      HYDSTRDAT(IGRID)%NHYDSTR=>NHYDSTR
      HYDSTRDAT(IGRID)%ISTRHYD=>ISTRHYD
      HYDSTRDAT(IGRID)%HYDSTRARR=>HYDSTRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7DA(IGRID)
C  Deallocate HYD SFR memory
      USE HYDSFRMODULE
      INTEGER IERR
C
      DEALLOCATE(HYDSFRDAT(IGRID)%NHYDSFR,   STAT=IERR)
      DEALLOCATE(HYDSFRDAT(IGRID)%ISFRHYD,   STAT=IERR)
      DEALLOCATE(HYDSFRDAT(IGRID)%HYDSFRARR,   STAT=IERR)
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PNT(IGRID)
C  Change HYD SFR data to a different grid.
      USE HYDSFRMODULE
C
      NHYDSFR=>HYDSFRDAT(IGRID)%NHYDSFR
      ISFRHYD=>HYDSFRDAT(IGRID)%ISFRHYD
      HYDSFRARR=>HYDSFRDAT(IGRID)%HYDSFRARR
C
      RETURN
      END
      SUBROUTINE SGWF2HYD7SFR7PSV(IGRID)
C  Save HYD SFR data for a grid.
      USE HYDSFRMODULE
C
      HYDSFRDAT(IGRID)%NHYDSFR=>NHYDSFR
      HYDSFRDAT(IGRID)%ISFRHYD=>ISFRHYD
      HYDSFRDAT(IGRID)%HYDSFRARR=>HYDSFRARR
C
      RETURN
      END
