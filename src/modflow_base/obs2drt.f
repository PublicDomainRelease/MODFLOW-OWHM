      MODULE OBSDRTMODULE
         USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE
         PRIVATE:: GENERIC_OUTPUT_FILE
         INTEGER, SAVE, POINTER  ::NQDR,NQCDR,NQTDR,IUDRTOBSV,IPRT
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQOBDR
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQCLDR
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER,CONTIGUOUS::OBSNAM
         LOGICAL,     SAVE,DIMENSION(:),POINTER,CONTIGUOUS::SKIP_OBS
         !INTEGER, SAVE,POINTER ::FN_PRN_ALL,FN_PRN
         TYPE(GENERIC_OUTPUT_FILE), SAVE,POINTER:: FN_PRN, FN_PRN_ALL
      TYPE OBSDRTTYPE
         INTEGER, POINTER  ::NQDR,NQCDR,NQTDR,IUDRTOBSV,IPRT
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQOBDR
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQCLDR
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,        DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,DIMENSION(:),   POINTER,CONTIGUOUS::OBSNAM
         LOGICAL,     DIMENSION(:),   POINTER,CONTIGUOUS::SKIP_OBS
         !INTEGER,  POINTER    ::FN_PRN_ALL,FN_PRN
         TYPE(GENERIC_OUTPUT_FILE),POINTER:: FN_PRN, FN_PRN_ALL
      END TYPE
      TYPE(OBSDRTTYPE),  SAVE   ::OBSDRTDAT(10)
      END MODULE
C  NQDR -- number of cell groups
C  NQCDR -- total number of cells in all groups
C  NQTDR -- total number of observations -- sum of the number of times for each group
C  NQOBDR(NQDR) -- The number of observations in each observation group
C  NQCLDR(NQDR) -- The number of cells in each observation group
C  IOBTS(NQTDR) -- Observation time step
C  FLWSIM(NQTDR) -- Simulated value
C  FLWOBS(NQTDR) -- Observed value
C  TOFF(NQTDR) -- Fractional offset between time steps
C  OTIME(NQTDR) -- 
C  QCELL(4,NQCDR) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2DRT7AR(IUDRTOB,IUDRT,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT DRAIN CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE OBSDRTMODULE
      CHARACTER*700 LINE
      CHARACTER(10)::DATE
      CHARACTER(13)::DYEAR
C     ------------------------------------------------------------------
C
      ALLOCATE(NQDR,NQCDR,NQTDR,IUDRTOBSV,IPRT)
      ALLOCATE(FN_PRN_ALL,FN_PRN)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
      !FN_PRN_ALL=0
      !FN_PRN=0
C
C     IDENTIFY PROCESS AND PACKAGE
        WRITE(IOUT,7) IUDRTOB
    7 FORMAT(/,' OBS2DRT7 -- OBSERVATION PROCESS (DRT FLOW ',
     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFDRT is not active
      IF (IUDRT.EQ.0) THEN
          WRITE (IOUT,29 )
   29   FORMAT (/,' DRT DRAIN PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUDRTOB,IOUT,LINE)
      ! CHECK FOR POTENTIAL KEYWORDS
      LLOC = 1
      DO
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IUDRTOB)
         IF    (LINE(ISTART:ISTOP)=='TIME_STEP_PRINT') THEN
             CALL FN_PRN%OPEN(LINE,LLOC,IOUT,IUDRTOB,NOBINARY=.TRUE.)
             !CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,DUM,IOUT,IUDRTOB)
             !READ(LINE(ISTART:ISTOP),*,IOSTAT=IERR) FN_PRN
             !IF(IERR.NE.0) THEN
             !    OPEN(NEWUNIT=FN_PRN,    FILE=LINE(ISTART:ISTOP),
     +       !        ACTION='WRITE',POSITION='REWIND', STATUS='REPLACE')
             !END IF
            CALL URDCOM(IUDRTOB,IOUT,LINE)
         ELSEIF(LINE(ISTART:ISTOP)=='TIME_STEP_PRINT_ALL') THEN
            CALL FN_PRN_ALL%OPEN(LINE,LLOC,IOUT,IUDRTOB,NOBINARY=.TRUE.,
     +                            NO_INTERNAL=.TRUE.)
             !CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,DUM,IOUT,IUDRTOB)
             !READ(LINE(ISTART:ISTOP),*,IOSTAT=IERR) FN_PRN_ALL
             !IF(IERR.NE.0) THEN
             !    OPEN(NEWUNIT=FN_PRN_ALL,FILE=LINE(ISTART:ISTOP),
     +       !        ACTION='WRITE',POSITION='REWIND', STATUS='REPLACE')
             !END IF
            CALL URDCOM(IUDRTOB,IOUT,LINE)
         ELSE
             EXIT
         END IF
      END DO
      !
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQDR,DUM,IOUT,IUDRTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCDR,DUM,IOUT,IUDRTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTDR,DUM,IOUT,IUDRTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDRTOBSV,DUM,IOUT,IUDRTOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IUDRTOB)
      IPRT=1
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        IPRT=0
        WRITE(IOUT,*) 'NOPRINT option for DRT OBSERVATIONS'
      END IF
        WRITE (IOUT,9) NQDR, NQCDR, NQTDR
    9 FORMAT (/,
     &     ' NUMBER OF FLOW-OBSERVATION DRT-CELL GROUPS.....: ',I6,/,
     &     '   NUMBER OF CELLS IN DRT-CELL GROUPS...........: ',I6,/,
     &     '   NUMBER OF DRT-CELL FLOWS.....................: ',I6)
      IF(NQTDR.LE.0) THEN
           WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL',
     1       ' TO 0'
         CALL USTOP(' ')
      END IF
      IF(IUDRTOBSV.GT.0) THEN
           WRITE(IOUT,21) IUDRTOBSV
   21    FORMAT(1X,
     1      'DRT OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
      ELSE
           WRITE(IOUT,22)
   22    FORMAT(1X,'DRT OBSERVATIONS WILL NOT BE SAVED IN A FILE')
      END IF
C
Cx------Allocate memory
      ALLOCATE(SKIP_OBS(NQTDR), SOURCE=.FALSE.)                          !seb
      ALLOCATE(NQOBDR(NQDR))
      ALLOCATE(NQCLDR(NQDR))
      ALLOCATE(IOBTS(NQTDR))
      ALLOCATE(FLWSIM(NQTDR))
      ALLOCATE(FLWOBS(NQTDR))
      ALLOCATE(TOFF(NQTDR))
      ALLOCATE(OTIME(NQTDR))
      ALLOCATE(QCELL(4,NQCDR))
      ALLOCATE(OBSNAM(NQTDR))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTDR
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IUDRTOB,*) TOMULTDR
      IF(IPRT.NE.0) THEN
        WRITE (IOUT,20) TOMULTDR
      ENDIF
   20 FORMAT (/,' OBSERVED DRT-CELL FLOW DATA',/,' -- TIME OFFSETS',
     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQDR
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUDRTOB,*) NQOBDR(IQ), NQCLDR(IQ)
        IF(IPRT.NE.0) THEN
          WRITE (IOUT,25) IQ, 'DRT', NQCLDR(IQ), NQOBDR(IQ)
        ENDIF
   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
     &  40X,'OBSERVED',/,
     &  20X,'REFER.',13X,'DRAIN FLOW',/,
     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLDR(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLDR(IQ) = -NQCLDR(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBDR(IQ)
        DO 30 J = NT1, NT2
          READ (IUDRTOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          IF(IPRT.NE.0) THEN
            WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
          ENDIF
   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTDR,TSMULT,1,
     &                OTIME(J),SKIP_OBS(J),DATE,DYEAR)
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLDR(IQ)
        IF(IPRT.NE.0) THEN
          WRITE (IOUT,54)
        ENDIF
   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUDRTOB,*) (QCELL(I,L),I=1,4)
          IF(IFCTFLG.EQ.1) QCELL(4,L) = 1.
          IF(IPRT.NE.0) THEN
            WRITE (IOUT,55) (QCELL(I,L),I=1,4)
          ENDIF
   55     FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
          I = QCELL(2,L)
          J = QCELL(3,L)
          IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
              WRITE (IOUT,59)
   59       FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &          ' -- STOP EXECUTION (OBS2DRT7RP)',/)
            IERR = 1
          ENDIF
  100   CONTINUE
C
Cx------END OF INPUT FOR ONE CELL GROUP -- UPDATE COUNTERS.
        NC = NC2
        NT = NT2
  200 CONTINUE
C
C
      IF (IERR.GT.0) THEN
          WRITE(IOUT,620)
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS2DRT7RP)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2DRT7PSV(IGRID)
      RETURN
      END
      SUBROUTINE OBS2DRT7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE DRAIN
C     PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,HNEW,IBOUND
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF
      USE OBSBASMODULE,ONLY:ITS
      USE OBSDRTMODULE
      DOUBLE PRECISION HHNEW, HB, C
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
      CALL SOBS2DRT7PNT(IGRID)
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C
Cx------JRBOT IS FLAG FOR PRINTING THE HEADING FOR CELLS THAT ARE NOT
Cx------HEAD DEPENDENT.  THE FLAG IS USED TO PRINT THIS HEADING ONLY ONCE.
      JRBOT = 0
C
C-------LOOP THROUGH CELL GROUPS
      DO 800 IQ = 1, NQDR
        NT2 = NT1 + NQOBDR(IQ) - 1
C
Cx------LOOK THROUGH ALL OBSERVATIONS FOR THIS GROUP TO SEE FIND OBSERVATIONS
Cx------FOR THE CURRENT TIME STEP.
        DO 600 NT = NT1, NT2
          IF(SKIP_OBS(NT)) THEN
              WRITE (IOUT,70) NT, OBSNAM(NT)
  70          FORMAT (/,' DRT  OBS#',I5,', ID ',A,' IS BEING SKIPPED',
     &          ' DUE TO NOT BEING WITHIN SIMULATED TIME (OBS2DRT7SE)')
              CYCLE
          END IF
          IF (IOBTS(NT).EQ.ITS .OR.
     &        (IOBTS(NT).EQ.ITS-1.AND.TOFF(NT).GT.ZERO)) THEN
C
Cx------FOUND AN OBSERVATION FOR CURRENT TIME STEP.
Cx------INITIALIZE NUMBER OF DRY CELLS IN OBSERVATION (KRBOT) AND
Cx------NUMBER OF NON-HEAD-DEPENDENT CELLS (IRBOT).
            IRBOT = 0
            KRBOT = 0
C
Cx------LOOP THROUGH CELLS IN THE CELL GROUP
            NC1 = NC + 1
            NC2 = NC + NQCLDR(IQ)
            NB = 0
            DO 400 N = NC1, NC2
              K = QCELL(1,N)
              I = QCELL(2,N)
              J = QCELL(3,N)
C
Cx------LOOP THROUGH ACTIVE DRAIN REACHES TO FIND A MATCH.
              DO 100 MNB = 1, NDRTCL
                NB = NB + 1
                IF (NB.GT.NDRTCL) NB = 1
                KK = DRTF(1,NB)
                II = DRTF(2,NB)
                JJ = DRTF(3,NB)
C
Cx------DO SIMULATED EQUIVALENT CALCULATIONS IF THIS IS A MATCH.
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
C
Cx------CHECK IF THE MATCHED REACH IS IN A DRY CELL.
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 400
                  ENDIF
C
Cx------COMPUTE FLOW FOR THE REACH.
                  HHNEW = HNEW(J,I,K)
                  HB = DRTF(4,NB)
                  C = DRTF(5,NB)
                  HH = C*(HB-HHNEW)
                  IF(HHNEW.LE.HB) THEN
                    HH = ZERO
                      IF (JRBOT.EQ.0) WRITE (IOUT,83 )
   83                 FORMAT (/,
     &       ' HEADS AT DRT DRAIN CELLS ARE BELOW THE',
     &       ' BOTTOM OF THE DRT DRAIN BED AT THE CELLS LISTED',/,
     &       ' BELOW.  THESE CONDITIONS DIMINISH THE IMPACT',
     &       ' OF THE OBSERVATION ON ESTIMATES OF',/,
     &       ' ALL PARAMETERS EXCEPT THOSE THAT CONTROL THE HYDRAULIC',
     &        ' CONDUCTIVITY OF THE',/,
     &        ' DRT DRAIN BED.  (SEE TEXT FOR MORE INFORMATION).')
                    JRBOT = 1
                    IF (IRBOT.EQ.0) THEN
                        WRITE (IOUT,92 ) NT, OBSNAM(NT), ITS
   92                 FORMAT (/,' OBS# ',I6,', ID ',A,', TIME STEP ',I5)
                        WRITE (IOUT,93 )
   93                 FORMAT ('    LAYER   ROW  COLUMN')
                    ENDIF
                    IRBOT = IRBOT + 1
                      WRITE (IOUT,97 ) K, I, J
   97               FORMAT(3I7)
                  ENDIF
C
Cx------CALCULATE THE FACTOR FOR TEMPORAL INTERPOLATION.
                  FACT = 1.0
                  IF (TOFF(NT).GT.ZERO) THEN
                    IF (IOBTS(NT).EQ.ITS) FACT = 1. - TOFF(NT)
                    IF (IOBTS(NT).EQ.ITS-1) FACT = TOFF(NT)
                  ENDIF
C
Cx------ADD FLOW FOR THE REACH TO THE SIMULATED EQUIVALENT.
Cx------QCELL(4,N) IS THE PORTION FACTOR.
                  FLWSIM(NT) = FLWSIM(NT) + HH*FACT*QCELL(4,N)
                  GO TO 400
                ENDIF
  100         CONTINUE
C
Cx------LOOKED THROUGH ENTIRE LIST OF ACTIVE DRAIN REACHES WITHOUT
Cx------FINDING OBSERVATION CELL.  STOP.
                WRITE (IOUT,140) N, IQ, OBSNAM(NT),K,I,J
  140 FORMAT  (' CELL ',I6,
     1         ' OF DRT DRAIN OBSERVATION CELL GROUP',I5,/,
     2         ' NOT FOUND IN CELLS LISTED FOR DRT PACKAGE',/,
     3         ' OBSERVATION NAME:',A,/,
     4         ' CELL LAYER, ROW, AND COLUMN:',3I8,/,
     5         '  -- STOP EXECUTION (OBS2DRT7SE)')
                CALL USTOP(' ')
C
Cx------END OF LOOP FOR THE CELLS IN ONE CELL GROUP FOR ONE OBSERVATION TIME..
  400       CONTINUE
C
C-------PRINT NUMBER OF CELLS AT WHICH HEAD IS BELOW THE BOTTOM OF THE
C-------DRAIN BED; CHECK FOR ALL CELLS IN OBSERVATION BEING DRY.
              IF(IRBOT.GT.0) WRITE (IOUT,530) IRBOT, NQCLDR(IQ)
  530           FORMAT (I7,' OF THE',I7,' CELLS USED TO SIMULATE THE',
     &            ' GAIN OR LOSS ARE',/,22X,'AFFECTED.')
            IF(KRBOT.EQ.NQCLDR(IQ)) THEN
                WRITE (IOUT,535)
  535         FORMAT(' ALL CELLS INCLUDED IN THIS OBSERVATION ARE DRY')
            ENDIF
          ENDIF
C
Cx------END OF LOOP FOR OBSERVATION TIMES IN ONE CELL GROUP
  600   CONTINUE
C
C-------UPDATE COUNTERS
  700   NC = NC + NQCLDR(IQ)
        NT1 = NT2 + 1
C
Cx------END OF LOOP FOR ALL CELL GROUPS.
  800 CONTINUE
C
      IF(FN_PRN_ALL%IS_OPEN) THEN
         CALL UOBSSV(FN_PRN_ALL%IU,NQTDR,FLWSIM,
     1                              FLWOBS,OBSNAM,0)
      END IF
Cx------RETURN
      RETURN
      END
      SUBROUTINE OBS2DRT7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE OBSDRTMODULE
      DOUBLE PRECISION SQ,SUMSQ
C     ------------------------------------------------------------------
      CALL SOBS2DRT7PNT(IGRID)
C
C1------WRITE OBSERVATIONS TO LISTING FILE.
      IF(IPRT.NE.0) THEN
        WRITE(IOUT,17)
      ENDIF
   17 FORMAT(1X,/,1X,'DRT FLOW OBSERVATIONS  ',/,
     1  1X,'OBSERVATION     OBSERVED      SIMULATED',/
     2  1X,'  NAME              VALUE              VALUE',
     3     '             DIFFERENCE',/
     4  1X,'----------------------------------------------',
     5     '----------------------')
      SUMSQ=0.
      DO 100 N=1,NQTDR
      DIFF=FLWOBS(N)-FLWSIM(N)
      SQ=DIFF*DIFF
      SUMSQ=SUMSQ+SQ
      IF(IPRT.NE.0) THEN
        WRITE(IOUT,27) OBSNAM(N),FLWOBS(N),FLWSIM(N),DIFF
      ENDIF
   27 FORMAT(1X,A,1P,3G20.11)
  100 CONTINUE
        WRITE(IOUT,28) SUMSQ
   28 FORMAT(1X,/,1X,'DRT FLOW SUM OF SQUARED DIFFERENCE:',1P,E15.5)
C
C2------WRITE OBSERVATIONS TO SEPARATE FILE.
      IF(IUDRTOBSV.GT.0) CALL UOBSSV(IUDRTOBSV,NQTDR,FLWSIM,
     1                              FLWOBS,OBSNAM,0)
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE OBS2DRT7DA(IGRID)
C  Deallocate OBSDRT memory
      USE OBSDRTMODULE
C
      DEALLOCATE(OBSDRTDAT(IGRID)%NQDR)
      DEALLOCATE(OBSDRTDAT(IGRID)%NQCDR)
      DEALLOCATE(OBSDRTDAT(IGRID)%NQTDR)
      DEALLOCATE(OBSDRTDAT(IGRID)%IUDRTOBSV)
      DEALLOCATE(OBSDRTDAT(IGRID)%IPRT)
      DEALLOCATE(OBSDRTDAT(IGRID)%NQOBDR)
      DEALLOCATE(OBSDRTDAT(IGRID)%NQCLDR)
      DEALLOCATE(OBSDRTDAT(IGRID)%IOBTS)
      DEALLOCATE(OBSDRTDAT(IGRID)%FLWSIM)
      DEALLOCATE(OBSDRTDAT(IGRID)%FLWOBS)
      DEALLOCATE(OBSDRTDAT(IGRID)%TOFF)
      DEALLOCATE(OBSDRTDAT(IGRID)%OTIME)
      DEALLOCATE(OBSDRTDAT(IGRID)%QCELL)
      DEALLOCATE(OBSDRTDAT(IGRID)%OBSNAM)
      DEALLOCATE(OBSDRTDAT(IGRID)%SKIP_OBS)
      ! GFORTRAN compiler error work-around for pointer data type FINAL statement
      FN_PRN_ALL=>OBSDRTDAT(IGRID)%FN_PRN_ALL
      OBSDRTDAT(IGRID)%FN_PRN_ALL=>NULL()
      DEALLOCATE(FN_PRN_ALL)
      FN_PRN_ALL=>NULL()
      !
      FN_PRN=>OBSDRTDAT(IGRID)%FN_PRN
      OBSDRTDAT(IGRID)%FN_PRN=>NULL()
      DEALLOCATE(FN_PRN)
      FN_PRN=>NULL()
      !DEALLOCATE(OBSDRTDAT(IGRID)%FN_PRN_ALL)
      !DEALLOCATE(OBSDRTDAT(IGRID)%FN_PRN)
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.NE.1)THEN
        NQDR    =>NULL()
        NQCDR   =>NULL()
        NQTDR   =>NULL()
        IUDRTOBSV=>NULL()
        IUDRTOBSV=>NULL()
        IPRT    =>NULL()
        NQOBDR  =>NULL()
        NQCLDR  =>NULL()
        IOBTS   =>NULL()
        FLWSIM  =>NULL()
        FLWOBS  =>NULL()
        TOFF    =>NULL()
        OTIME   =>NULL()
        QCELL   =>NULL()
        OBSNAM  =>NULL()
        SKIP_OBS=>NULL()
        FN_PRN_ALL=>NULL()
        FN_PRN=>NULL()
      END IF
C
      RETURN
      END
      SUBROUTINE SOBS2DRT7PNT(IGRID)
C  Change OBSDRT data to a different grid.
      USE OBSDRTMODULE
C
      NQDR=>OBSDRTDAT(IGRID)%NQDR
      NQCDR=>OBSDRTDAT(IGRID)%NQCDR
      NQTDR=>OBSDRTDAT(IGRID)%NQTDR
      IUDRTOBSV=>OBSDRTDAT(IGRID)%IUDRTOBSV
      IPRT=>OBSDRTDAT(IGRID)%IPRT
      NQOBDR=>OBSDRTDAT(IGRID)%NQOBDR
      NQCLDR=>OBSDRTDAT(IGRID)%NQCLDR
      IOBTS=>OBSDRTDAT(IGRID)%IOBTS
      FLWSIM=>OBSDRTDAT(IGRID)%FLWSIM
      FLWOBS=>OBSDRTDAT(IGRID)%FLWOBS
      TOFF=>OBSDRTDAT(IGRID)%TOFF
      OTIME=>OBSDRTDAT(IGRID)%OTIME
      QCELL=>OBSDRTDAT(IGRID)%QCELL
      FN_PRN_ALL=>OBSDRTDAT(IGRID)%FN_PRN_ALL
      FN_PRN    =>OBSDRTDAT(IGRID)%FN_PRN
C
      RETURN
      END
      SUBROUTINE SOBS2DRT7PSV(IGRID)
C  Save OBSDRT data for a grid.
      USE OBSDRTMODULE
C
      OBSDRTDAT(IGRID)%NQDR=>NQDR
      OBSDRTDAT(IGRID)%NQCDR=>NQCDR
      OBSDRTDAT(IGRID)%NQTDR=>NQTDR
      OBSDRTDAT(IGRID)%IUDRTOBSV=>IUDRTOBSV
      OBSDRTDAT(IGRID)%IPRT=>IPRT
      OBSDRTDAT(IGRID)%NQOBDR=>NQOBDR
      OBSDRTDAT(IGRID)%NQCLDR=>NQCLDR
      OBSDRTDAT(IGRID)%IOBTS=>IOBTS
      OBSDRTDAT(IGRID)%FLWSIM=>FLWSIM
      OBSDRTDAT(IGRID)%FLWOBS=>FLWOBS
      OBSDRTDAT(IGRID)%TOFF=>TOFF
      OBSDRTDAT(IGRID)%OTIME=>OTIME
      OBSDRTDAT(IGRID)%QCELL=>QCELL
      OBSDRTDAT(IGRID)%OBSNAM=>OBSNAM
      OBSDRTDAT(IGRID)%SKIP_OBS  =>SKIP_OBS
      OBSDRTDAT(IGRID)%FN_PRN_ALL=>FN_PRN_ALL
      OBSDRTDAT(IGRID)%FN_PRN    =>FN_PRN
C
      RETURN
      END
