      MODULE OBSGHBMODULE
         INTEGER, SAVE, POINTER  ::NQGB,NQCGB,NQTGB,IUGBOBSV,IPRT
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQOBGB
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQCLGB
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER,CONTIGUOUS::OBSNAM
      TYPE OBSGHBTYPE
         INTEGER, POINTER  ::NQGB,NQCGB,NQTGB,IUGBOBSV,IPRT
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQOBGB
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQCLGB
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,        DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,DIMENSION(:),   POINTER,CONTIGUOUS::OBSNAM
      END TYPE
      TYPE(OBSGHBTYPE),  SAVE   ::OBSGHBDAT(10)
      END MODULE
C  NQGB -- number of cell groups
C  NQCGB -- total number of cells in all groups
C  NQTGB -- total number of observations -- sum of the number of times for each group
C  NQOBGB(NQGB) -- The number of observations in each observation group
C  NQCLGB(NQGB) -- The number of cells in each observation group
C  IOBTS(NQTGB) -- Observation time step
C  FLWSIM(NQTGB) -- Simulated value
C  FLWOBS(NQTGB) -- Observed value
C  TOFF(NQTGB) -- Fractional offset between time steps
C  OTIME(NQTGB) -- Observation time in model time units
C  QCELL(4,NQCGB) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2GHB7AR(IUGBOB,IUGB,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT GHB CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE GLOBAL,  ONLY:LSTCHK
      USE OBSGHBMODULE
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NQGB,NQCGB,NQTGB,IUGBOBSV,IPRT)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,7) IUGBOB
      ENDIF
    7 FORMAT(/,' OBS2GHB7 -- OBSERVATION PROCESS (GHB FLOW ',
     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Turn off observation package if GWFGHB is not active
      IF (IUGB.EQ.0) THEN
        IF(LSTCHK(1)) THEN
          WRITE (IOUT,29 )
        ENDIF
   29   FORMAT (/,' GHB PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUGBOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGBOBSV,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IUGBOB)
      IPRT=1
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        IPRT=0
        WRITE(IOUT,*) 'NOPRINT option for GHB OBSERVATIONS'
      END IF
      IF(LSTCHK(3)) THEN
        WRITE (IOUT,9) NQGB, NQCGB, NQTGB
      ENDIF
    9 FORMAT (/,
     &     ' NUMBER OF FLOW-OBSERVATION GHB-CELL GROUPS.....: ',I6,/,
     &     '   NUMBER OF CELLS IN GHB-CELL GROUPS...........: ',I6,/,
     &     '   NUMBER OF GHB-CELL FLOWS.....................: ',I6)
      IF(NQTGB.LE.0) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL',
     1        ' TO 0'
         ENDIF
         CALL USTOP(' ')
      END IF
      IF(IUGBOBSV.GT.0) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,21) IUGBOBSV
         ENDIF
   21    FORMAT(1X,
     1      'GHB OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
      ELSE
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,22)
         ENDIF
   22    FORMAT(1X,'GHB OBSERVATIONS WILL NOT BE SAVED IN A FILE')
      END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBGB(NQGB))
      ALLOCATE(NQCLGB(NQGB))
      ALLOCATE(IOBTS(NQTGB))
      ALLOCATE(FLWSIM(NQTGB))
      ALLOCATE(FLWOBS(NQTGB))
      ALLOCATE(TOFF(NQTGB))
      ALLOCATE(OTIME(NQTGB))
      ALLOCATE(QCELL(4,NQCGB))
      ALLOCATE(OBSNAM(NQTGB))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTGB
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IUGBOB,*) TOMULTRV
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE (IOUT,20) TOMULTRV
      ENDIF
   20 FORMAT (/,' OBSERVED GHB-CELL FLOW DATA',/,' -- TIME OFFSETS',
     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQGB
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUGBOB,*) NQOBGB(IQ), NQCLGB(IQ)
        IF(LSTCHK(3).AND.IPRT.NE.0) THEN
          WRITE (IOUT,25) IQ, 'GHB', NQCLGB(IQ), NQOBGB(IQ)
        ENDIF
   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
     &  40X,'OBSERVED',/,
     &  20X,'REFER.',13X,'GHB FLOW',/,
     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLGB(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLGB(IQ) = -NQCLGB(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBGB(IQ)
        DO 30 J = NT1, NT2
          READ (IUGBOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          IF(LSTCHK(3).AND.IPRT.NE.0) THEN
            WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
          ENDIF
   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTRV,TSMULT,1,
     &                OTIME(J))
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLGB(IQ)
        IF(LSTCHK(3).AND.IPRT.NE.0) THEN
          WRITE (IOUT,54)
        ENDIF
   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUGBOB,*) (QCELL(I,L),I=1,4)
          IF(IFCTFLG.EQ.1) QCELL(4,L) = 1.
          IF(LSTCHK(3).AND.IPRT.NE.0) THEN
            WRITE (IOUT,55) (QCELL(I,L),I=1,4)
          ENDIF
   55     FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
          I = QCELL(2,L)
          J = QCELL(3,L)
          IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
            IF(LSTCHK(1)) THEN
              WRITE (IOUT,59)
            ENDIF
   59       FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &          ' -- STOP EXECUTION (OBS2GHB7AR)',/)
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
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,620)
        ENDIF
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS2GHB7AR)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2GHB7PSV(IGRID)
      RETURN
      END
      SUBROUTINE OBS2GHB7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE GHB
C     PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,HNEW,IBOUND
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFGHBMODULE, ONLY:NBOUND,GHBDATA
      USE OBSBASMODULE,ONLY:ITS
      USE OBSGHBMODULE
      DOUBLE PRECISION HHNEW, C, HB , RBOT
C     ------------------------------------------------------------------
      CALL SGWF2GHB7PNT(IGRID)
      CALL SOBS2GHB7PNT(IGRID)
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
      DO 800 IQ = 1, NQGB
        NT2 = NT1 + NQOBGB(IQ) - 1
C
Cx------LOOK THROUGH ALL OBSERVATIONS FOR THIS GROUP TO SEE FIND OBSERVATIONS
Cx------FOR THE CURRENT TIME STEP.
        DO 600 NT = NT1, NT2
          IF (IOBTS(NT).EQ.ITS .OR.
     &        (IOBTS(NT).EQ.ITS-1.AND.TOFF(NT).GT.ZERO)) THEN
C
Cx------FOUND AN OBSERVATION FOR CURRENT TIME STEP.
Cx------INITIALIZE NUMBER OF DRY CELLS IN OBSERVATION (KRBOT)
            KRBOT = 0
C
Cx------LOOP THROUGH CELLS IN THE CELL GROUP
            NC1 = NC + 1
            NC2 = NC + NQCLGB(IQ)
            NB = 0
            DO 400 N = NC1, NC2
              K = QCELL(1,N)
              I = QCELL(2,N)
              J = QCELL(3,N)
C
Cx------LOOP THROUGH ACTIVE GHB CELLS TO FIND A MATCH.
              DO 100 MNB = 1, NBOUND
                NB = NB + 1
                IF (NB.GT.NBOUND) NB = 1
                KK = GHBDATA(NB)%LAY
                II = GHBDATA(NB)%ROW
                JJ = GHBDATA(NB)%COL
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
Cx------COMPUTE FLOW FOR THE BOUNDARY.
                  HHNEW = HNEW(J,I,K)
                  HB = GHBDATA(NB)%VAL(1)
                  C = GHBDATA(NB)%VAL(2)
                  HH = C*(HB-HHNEW)
C
Cx------CALCULATE THE FACTOR FOR TEMPORAL INTERPOLATION.
                  TFACT = 1.0
                  IF (TOFF(NT).GT.ZERO) THEN
                    IF (IOBTS(NT).EQ.ITS) TFACT = 1. - TOFF(NT)
                    IF (IOBTS(NT).EQ.ITS-1) TFACT = TOFF(NT)
                  ENDIF
C
Cx------ADD FLOW FOR THE REACH TO THE SIMULATED EQUIVALENT.
Cx------QCELL(4,N) IS THE PORTION FACTOR.
                  FLWSIM(NT) = FLWSIM(NT) + HH*TFACT*QCELL(4,N)
                  GO TO 400
                ENDIF
  100         CONTINUE
C
Cx------LOOKED THROUGH ENTIRE LIST OF ACTIVE GHB CELLS WITHOUT
Cx------FINDING OBSERVATION CELL.  STOP.
              IF(LSTCHK(1)) THEN
                WRITE (IOUT,140) N, IQ, OBSNAM(NT),K,I,J
              ENDIF
  140 FORMAT  (' CELL ',I6,
     1         ' OF GHB OBSERVATION CELL GROUP',I5,/,
     2         ' NOT FOUND IN CELLS LISTED FOR GHB PACKAGE',/,
     3         ' OBSERVATION NAME:',A,/,
     4         ' CELL LAYER, ROW, AND COLUMN:',3I8,/,
     5         '  -- STOP EXECUTION (OBS2GHB7SE)')
                CALL USTOP(' ')
C
Cx------END OF LOOP FOR THE CELLS IN ONE CELL GROUP FOR ONE OBSERVATION TIME..
  400       CONTINUE
C
C-------CHECK FOR ALL CELLS IN OBSERVATION BEING DRY.
            IF(KRBOT.EQ.NQCLGB(IQ)) THEN
              IF(LSTCHK(2)) THEN
                WRITE (IOUT,535)
              ENDIF
  535         FORMAT(' ALL CELLS INCLUDED IN THIS OBSERVATION ARE DRY')
            ENDIF
          ENDIF
C
Cx------END OF LOOP FOR OBSERVATION TIMES IN ONE CELL GROUP
  600   CONTINUE
C
C-------UPDATE COUNTERS
  700   NC = NC + NQCLGB(IQ)
        NT1 = NT2 + 1
C
Cx------END OF LOOP FOR ALL CELL GROUPS.
  800 CONTINUE
C
Cx------RETURN
      RETURN
      END
      SUBROUTINE OBS2GHB7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE GLOBAL, ONLY:LSTCHK
      USE OBSGHBMODULE
      DOUBLE PRECISION SQ,SUMSQ
C     ------------------------------------------------------------------
      CALL SOBS2GHB7PNT(IGRID)
C
C1------WRITE OBSERVATIONS TO LISTING FILE.
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE(IOUT,17)
      ENDIF
   17 FORMAT(1X,/,1X,'GHB FLOW OBSERVATIONS',/,
     1  1X,'OBSERVATION       OBSERVED           SIMULATED',/
     2  1X,'  NAME              VALUE              VALUE',
     3     '             DIFFERENCE',/
     4  1X,'----------------------------------------------',
     5     '----------------------')
      SUMSQ=0.
      DO 100 N=1,NQTGB
      DIFF=FLWOBS(N)-FLWSIM(N)
      SQ=DIFF*DIFF
      SUMSQ=SUMSQ+SQ
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE(IOUT,27) OBSNAM(N),FLWOBS(N),FLWSIM(N),DIFF
      ENDIF
   27 FORMAT(1X,A,1P,3G20.11)
  100 CONTINUE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,28) SUMSQ
      ENDIF
   28 FORMAT(1X,/,1X,'SUM OF SQUARED DIFFERENCE:',1P,E15.5)
C
C2------WRITE OBSERVATIONS TO SEPARATE FILE.
      IF(IUGBOBSV.GT.0) CALL UOBSSV(IUGBOBSV,NQTGB,FLWSIM,
     1                      FLWOBS,OBSNAM,0)
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE OBS2GHB7DA(IGRID)
C  Deallocate OBSGHB memory
      USE OBSGHBMODULE
C
      DEALLOCATE(OBSGHBDAT(IGRID)%NQGB)
      DEALLOCATE(OBSGHBDAT(IGRID)%NQCGB)
      DEALLOCATE(OBSGHBDAT(IGRID)%NQTGB)
      DEALLOCATE(OBSGHBDAT(IGRID)%IUGBOBSV)
      DEALLOCATE(OBSGHBDAT(IGRID)%IPRT)
      DEALLOCATE(OBSGHBDAT(IGRID)%NQOBGB)
      DEALLOCATE(OBSGHBDAT(IGRID)%NQCLGB)
      DEALLOCATE(OBSGHBDAT(IGRID)%IOBTS)
      DEALLOCATE(OBSGHBDAT(IGRID)%FLWSIM)
      DEALLOCATE(OBSGHBDAT(IGRID)%FLWOBS)
      DEALLOCATE(OBSGHBDAT(IGRID)%TOFF)
      DEALLOCATE(OBSGHBDAT(IGRID)%OTIME)
      DEALLOCATE(OBSGHBDAT(IGRID)%QCELL)
      DEALLOCATE(OBSGHBDAT(IGRID)%OBSNAM)
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.EQ.1)THEN
        NQGB    =>NULL()
        NQCGB   =>NULL()
        NQTGB   =>NULL()
        IUGBOBSV=>NULL()
        IPRT    =>NULL()
        NQOBGB  =>NULL()
        NQCLGB  =>NULL()
        IOBTS   =>NULL()
        FLWSIM  =>NULL()
        FLWOBS  =>NULL()
        TOFF    =>NULL()
        OTIME   =>NULL()
        QCELL   =>NULL()
        OBSNAM  =>NULL()
      END IF
C
      RETURN
      END
      SUBROUTINE SOBS2GHB7PNT(IGRID)
C  Change OBSGHB data to a different grid.
      USE OBSGHBMODULE
C
      NQGB=>OBSGHBDAT(IGRID)%NQGB
      NQCGB=>OBSGHBDAT(IGRID)%NQCGB
      NQTGB=>OBSGHBDAT(IGRID)%NQTGB
      IUGBOBSV=>OBSGHBDAT(IGRID)%IUGBOBSV
      IPRT=>OBSGHBDAT(IGRID)%IPRT
      NQOBGB=>OBSGHBDAT(IGRID)%NQOBGB
      NQCLGB=>OBSGHBDAT(IGRID)%NQCLGB
      IOBTS=>OBSGHBDAT(IGRID)%IOBTS
      FLWSIM=>OBSGHBDAT(IGRID)%FLWSIM
      FLWOBS=>OBSGHBDAT(IGRID)%FLWOBS
      TOFF=>OBSGHBDAT(IGRID)%TOFF
      OTIME=>OBSGHBDAT(IGRID)%OTIME
      QCELL=>OBSGHBDAT(IGRID)%QCELL
      OBSNAM=>OBSGHBDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2GHB7PSV(IGRID)
C  Save OBSGHB data for a grid.
      USE OBSGHBMODULE
C
      OBSGHBDAT(IGRID)%NQGB=>NQGB
      OBSGHBDAT(IGRID)%NQCGB=>NQCGB
      OBSGHBDAT(IGRID)%NQTGB=>NQTGB
      OBSGHBDAT(IGRID)%IUGBOBSV=>IUGBOBSV
      OBSGHBDAT(IGRID)%IPRT=>IPRT
      OBSGHBDAT(IGRID)%NQOBGB=>NQOBGB
      OBSGHBDAT(IGRID)%NQCLGB=>NQCLGB
      OBSGHBDAT(IGRID)%IOBTS=>IOBTS
      OBSGHBDAT(IGRID)%FLWSIM=>FLWSIM
      OBSGHBDAT(IGRID)%FLWOBS=>FLWOBS
      OBSGHBDAT(IGRID)%TOFF=>TOFF
      OBSGHBDAT(IGRID)%OTIME=>OTIME
      OBSGHBDAT(IGRID)%QCELL=>QCELL
      OBSGHBDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
