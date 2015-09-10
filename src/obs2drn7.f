      MODULE OBSDRNMODULE
         INTEGER, SAVE, POINTER  ::NQDR,NQCDR,NQTDR,IUDROBSV,IPRT
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQOBDR
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::NQCLDR
         INTEGER, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER,CONTIGUOUS::OBSNAM
      TYPE OBSDRNTYPE
         INTEGER, POINTER  ::NQDR,NQCDR,NQTDR,IUDROBSV,IPRT
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQOBDR
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::NQCLDR
         INTEGER,     DIMENSION(:),   POINTER,CONTIGUOUS::IOBTS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWSIM
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::FLWOBS
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::TOFF
         REAL,        DIMENSION(:),   POINTER,CONTIGUOUS::OTIME
         REAL,        DIMENSION(:,:), POINTER,CONTIGUOUS::QCELL
         CHARACTER*12,DIMENSION(:),   POINTER,CONTIGUOUS ::OBSNAM
      END TYPE
      TYPE(OBSDRNTYPE),  SAVE   ::OBSDRNDAT(10)
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


      SUBROUTINE OBS2DRN7AR(IUDROB,IUDRN,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT DRAIN CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE GLOBAL,  ONLY:LSTCHK
      USE OBSDRNMODULE
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
C
      ALLOCATE(NQDR,NQCDR,NQTDR,IUDROBSV,IPRT)
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
        WRITE(IOUT,7) IUDROB
      ENDIF
    7 FORMAT(/,' OBS2DRN7 -- OBSERVATION PROCESS (DRAIN FLOW ',
     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFDRN is not active
      IF (IUDRN.EQ.0) THEN
        IF(LSTCHK(1)) THEN
          WRITE (IOUT,29 )
        ENDIF
   29   FORMAT (/,' DRAIN PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUDROB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDROBSV,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IUDROB)
      IPRT=1
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        IPRT=0
        WRITE(IOUT,*) 'NOPRINT option for DRAIN OBSERVATIONS'
      END IF
      IF(LSTCHK(3)) THEN
        WRITE (IOUT,9) NQDR, NQCDR, NQTDR
      ENDIF
    9 FORMAT (/,
     &     ' NUMBER OF FLOW-OBSERVATION DRAIN-CELL GROUPS.....: ',I6,/,
     &     '   NUMBER OF CELLS IN DRAIN-CELL GROUPS...........: ',I6,/,
     &     '   NUMBER OF DRAIN-CELL FLOWS.....................: ',I6)
      IF(NQTDR.LE.0) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL',
     1       ' TO 0'
         ENDIF
         CALL USTOP(' ')
      END IF
      IF(IUDROBSV.GT.0) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,21) IUDROBSV
         ENDIF
   21    FORMAT(1X,
     1      'DRAIN OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
      ELSE
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,22)
         ENDIF
   22    FORMAT(1X,'DRAIN OBSERVATIONS WILL NOT BE SAVED IN A FILE')
      END IF
C
Cx------Allocate memory
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
      READ(IUDROB,*) TOMULTDR
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE (IOUT,20) TOMULTDR
      ENDIF
   20 FORMAT (/,' OBSERVED DRAIN-CELL FLOW DATA',/,' -- TIME OFFSETS',
     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQDR
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUDROB,*) NQOBDR(IQ), NQCLDR(IQ)
        IF(LSTCHK(3).AND.IPRT.NE.0) THEN
          WRITE (IOUT,25) IQ, 'DRN', NQCLDR(IQ), NQOBDR(IQ)
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
          READ (IUDROB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          IF(LSTCHK(3).AND.IPRT.NE.0) THEN
            WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
          ENDIF
   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTDR,TSMULT,1,
     &                OTIME(J))
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLDR(IQ)
        IF(LSTCHK(3).AND.IPRT.NE.0) THEN
          WRITE (IOUT,54)
        ENDIF
   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUDROB,*) (QCELL(I,L),I=1,4)
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
     &          ' -- STOP EXECUTION (OBS2DRN7RP)',/)
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
     &' -- STOP EXECUTION (OBS2DRN7RP)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2DRN7PSV(IGRID)
      RETURN
      END
      SUBROUTINE OBS2DRN7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE DRAIN
C     PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,HNEW,IBOUND
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFDRNMODULE, ONLY:NDRAIN,DRAI
      USE OBSBASMODULE,ONLY:ITS
      USE OBSDRNMODULE
      DOUBLE PRECISION HHNEW, HB, C
C     ------------------------------------------------------------------
      CALL SGWF2DRN7PNT(IGRID)
      CALL SOBS2DRN7PNT(IGRID)
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
              DO 100 MNB = 1, NDRAIN
                NB = NB + 1
                IF (NB.GT.NDRAIN) NB = 1
                KK = DRAI(1,NB)
                II = DRAI(2,NB)
                JJ = DRAI(3,NB)
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
                  HB = DRAI(4,NB)
                  C = DRAI(5,NB)
                  HH = C*(HB-HHNEW)
                  IF(HHNEW.LE.HB) THEN
                    HH = ZERO
                    IF(LSTCHK(2)) THEN
                      IF (JRBOT.EQ.0) WRITE (IOUT,83 )
                    ENDIF
   83                 FORMAT (/,
     &                ' HEADS AT DRAIN CELLS ARE BELOW THE',
     &                ' BOTTOM OF THE DRAIN BED AT THE CELLS LISTED',/,
     &                ' BELOW.  THESE CONDITIONS DIMINISH THE IMPACT',
     &                ' OF THE OBSERVATION ON ESTIMATES OF',/,
     &       ' ALL PARAMETERS EXCEPT THOSE THAT CONTROL THE HYDRAULIC',
     &                ' CONDUCTIVITY OF THE',/,
     &                ' DRAIN BED.  (SEE TEXT FOR MORE INFORMATION).')
                    JRBOT = 1
                    IF (IRBOT.EQ.0) THEN
                      IF(LSTCHK(2)) THEN
                        WRITE (IOUT,92 ) NT, OBSNAM(NT), ITS
                      ENDIF
   92                 FORMAT (/,' OBS# ',I6,', ID ',A,', TIME STEP ',I5)
                      IF(LSTCHK(2)) THEN
                        WRITE (IOUT,93 )
                      ENDIF
   93                 FORMAT ('    LAYER   ROW  COLUMN')
                    ENDIF
                    IRBOT = IRBOT + 1
                    IF(LSTCHK(2)) THEN
                      WRITE (IOUT,97 ) K, I, J
                    ENDIF
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
              IF(LSTCHK(1)) THEN
                WRITE (IOUT,140) N, IQ, OBSNAM(NT),K,I,J
              ENDIF
  140 FORMAT  (' CELL ',I6,
     1         ' OF DRAIN OBSERVATION CELL GROUP',I5,/,
     2         ' NOT FOUND IN CELLS LISTED FOR DRAIN PACKAGE',/,
     3         ' OBSERVATION NAME:',A,/,
     4         ' CELL LAYER, ROW, AND COLUMN:',3I8,/,
     5         '  -- STOP EXECUTION (OBS2DRN7SE)')
                CALL USTOP(' ')
C
Cx------END OF LOOP FOR THE CELLS IN ONE CELL GROUP FOR ONE OBSERVATION TIME..
  400       CONTINUE
C
C-------PRINT NUMBER OF CELLS AT WHICH HEAD IS BELOW THE BOTTOM OF THE
C-------DRAIN BED; CHECK FOR ALL CELLS IN OBSERVATION BEING DRY.
            IF(LSTCHK(3)) THEN
              IF(IRBOT.GT.0) WRITE (IOUT,530) IRBOT, NQCLDR(IQ)
            ENDIF
  530           FORMAT (I7,' OF THE',I7,' CELLS USED TO SIMULATE THE',
     &            ' GAIN OR LOSS ARE',/,22X,'AFFECTED.')
            IF(KRBOT.EQ.NQCLDR(IQ)) THEN
              IF(LSTCHK(3)) THEN
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
  700   NC = NC + NQCLDR(IQ)
        NT1 = NT2 + 1
C
Cx------END OF LOOP FOR ALL CELL GROUPS.
  800 CONTINUE
C
Cx------RETURN
      RETURN
      END
      SUBROUTINE OBS2DRN7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE GLOBAL, ONLY:LSTCHK
      USE OBSDRNMODULE
      DOUBLE PRECISION SQ,SUMSQ
C     ------------------------------------------------------------------
      CALL SOBS2DRN7PNT(IGRID)
C
C1------WRITE OBSERVATIONS TO LISTING FILE.
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE(IOUT,17)
      ENDIF
   17 FORMAT(1X,/,1X,'DRAIN FLOW OBSERVATIONS',/,
     1  1X,'OBSERVATION     OBSERVED      SIMULATED',/
     2  1X,'  NAME            VALUE         VALUE      DIFFERENCE',/
     3  1X,'-------------------------------------------------------')
      SUMSQ=0.
      DO 100 N=1,NQTDR
      DIFF=FLWOBS(N)-FLWSIM(N)
      SQ=DIFF*DIFF
      SUMSQ=SUMSQ+SQ
      IF(LSTCHK(3).AND.IPRT.NE.0) THEN
        WRITE(IOUT,27) OBSNAM(N),FLWOBS(N),FLWSIM(N),DIFF
      ENDIF
   27 FORMAT(1X,A,1P,3G14.6)
  100 CONTINUE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,28) SUMSQ
      ENDIF
   28 FORMAT(1X,/,1X,'SUM OF SQUARED DIFFERENCE:',1P,E15.5)
C
C2------WRITE OBSERVATIONS TO SEPARATE FILE.
      IF(IUDROBSV.GT.0) CALL UOBSSV(IUDROBSV,NQTDR,FLWSIM,
     1                              FLWOBS,OBSNAM,0)
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE OBS2DRN7DA(IGRID)
C  Deallocate OBSDRN memory
      USE OBSDRNMODULE
C
      DEALLOCATE(OBSDRNDAT(IGRID)%NQDR)
      DEALLOCATE(OBSDRNDAT(IGRID)%NQCDR)
      DEALLOCATE(OBSDRNDAT(IGRID)%NQTDR)
      DEALLOCATE(OBSDRNDAT(IGRID)%IUDROBSV)
      DEALLOCATE(OBSDRNDAT(IGRID)%IPRT)
      DEALLOCATE(OBSDRNDAT(IGRID)%NQOBDR)
      DEALLOCATE(OBSDRNDAT(IGRID)%NQCLDR)
      DEALLOCATE(OBSDRNDAT(IGRID)%IOBTS)
      DEALLOCATE(OBSDRNDAT(IGRID)%FLWSIM)
      DEALLOCATE(OBSDRNDAT(IGRID)%FLWOBS)
      DEALLOCATE(OBSDRNDAT(IGRID)%TOFF)
      DEALLOCATE(OBSDRNDAT(IGRID)%OTIME)
      DEALLOCATE(OBSDRNDAT(IGRID)%QCELL)
      DEALLOCATE(OBSDRNDAT(IGRID)%OBSNAM)
C
C NULLIFY THE LOCAL POINTERS
      IF(IGRID.NE.1)THEN
        NQDR    =>NULL()
        NQCDR   =>NULL()
        NQTDR   =>NULL()
        IUDROBSV=>NULL()
        IUDROBSV=>NULL()
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
      END IF
C
      RETURN
      END
      SUBROUTINE SOBS2DRN7PNT(IGRID)
C  Change OBSDRN data to a different grid.
      USE OBSDRNMODULE
C
      NQDR=>OBSDRNDAT(IGRID)%NQDR
      NQCDR=>OBSDRNDAT(IGRID)%NQCDR
      NQTDR=>OBSDRNDAT(IGRID)%NQTDR
      IUDROBSV=>OBSDRNDAT(IGRID)%IUDROBSV
      IPRT=>OBSDRNDAT(IGRID)%IPRT
      NQOBDR=>OBSDRNDAT(IGRID)%NQOBDR
      NQCLDR=>OBSDRNDAT(IGRID)%NQCLDR
      IOBTS=>OBSDRNDAT(IGRID)%IOBTS
      FLWSIM=>OBSDRNDAT(IGRID)%FLWSIM
      FLWOBS=>OBSDRNDAT(IGRID)%FLWOBS
      TOFF=>OBSDRNDAT(IGRID)%TOFF
      OTIME=>OBSDRNDAT(IGRID)%OTIME
      QCELL=>OBSDRNDAT(IGRID)%QCELL
      OBSNAM=>OBSDRNDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2DRN7PSV(IGRID)
C  Save OBSDRN data for a grid.
      USE OBSDRNMODULE
C
      OBSDRNDAT(IGRID)%NQDR=>NQDR
      OBSDRNDAT(IGRID)%NQCDR=>NQCDR
      OBSDRNDAT(IGRID)%NQTDR=>NQTDR
      OBSDRNDAT(IGRID)%IUDROBSV=>IUDROBSV
      OBSDRNDAT(IGRID)%IPRT=>IPRT
      OBSDRNDAT(IGRID)%NQOBDR=>NQOBDR
      OBSDRNDAT(IGRID)%NQCLDR=>NQCLDR
      OBSDRNDAT(IGRID)%IOBTS=>IOBTS
      OBSDRNDAT(IGRID)%FLWSIM=>FLWSIM
      OBSDRNDAT(IGRID)%FLWOBS=>FLWOBS
      OBSDRNDAT(IGRID)%TOFF=>TOFF
      OBSDRNDAT(IGRID)%OTIME=>OTIME
      OBSDRNDAT(IGRID)%QCELL=>QCELL
      OBSDRNDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
