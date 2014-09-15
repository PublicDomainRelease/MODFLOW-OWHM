C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- One-Water Hydrologic Model (OWHM)
C     WITH THE FARM PROCESS (version 3), LGR2, NWT1, SWR1, RIP1, and SWI2
C     ******************************************************************
C
      PROGRAM MF_OWHM
C     ------------------------------------------------------------------
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFLAKMODULE, ONLY:NLAKESAR,THETA,STGOLD,STGNEW,VOL
      USE GWFSFRMODULE, ONLY:NUMTAB
      USE GWFNWTMODULE, ONLY:LINMETH,ICNVGFLG,ITREAL
!      USE GWFSUBMODULE, ONLY:SUBLNK                                     !ADDED FOR SUB-LINK BY WSCHMID 07/29/10 !SUBLNK NOW PART OF GLOBAL
      USE FMPMODULE,    ONLY:IRTFL,ICUFL,IPFL,IEBFL,QBD,MCLOSE,ISTARTFL,!inserted by SCHMID
     +                       FMP3DA,SFMP3PNT                            ! seb
      USE FMPMAIN,      ONLY:FMP3AR,FMP3RP,FMP3RQ,FMP3AD,FMP3ADSUB,
     +                       FMP3FM,FMP3FNRBD,FMP3WELBD,
     +                       FMP3QCNVG,FMP3ETPRT
      USE PCGMODULE
      USE SIPMODULE
      USE DE4MODULE
      USE GMGMODULE
      USE LGRMODULE
      USE PCGN
      USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD
      USE CVT2STR, ONLY: NUM2STR
      INCLUDE 'openspec.inc'
          
C
C-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION,VERSION2,VERSION3
      CHARACTER*35 MFVNAM
      PARAMETER (VERSION='1.9.01 5/01/2012')
      PARAMETER (VERSION2='3.0.0 01/09/2013')
      PARAMETER (VERSION3='1.01.0 09/10/2012')
      PARAMETER (MFVNAM='FMP3-LGR2-NWT1-SWR1-SWI2')
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*700 FNAME
      INTEGER IBDT(8)
ccrth
      INTEGER(4):: START, FINISH, ClockRate
      REAL:: CPU_TIME
ccrth
C
      !CHARACTER*4 CUNIT(NIUNIT) !seb MOVED TO GLOBAL MODULE
      CUNIT = [  'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'RIP ', 'GHB ',  !  7 !RIP ADDED BY SCHMID
     +           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     +           'GWT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     +           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     +           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     +           '    ', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'GMG ',  ! 42
     +           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', 'BFH2', 'LMT6',  ! 49
     +           'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'GWM ',  ! 56
     +           'SWT ', 'cfp ', 'coc ', 'crch', 'FMP ', 'UPW ', 'NWT ',  ! 63
     +           'SWR ', 'SWI2', '    ', '    ', '    ', '    ', 'PCGN',  ! 70     - SWR - JDH 
     +           ('    ',I=1,30)                                       ]  ! 71-100 - SWR - JDH -SEB CHANGED FROM 30*'   '
C     ------------------------------------------------------------------
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      CALL PRINT_MAIN_HEADER(6)  !PRINT TO COMMAND PROMPT seb
!      WRITE (*,1) MFVNAM,VERSION,VERSION2,VERSION3
!    1 FORMAT (/,36X,'OWHM 1.0',/,
!     &4X,'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE ',/ 24x
!     &'ONE-WATER HYDROLOGIC-FLOW MODEL [OWHM]',/,25X,'WITH ',A
!     &,  /,29X,'Version ',A/,20X,'BASED ON MODFLOW-2005 Version ',A,
!     &  /,20X,'SWR1 Version ',A/)
      INUNIT = 99
      ILUNIT = 98
      NGRIDS = 1 
      ILGR = 0
      NCVGERR=0
C
C3------GET THE NAME OF THE NAME FILE
      CALL GETNAMFIL(ILGR,NGRIDS,FNAME,ILUNIT)
      DO IGRID = 1, NGRIDS
        MAXUNIT= INUNIT
C
C4A------IF USING LGR, READ NAMES FROM LGR NAME FILE
        IF(ILGR .NE. 0) CALL GETNAMFILLGR(ILUNIT,FNAME,IGRID)
C4B-----OPEN NAME FILE.
        OPEN (UNIT=INUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
        NC=INDEX(FNAME,' ')
        WRITE(*,490)' Using NAME file: ',FNAME(1:NC)
  490   FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
        IF(IGRID .EQ. 1)THEN
          CALL DATE_AND_TIME(VALUES=IBDT)
          WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2     FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &    I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
        ENDIF
C
C6------ALLOCATE AND READ (AR) PROCEDURE
ccrth      IGRID=1
      NSOL=1
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,IGRID,12,
     1                HEADNG,26,MFVNAM)
      IF(IUNIT(50).GT.0 .AND. IUNIT(52).GT.0) THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same grid'
        ENDIF
        CALL USTOP(' ')
      END IF
      IF(ILGR .NE.0) CALL GWF2LGR2AR(ILUNIT,FNAME,NGRIDS,IGRID)
      IF(IUNIT(50).GT.0 .AND. IUNIT(52).GT.0) THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same grid'
        ENDIF
        CALL USTOP(' ')
      END IF
C ScottBoyce Moved Call of GWF2HFB7AR tobefore NWT so that fault locations can be known beforehand
      IF(IUNIT(21).GT.0) CALL GWF2HFB7AR(IUNIT(21),IGRID)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7AR(IUNIT(1),ILGR,IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47),
     1                                     IUNIT(53),ILGR,IGRID)
! Allocate arrays for Newton Solver
      IF(IUNIT(63).GT.0) CALL GWF2NWT1AR(IUNIT(63),MXITER, 
     1                                   IUNIT(22),IGRID)
      IF(IUNIT(62).GT.0) CALL GWF2UPW1AR(IUNIT(62), Igrid)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7AR(IUNIT(2),IUNIT(63),IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7AR(IUNIT(3),IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7AR(IUNIT(4),IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      If(IUNIT(6).GT.0) CALL GWF2RIP4AR(IUNIT(6),IGRID)               !inserted by schmid
      IF(IUNIT(7).GT.0) CALL GWF2GHB7AR(IUNIT(7),IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7AR(IUNIT(16),IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7AR(IUNIT(17),IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7AR(IUNIT(18),IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7AR(IUNIT(20),IGRID)
C ScottBoyce Moved GWF2HFB7AR to before GWF2BCF7AR
      IF(IUNIT(44).GT.0) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS,
     2                           IUNIT(62),IUNIT(55),IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
     1                                   IUNIT(23),IUNIT(37),
     2                                   IUNIT(63),IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0) CALL GWF2LAK7AR(
     1             IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44),
     1                                     IUNIT(22),IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7AR(IUNIT(39),IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7AR(IUNIT(40),IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      IF(IUNIT(48).GT.0) CALL GWF2BFH2AR(IUNIT(48),ILGR,IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10).GT.0) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(70).GT.0) CALL PCGN2AR(IUNIT(70),IFREFM,MXITER,IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27AR(IUNIT(50),IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),
     1                     IUNIT(10),IUNIT(63),0,IUNIT(13),
     2                     0,IUNIT(42),IUNIT(70),FNAME,IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7AR(IUNIT(57),IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7AR(IUNIT(64),
     2                        IUNIT(1),IUNIT(23),IUNIT(37),
     3                        IUNIT(62),IUNIT(44),IUNIT(63),IGRID)  !SWR  - JDH
      IF(IUNIT(65).GT.0) CALL GWF2SWI2AR(IUNIT(65),
     2                        IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),
     3                        IGRID)   !SWI  - JDH
ccrth      IF(IUNIT(66).GT.0) CALL GWF2XXX7AR(IUNIT(66),IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
     1                   CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
     1                   CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                   CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                   CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
      IF(IUNIT(49).GT.0) CALL LMT7BAS7AR(INUNIT,CUNIT,IGRID)
      IF(IUNIT(61).GT.0) THEN
        CALL FMP3AR(
     1  IUNIT(61),IUNIT(44),IUNIT(52),IUNIT(50),IUNIT(55),
     2                 IUNIT(63),IUNIT(40),IGRID,ILGR)                  !FMP3AR CALL ADDED BY SCHMID, added NWT iunit rth
        CALL FMP3RQ(IUNIT(61),IUNIT(44),IUNIT(52),IUNIT(50),
     1            IUNIT(63),IUNIT(40),IGRID,ILGR)                       !FMP3RQ CALL ADDED BY SCHMID, added NWT,MNWI, & MNW2 iunits rth
      ENDIF
C
C  Observation allocate and read
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7AR(IUNIT(38),IGRID)
C
C END LOOP FOR ALLOCATING AND READING DATA FOR EACH GRID
C      IF(ILGR.NE.0.AND.IGRID.GT.1.AND.IUNIT(62).GT.0) CALL #2PRTOCH()
      ENDDO
      CLOSE(ILUNIT)
C
C7------SIMULATE EACH STRESS PERIOD.
ccrth
      START=0 
      FINISH=0
      ClockRate=0
      CALL SYSTEM_CLOCK(COUNT_RATE=ClockRate) ! Find the rate 
      DO 100 KPER = 1, NPER
ccrth
      CALL SYSTEM_CLOCK(COUNT=START)                  ! Start timing 
ccrth
        KKPER = KPER
        DO IGRID = 1, NGRIDS
        CALL SGWF2BAS7PNT(IGRID)                       !seb lgr
        IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,Igrid)
        CALL GWF2BAS7ST(KKPER,IGRID)
        IF(IUNIT(19).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
        IF(IUNIT(54).GT.0) CALL GWF2SUB7ST(KKPER,IGRID)
        IF(IUNIT(57).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
cswm: note the '1' below is hardwired for the parent grid
        IF(ILGR .NE. 0) CALL GWF2LGR2RP(KKPER,1,IGRID)
        IF(IUNIT(2).GT.0) CALL GWF2WEL7RP(IUNIT(2),IGRID)
        IF(IUNIT(3).GT.0) CALL GWF2DRN7RP(IUNIT(3),IGRID,IUNIT(63))
        IF(IUNIT(4).GT.0) CALL GWF2RIV7RP(IUNIT(4),IGRID)
        IF(IUNIT(5).GT.0) CALL GWF2EVT7RP(IUNIT(5),IGRID)
        If(IUNIT(6).GT.0) CALL GWF2RIP4RP(IUNIT(6),IGRID)             !inserted by schmid
        IF(IUNIT(7).GT.0) CALL GWF2GHB7RP(IUNIT(7),IGRID,IUNIT(63))   !inserted Iunit(63) for NWT by rth
!        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IUNIT(44),IGRID)
        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IGRID)
        IF(IUNIT(17).GT.0) CALL GWF2RES7RP(IUNIT(17),IGRID)
        IF(IUNIT(18).GT.0) CALL GWF2STR7RP(IUNIT(18),IGRID)
        IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                     CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(20).GT.0) CALL GWF2CHD7RP(IUNIT(20),IGRID)
        IF(IUNIT(21).GT.0) CALL GWF2HFB7RP(IUNIT(21),KKPER,IGRID)       !seb
        IF(IUNIT(44).GT.0) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),
     1                                IUNIT(22),IUNIT(54),KKPER,KKSTP,
     2                                     NSOL,IOUTS,IUNIT(1),
     3                                     IUNIT(23),IUNIT(37),
     4                                     IUNIT(62), IUNIT(55), IGRID)
      IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                     CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,IUNIT(44),
     1                                     IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               IUNIT(62),KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL,IGRID)
        IF(IUNIT(39).GT.0) CALL GWF2ETS7RP(IUNIT(39),IGRID)
        IF(IUNIT(40).GT.0) CALL GWF2DRT7RP(IUNIT(40),IGRID)
        IF(IUNIT(50).GT.0) CALL GWF2MNW27RP(IUNIT(50),KKPER,IUNIT(9),
     +                       IUNIT(10),0,IUNIT(13),0,IUNIT(42),
     +                       IUNIT(70),IUNIT(15),IUNIT(63),IGRID)
        IF(IUNIT(51).GT.0.AND.KKPER.EQ.1) CALL GWF2MNW2I7RP(IUNIT(51),
     1                     0,IGRID)
        IF(IUNIT(52).GT.0) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),
     1                            IUNIT(23),IUNIT(37),IUNIT(62),KKPER,
     2                            IGRID)     
        IF(IUNIT(48).GT.0) CALL GWF2BFH2RP(IUNIT(48),KKPER,IGRID)
        IF(IUNIT(61).GT.0) CALL FMP3RP(IUNIT(61),KKPER,                 !FMP3AR CALL ADDED BY SCHMID
     1                          IUNIT(44),IUNIT(52),IUNIT(50),
     2                          IUNIT(63),IGRID,NGRIDS,ILGR) 
        IF(IUNIT(64).GT.0) CALL GWF2SWR7RP(IUNIT(64),KKPER,IGRID)  !SWR - JDH
        IF(IUNIT(63).GT.0 ) CALL GWF2NWT1RP(IUNIT(63),KPER,Mxiter,IGRID)
        IF(IUNIT(13).GT.0 ) CALL PCG7RP(IUNIT(13),KPER,Mxiter,IGRID)
ccrth        IF(IUNIT(66).GT.0) CALL GWF2XXX7RP(IUNIT(66),IGRID)
        ENDDO 
C
C7C-----SIMULATE EACH TIME STEP.
        DO 90 KSTP = 1, NSTP(KPER)
          KKSTP = KSTP
          DO IGRID = 1, NGRIDS
ccrth          IF ( IRESTART.EQ.1 ) THEN
ccrth            IF ( KPER.EQ.KPERSTART .AND. KSTP.EQ.KSTPSTART ) THEN
ccrth              CALL RESTARTHEADS(IOUT)
ccrth            END IF
ccrth          END IF
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
        CALL SGWF2BAS7PNT(IGRID)                       !seb lgr
        IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,Igrid)
        CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
        IF(IUNIT(54).GT.0)CALL GWF2SUB7AD(KKPER,KKSTP,IUNIT(54),IGRID)
ccrth--Update Drain Elevations if SUB-LINK active
        IF(IUNIT(54).GT.0)THEN
         IF(IUNIT(3).GT.0) CALL GWF2DRN7AD(KKSTP,IGRID)
         IF(IUNIT(40).GT.0) CALL GWF2DRT7AD(KKSTP,IGRID)
        ENDIF
ccrth
          IF(IUNIT(62).GT.0) CALL GWF2UPW1AD(IGRID)
          IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
          IF(IUNIT(1).GT.0) CALL GWF2BCF7AD(KKPER,IGRID)
          IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
          IF(IUNIT(23).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
          IF(IUNIT(37).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
          IF(IUNIT(16).GT.0) CALL GWF2FHB7AD(IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(1),
     1               IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               IUNIT(62),IUNIT(15),IGRID)
          IF(IUNIT(65).GT.0) CALL GWF2SWI2AD(KKSTP,KKPER,IGRID)  !SWI
          IF(IUNIT(44).GT.0.and.IUNIT(54).GT.0)                         !ADDED FOR SUB-LINK BY WSCHMID 07/29/10 --> sfr only uses AD for Sublink
     1                                      CALL GWF2SFR7AD(KKSTP,IGRID)!
          IF(IUNIT(7).GT.0) CALL GWF2GHB7AD(IGRID)                     !seb added GHB and WEL AD
          IF(IUNIT(2).GT.0) CALL GWF2WEL7AD(KKSTP,IGRID)
          IF( IUNIT(44).GT.0 )CALL GWF2SFR7LAKE(IUNIT(22),IGRID) 
          IF(IUNIT(50).GT.0) THEN
            IF (IUNIT(1).GT.0) THEN
              CALL GWF2MNW27BCF(KPER,IGRID)
            ELSE IF (IUNIT(23).GT.0) THEN
              CALL GWF2MNW27LPF(KPER,IGRID)
            ELSE IF(IUNIT(37).GT.0) THEN
              CALL GWF2MNW27HUF(KPER,IGRID)
            ELSE IF(IUNIT(62).GT.0) THEN
              CALL GWF2MNW27UPW(KPER,IGRID)
            ELSE
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,1000)
              ENDIF
 1000         FORMAT(/1X,
     &      '***ERROR: MNW2 PACKAGE DOES NOT SUPPORT',/,
     &      ' SELECTED FLOW PACKAGE',/,
     &  ' (MNW2 DOES FULLY SUPPORT BCF, LPF, HUF, AND UPW PACKAGES)',/,
     &      ' -- STOP EXECUTION')
              CALL USTOP('MNW2 error-flow package')
            END IF
            CALL GWF2MNW27AD(KKSTP,KKPER,IUNIT(62),IGRID)
          END IF
          IF(IUNIT(52).GT.0) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),
     1                                  IUNIT(37),IUNIT(62),IGRID)
          IF(IUNIT(48).GT.0) CALL GWF2BFH2AD(IUNIT(48),IGRID)
          IF(IUNIT(61).GT.0) THEN                                       !FMP3AD CALL ADDED BY SCHMID
             IF(IRTFL.EQ.3.OR.ICUFL.EQ.3.OR.IPFL.EQ.3
     1                             .OR.IEBFL.EQ.1.OR.IEBFL.EQ.3)
     2                                          CALL FMP3AD(KKPER,IGRID)
             IF(IUNIT(54).GT.0) CALL FMP3ADSUB(KKPER,IGRID)             !ADDED FOR SUB-LINK BY WSCHMID 07/29/10
          ENDIF
          IF(IUNIT(6).GT.0.AND.IUNIT(54).GT.0) 
     +                                       CALL GWF2RIPAD(KKPER,IGRID)!RIP-SUBLINK Update - rth
          IF(IUNIT(64).GT.0) CALL GWF2SWR7AD(KKPER,KSTP,IGRID,IUNIT(54))  !SWR - JDH
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
ccrth          IF ( IRESTART.GT.0 ) THEN
ccrth            IF ( KPER.LT.KPERSTART ) THEN
ccrth              CALL UMESPR('SKIPPING STEP TO RESTART',' ',IOUT)
ccrth              WRITE(*,26)KPER,KSTP
ccrth            ELSE IF ( KPER.EQ.KPERSTART .AND. KSTP.LT.KSTPSTART ) THEN
ccrth              CALL UMESPR('SKIPPING STEP TO RESTART',' ',IOUT)
ccrth              WRITE(*,26)KPER,KSTP
ccrth            ELSE
ccrth              CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
ccrth              WRITE(*,25)KPER,KSTP
ccrth            END IF
ccrth          ELSE
            CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
ccrth          END IF
          ENDDO  !end of multi-grid loop
          WRITE(*,25)KPER,KSTP                            !seb moved outside of IGRID LOOP
   25     FORMAT(' Solving:  Stress period: ',i6,4x,
     &       'Time step: ',i6,4x,'Groundwater-Flow Eqn.')
   26     FORMAT('Skipping:  Stress period: ',i6,4x,
     &       'Time step: ',i6)
C          
C---------BEGIN LOOP FOR ITERATING BETWEEN GRIDS (LGR ITERATIONS)
          LGRCNVG = 0            
          LGRITER = 0            
          DO WHILE (LGRCNVG .EQ. 0)
            LGRITER = LGRITER + 1

C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS FOR EACH GRID
            DO IGRID = 1, NGRIDS
              CALL SGWF2BAS7PNT(IGRID)
C-------------CHECK IF LGR IS ACTIVE
              IF(ILGR .NE. 0)THEN
                CALL SGWF2LGR2PNT(IGRID)
C---------------CHECK IF PARENT OR CHILD GRID 
                IF(ISCHILD .NE. -1)THEN
C-----------------INTERPOLATE PARENT HEAD TO GHOST NODES AND RELAX
!swm: NOTE: the '1' in the arguement list is hardwired for the parent
!grid
                  CALL GWF2LGR2DARCY(KKPER,KKSTP,LGRITER,1,IGRID)
                ENDIF          
              ENDIF
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
           KITER = 0
           ITREAL2 = 0
ccrth           NOITER = 1
           IF ( IUNIT(63).GT.0 ) ITREAL = 0
ccrth           IF ( IRESTART.GT.0 ) THEN
ccrth             NOITER = 0
ccrth             IF ( KPER.GT.KPERSTART ) THEN
ccrth               NOITER = 1
ccrth             ELSE IF ( KPER.EQ.KPERSTART .AND. KSTP.GE.KSTPSTART ) THEN
ccrth               NOITER = 1 
ccrth             END IF
ccrth           END IF
ccrth           DO WHILE (ITREAL2.LT.MXITER .AND. NOITER.EQ.1)
           DO WHILE (ITREAL2.LT.MXITER)
            KITER = KITER + 1
            KKITER = KITER
            IF ( IUNIT(63).EQ.0 ) ITREAL2 = KITER
            IF(IUNIT(62).GT.0) CALL GWF2UPWUPDATE(2,Igrid)
C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7FM(IGRID)
            IF(IUNIT(1).GT.0) CALL GWF2BCF7FM(KKITER,KKSTP,
     1                               KKPER,ILGR,IGRID)                       !SCOTT ORIGINALLY KKPER,ILGR,IGRID) BUT SUBROUTINE IS NOT DESIGNED TO RECIEVE ILGR, THIS MAY NEED TO BE INCORPORATED !swm: I added back in and modified BCF
            IF(IUNIT(62).GT.0) CALL GWF2UPWFMS(KKITER,KKSTP,KKPER,IGRID)
            IF(IUNIT(23).GT.0) CALL GWF2LPF7FM(KKITER,KKSTP,KKPER,ILGR,
     1                            IGRID)
            IF(IUNIT(37).GT.0) CALL GWF2HUF7FM(KKITER,
     1                             KKSTP,KKPER,IUNIT(47),ILGR,IGRID)
            IF ( IUNIT(62).EQ.0 ) THEN
              IF(IUNIT(21).GT.0) CALL GWF2HFB7FM(IGRID)
            END IF
            IF(IUNIT(2).GT.0) CALL GWF2WEL7FM(IUNIT(63),IGRID)
            IF(IUNIT(3).GT.0) CALL GWF2DRN7FM(IGRID)
            IF(IUNIT(4).GT.0) CALL GWF2RIV7FM(IGRID)
            IF(IUNIT(5).GT.0) THEN
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
              CALL GWF2EVT7FM(IGRID)
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(6).GT.0) CALL GWF2RIP4FM(IGRID)                !inserted by schmid
            IF(IUNIT(7).GT.0) CALL GWF2GHB7FM(IGRID)
            IF(IUNIT(8).GT.0) THEN
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
               CALL GWF2RCH7FM(IGRID)
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(16).GT.0) CALL GWF2FHB7FM(IGRID)
            IF(IUNIT(17).GT.0) CALL GWF2RES7FM(IGRID)
            IF(IUNIT(18).GT.0) CALL GWF2STR7FM(IGRID)
            IF(IUNIT(19).GT.0) CALL GWF2IBS7FM(KKPER,IGRID)
            IF(IUNIT(39).GT.0) CALL GWF2ETS7FM(IGRID)
            IF(IUNIT(40).GT.0) CALL GWF2DRT7FM(IGRID)                   !DRT MUST OCCUR BEFORE FMP seb
ccrth            SEC1=MCLOCK(yy)
            IF(IUNIT(61).GT.0) CALL FMP3FM(KKITER,KKPER,KKSTP,            !FMP3FM CALL ADDED BY SCHMID
     1                    IUNIT(44),IUNIT(52),IUNIT(50),
     2                    IUNIT(63),IUNIT(55),IUNIT(40),
     3                    IGRID,NGRIDS,ILGR,LGRITER)
ccrth            SEC2=MCLOCK(yy)
ccrth            WRITE(*,*)'FMPFM elaps-time SEC1 SEC2 SEC2-SEC1 ITR TS SP', 
ccrth     1       sec1,sec2, sec2-sec1, KKITER, KKPER,KKSTP     
            IF(IUNIT(55).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,
     1                           IUNIT(44),IUNIT(22),IUNIT(58),
     2                           IUNIT(63),
     3                           IUNIT(64),IGRID)  !SWR - JDH ADDED IUNIT(64)
            IF(IUNIT(44).GT.0) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,
     1                        IUNIT(22),IUNIT(63),IUNIT(8),IUNIT(55),
     3                        ILGR,LGRITER,NGRIDS,IGRID)   !cjm (added IUNIT(8))
            IF(IUNIT(22).GT.0) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,
     1                                     IUNIT(44),IUNIT(55),IGRID)
            IF(IUNIT(50).GT.0) THEN
              IF (IUNIT(1).GT.0) THEN
                CALL GWF2MNW27BCF(KPER,IGRID)
              ELSE IF (IUNIT(23).GT.0) THEN
                CALL GWF2MNW27LPF(KPER,IGRID)
              ELSE IF(IUNIT(37).GT.0) THEN
                CALL GWF2MNW27HUF(KPER,IGRID)
              ELSE IF(IUNIT(62).GT.0) THEN
                CALL GWF2MNW27UPW(KPER,IGRID)
              END IF
              CALL GWF2MNW27FM(KKITER,IUNIT(62),kkstp,kkper,IGRID)
            END IF
            IF(IUNIT(52).GT.0) CALL GWF2MNW17FM(KKITER,IUNIT(1),
     1                               IUNIT(23),IUNIT(37),IUNIT(62),
     2                               IGRID)
            IF(IUNIT(54).GT.0) CALL GWF2SUB7FM(KKPER,KKITER,
     1                                         IUNIT(9),IGRID)
            IF(IUNIT(57).GT.0) CALL GWF2SWT7FM(KKPER,IGRID)
            IF(IUNIT(64).GT.0) CALL GWF2SWR7FM(KKITER,KKPER,KKSTP,IGRID)  !SWR - JDH
            IF(IUNIT(65).GT.0) CALL GWF2SWI2FM(KKSTP,KKPER,KKITER,IGRID)  !SWI - JDH
ccrth            IF(IUNIT(66).GT.0) CALL GWF2XXX7FM(IGRID)
            IF(IUNIT(48).GT.0) CALL GWF2BFH2FM(KKPER,KKSTP,KKITER,
     1                                            IGRID) 
C-----------------ADJUST HCOF AND RHS IF LGR IS ACTIVE
                IF(ILGR .NE. 0)THEN
                  IF(IGRID .EQ. 1)THEN  
                    DO LG =2,NGRIDS
                      IF(LGRDAT(IGRID)%IBPFLG(LG) .NE. 0)
     1                  CALL GWF2LGR2PFM(KKPER,KKSTP,KKITER,LGRITER,
     2                     IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),LG) 
                    ENDDO
                  ELSEIF(ISCHILD .GE. 0)THEN    
                    CALL GWF2LGR2CFM(KKITER,LGRITER,IUNIT(1),
     1                     IUNIT(23),IUNIT(37),IUNIT(62),IGRID)
                  ENDIF
                ENDIF
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
                IERR=0
            IF (IUNIT(9).GT.0) THEN
                   CALL SIP7PNT(IGRID)
                   CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,
     1               V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     2               KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),
     3               NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
            END IF
            IF (IUNIT(10).GT.0) THEN
                   CALL DE47PNT(IGRID)
                   CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP,
     1               MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,
     2               ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,
     3               NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,
     4               DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,
     5               DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
            END IF
            IF (IUNIT(13).GT.0) THEN
                   CALL PCG7PNT(IGRID)
                   CALL PCG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,VPCG,SS,
     1               P,CD,HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,
     2               HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,
     3               MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,
     4               NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF,
     5               HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,
     6               IHCOFADD)
            END IF
            IF (IUNIT(42).GT.0) THEN
                   CALL GMG7PNT(IGRID)
                   CALL GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     1                         IITER,MXITER,RCLOSEGMG,HCLOSEGMG,
     2                         KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,ICNVG,
     3                         SITER,TSITER,DAMPGMG,IADAMPGMG,IOUTGMG,
     4                         IOUT,GMGID,
     5                         IUNITMHC,DUP,DLOW,CHGLIMIT,
     6                         BIGHEADCHG,HNEWLAST)
            ENDIF
            IF (IUNIT(70).GT.0) THEN
              CALL PCGN2AP(HNEW,RHS,CR,CC,CV,HCOF,IBOUND,
     1              KKITER,KKSTP,KKPER,ICNVG,HNOFLO,IGRID)
            ENDIF      
! Calculate new heads using Newton solver
          IF(IUNIT(63).GT.0 ) 
     1          CALL GWF2NWT1FM(KKITER,ICNVG,KSTP,KPER,Mxiter,
     2                          IUNIT(22),IGRID)
          IF ( IUNIT(63).GT.0 )ITREAL2 = ITREAL
                IF(IERR.EQ.1) CALL USTOP(' ')
C
C-WSCHMID-INSERT (06/18/2009)
C-------ALLOW CONVERGENCE CRITERIA FOR MNW WELL PUMPAGE LINKED TO FARM PROCESS
C       (will be active in next version once net MNW rates are computed at the end of FMP-routine)
C             IF (IUNIT(62).GT.0.AND.
C     1        IUNIT(52).GT.0.AND.ICNVG.EQ.1) THEN !.AND.MCLOSE.EQ.1)THEN
C              CALL SFMP3PNT(IGRID) 
C              CALL FMP3QCNVG(IUNIT(52),IUNIT(13),IUNIT(9),              !FMP3QCNVG CALL ADDED BY SCHMID / WSCHMID-03/23/11 - MNW1 changed unit#
C     1                       IUNIT(10),IUNIT(42),IUNIT(63),IUNIT(70))
C             ENDIF
C-WSCHMID-END OF INSERT
C
C-WSCHMID-MODIFIED (07/21/08)
C-------ENSURE CONVERGENCE OF SWR - BASEFLOW CHANGES LESS THAN TOLF - JDH
            IF(IUNIT(64).GT.0) THEN
              CALL GWF2SWR7CV(KKITER,IGRID,ICNVG,MXITER)
            END IF
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.           
              IF(ICNVG.EQ.1) THEN
                IF(IUNIT(61).EQ.0) THEN
                  GOTO 33
                ENDIF
                IF(IUNIT(61).GT.0) THEN                                 
                  IF(ISTARTFL.GT.0) THEN                                !ALLOW ONE INITIAL ITERATION (AT ISTARTFL=0) PRIOR TO FMP-FM
                    GOTO 33                                             !AND ONLY SOLVE AFTER THAT ITERATION ONCE CONVERGENCE IS MET.
                  ENDIF
                ENDIF
              ENDIF
C-WSCHMID-END OF MODIFICATION
          ENDDO
          KITER = MXITER
C
   33     CONTINUE
ccrth          IF ( NOITER.EQ.1 ) THEN
          IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(2,Igrid)
C
C
C-------------PREPARE THE NEXT GRID FOR LGR ITERATION
              IF(ILGR.NE.0)THEN 
                IF(ISCHILD .EQ. -1)THEN
                  DO LG =2,NGRIDS
                    CALL GWF2LGR2INITP(KKPER,KKSTP,LGRITER,
     1                      LGRDAT(LG)%NPCBEG,LGRDAT(LG)%NPRBEG,
     2                      LGRDAT(LG)%NPLBEG,LGRDAT(LG)%NPCEND,
     3                      LGRDAT(LG)%NPREND,LGRDAT(LG)%NPLEND,
     4                      LGRDAT(LG)%ISHFLG,LGRDAT(LG)%MXLGRITER,
     5                      IUNIT(5),IUNIT(8),IUNIT(17),LG,IGRID) 
                  ENDDO
                ELSEIF(IGRID.NE.1)THEN
C-----------------CALCULATE FLUX ENTERING THE CHILD INTERFACE 
                  CALL GWF2LGR2FMBF(KKPER,KKSTP,LGRITER)
                ENDIF
              ENDIF
C-----------END GRID LOOP
            ENDDO 
C-----------CHECK CONVEGENCE OF LGR IF LGR IS ACTIVE
            IF(ILGR .EQ. 0)THEN
              LGRCNVG = 1
            ELSE
              CALL GWF2LGR2CNVG(IGRID,NGRIDS,LGRCNVG,LGRITER,KKPER,
     1                          KKSTP)
            ENDIF
C---------END LGR ITERATION LOOP
          ENDDO       
C
C7C3----DETERMINE WHICH OUTPUT IS NEEDED FOR EACH GRID
          DO IGRID = 1, NGRIDS
            CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,
     1                      GLOBALDAT(IGRID)%IUNIT(12),IGRID)

C---------SWAP POINTERS FOR LGR DATA   !swm: needed for SFR
            IF(ILGR.NE.0) CALL SGWF2LGR2PNT(IGRID)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
          MSUM = 1
          IF (IUNIT(1).GT.0) THEN
            CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2BCF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 37 IDIR = 1, 3
              CALL GWF2BCF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     1                          IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
   37       CONTINUE
          ENDIF
          IF(IUNIT(23).GT.0) THEN
            CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 157 IDIR=1,3
              CALL GWF2LPF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
157         CONTINUE
          ENDIF
! ADDED CALLS FOR FLOW BUDGETS TO UPW PACKAGE.
          IF(IUNIT(62).GT.0) THEN
            CALL GWF2UPWBDS(KKSTP,KKPER,IGRID)
            CALL GWF2UPWBDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 158 IDIR=1,3
              CALL GWF2UPWBDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
158         CONTINUE
          ENDIF
          IF(IUNIT(37).GT.0) THEN
            CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(47),IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 159 IDIR=1,3
              CALL GWF2HUF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
159         CONTINUE
          ENDIF
          IF(IUNIT(2).GT.0) CALL GWF2WEL7BD(KKSTP,KKPER,IUNIT(63),IGRID)
          IF(IUNIT(3).GT.0) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(4).GT.0) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(5).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
             CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(6).GT.0) CALL GWF2RIP4BD(KKSTP,KKPER,IGRID)        !inserted by schmid
          IF(IUNIT(7).GT.0) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(8).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
!           CALL GWF2RCH7BD(KKSTP,KKPER,IUNIT(44),IGRID)  
          CALL GWF2RCH7BD(KKSTP,KKPER,IGRID) 
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(16).GT.0) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(17).GT.0) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(18).GT.0) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(19).GT.0) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(39).GT.0) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(40).GT.0) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
! (CJM) Added RCH unit number for RCH->SFR.
          IF(IUNIT(44).GT.0) CALL GWF2SFR7BD(KKSTP,KKPER,IUNIT(15),
     1                    IUNIT(22),IUNIT(46),IUNIT(55),NSOL,IUNIT(8),
     2                    ILGR,NGRIDS,IGRID)
! Moved call to UZF1BD to follow SFR7BD for printing net recharge in UZF.
          IF(IUNIT(55).GT.0) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),
     1                             IUNIT(44),IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),
     1                       IUNIT(46),IUNIT(44),IUNIT(55),NSOL,IGRID)
          IF(IUNIT(50).GT.0) CALL GWF2MNW27BD(KKSTP,KKPER,IUNIT(62),
     1                                        IGRID)
          IF(IUNIT(52).GT.0) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,KKPER,
     1                      IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(57).GT.0) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(64).GT.0) CALL GWF2SWR7BD(KKSTP,KKPER,IGRID)  !SWR - JDH
          IF(IUNIT(65).GT.0) CALL GWF2SWI2BD(KKSTP,KKPER,IGRID)  !SWI - JDH
ccrth          IF(IUNIT(66).GT.0) CALL GWF2XXX7BD(KKSTP,KKPER,IGRID) 
C--FARM DEMAND AND SUPPLY, FARM WELLS, AND FARM NET-RECHARGE
c	 ...FMP3fm is inserted to allow recalculating FMP-flowrates, which 
c         may de a function of SFR & MNW flowrates: Q-fmp(h,Q-sfr,Q-mnw).
          IF (IUNIT(61).GT.0) THEN                                      !FMP3FM CALL ADDED BY SCHMID
             IF(QBD.EQ.1)                              
     1         CALL FMP3FM(KITER,KPER,KSTP,IUNIT(44),IUNIT(52),
     2                     IUNIT(50),IUNIT(63),IUNIT(55),IUNIT(40),
     3                     IGRID,NGRIDS,ILGR,LGRITER)                   !added inuit for NWT,MNWI,MNW2 by rth
C      ...ALLOW CONVERGENCE CRITERIA FOR MNW WELL PUMPAGE LINKED TO FARM PROCESS
             IF((IUNIT(50).GT.0.OR.IUNIT(52).GT.0).AND.MCLOSE.EQ.1) THEN
               CALL SFMP3PNT(IGRID)
               CALL FMP3QCNVG(IUNIT(52),IUNIT(50),IUNIT(13),IUNIT(9),   !FMP3QCNVG CALL ADDED BY SCHMID, added inuit for NWT by rth
     1                        IUNIT(10),IUNIT(42),IUNIT(63),IUNIT(70))  !added inuit for NWT by rth        
             ENDIF
             CALL FMP3WELBD(KKSTP,KKPER,IUNIT(52),IUNIT(50),
     +                     IUNIT(9),IUNIT(10),IUNIT(55),IUNIT(63),IGRID)!FMP3WELBD & FMP3FNRBD CALLS ADDED BY SCHMID, added inuit for NWT by rth
             CALL FMP3FNRBD(KKSTP,KKPER,IGRID)
             CALL FMP3ETPRT(KKSTP,KKPER,IGRID)
            ENDIF    
            IF(IUNIT(48).GT.0) CALL GWF2BFH2BD(KKSTP,KKPER,IUNIT(1),
     1                         IUNIT(23),IUNIT(37),IGRID)
            IF(ILGR .NE.0)THEN 
              IF(ISCHILD .LE. 0) CALL GWF2LGR2PBD(KKSTP,KKPER,
     1                    IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),NGRIDS)
              IF(ISCHILD .GT. 0) CALL GWF2LGR2CBD(KKSTP,KKPER,
     1                    IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62))
            ENDIF      
CLMT
CLMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
CLMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
CLMT
          IF(IUNIT(49).GT.0) CALL LMT7BD(KKSTP,KKPER,IGRID)
CLMT                              
C
C
!  Set NWT heads to Hdry when head is below bottom.
          IF(IUNIT(63).GT.0)CALL GWF2NWT1BD(IGRID)
C  Observation simulated equivalents
          CALL OBS2BAS7SE(IUNIT(28),IGRID)
          IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
          IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IGRID)
          IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7SE(1,IGRID)
          IF(IUNIT(43).GT.0 .AND. IUNIT(19).GT.0)
     1                              CALL GWF2HYD7IBS7SE(1,IGRID)
          IF(IUNIT(43).GT.0 .AND. IUNIT(54).GT.0)
     1                              CALL GWF2HYD7SUB7SE(1,IGRID)
          IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                              CALL GWF2HYD7STR7SE(1,IGRID)
          IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                              CALL GWF2HYD7SFR7SE(1,IGRID)
C
C7C5---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC)
          IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
     1                                       IGRID)
          IF(IUNIT(37).GT.0)THEN
            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
            ENDIF      
          IF(IUNIT(51).NE.0) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,
     1                       KKPER,IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),
     1                                       IGRID)
          IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
          IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
c-wschmid-03/32/11
c            IF(ILGR .NE.0 .AND. ISCHILD.GE.0) CALL GWF2LGR2COT(KKSTP,
c     1                                        KKPER,IGRID)
            IF(ILGR .NE.0) THEN
              IF(ISCHILD.GE.0) CALL GWF2LGR2COT(KKSTP,KKPER,IGRID)
            ENDIF 
C     1                                            
C------CHECK FOR CHANGES IN HEAD AND FLUX BOUNDARY CONDITIONS 
            IF(IUNIT(48).GT.0) CALL GWF2BFH2OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
          IF ( IUNIT(63).GT.0 ) THEN
            IF ( ICNVGFLG.EQ.0 ) THEN
              IF(ICNVG.EQ.0) GO TO 110
            END IF
          ELSE
            IF(ICNVG.EQ.0) THEN
            NCVGERR=NCVGERR+1
            IF(LSTCHK(3)) THEN
             WRITE(IOUT,87) BUDPERC
   87        FORMAT(1X,'FAILURE TO MEET SOLVER CONVERGENCE CRITERIA',/
     1       1X,'BUDGET PERCENT DISCREPANCY IS',F10.4)
            ENDIF
            IF(ABS(BUDPERC).GT.STOPER) THEN
              WRITE(IOUT,*) 'STOPPING SIMULATION'
              GO TO 110
            ELSE
              WRITE(IOUT,*) 'CONTINUING EXECUTION'
            END IF
          END IF
                
          END IF
ccrth          END IF
C---------END GRID OT GRID LOOP
      ENDDO
C
C-----END OF TIME STEP (KSTP) LOOP (90) AND STRESS PERIOD (KPER) LOOP (100)
   90   CONTINUE  !End Time-Step Loop
!To stop the timer you do the following code: 
      CALL SYSTEM_CLOCK(COUNT=FINISH)                 ! Stop timing 
      !
      CPU_TIME=REAL( (FINISH-START))/REAL(ClockRate*60) !minutes
      !
      WRITE(*,'(11x A,I6,4X,3A)') 'Stress Period: ',KPER,
     +'Completed With CPU Time of ',
     +NUM2STR( CPU_TIME ),' minutes'
C
  100 CONTINUE !End Stress-Period Loop
C
C
C
C8------END OF SIMULATION
  110 DO IGRID = 1, NGRIDS
        CALL SGWF2BAS7PNT(IGRID)
        IF(IUNIT(52).NE.0 .AND. ICNVG.NE.0)
     1     CALL GWF2MNW17OT(IGRID)
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
      IF(IUNIT(54).GT.0) CALL GWF2SUB7SV(IGRID)
C
C  Observation output
      IF(IUNIT(28).GT.0) CALL OBS2BAS7OT(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7OT(IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7OT(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7OT(IGRID)
C
C-------OUTPUT RESULTS OF SWR TIMER
      IF(IUNIT(64).GT.0) CALL GWF2SWR7OT(IGRID)

      ENDDO
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
      DO IGRID = 1, NGRIDS
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      CALL SGWF2BAS7PNT(IGRID)
      IF(ILGR.NE.0) CALL GWF2LGR2DA(IGRID)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(6).GT.0) CALL GWF2RIP4DA(IGRID)                        !inserted by schmid
      IF(IUNIT(7).GT.0) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7DA(IGRID)
      IF(IUNIT(10).GT.0) CALL DE47DA(IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7DA(IGRID)
!      IF(IUNIT(70).GT.0) CALL PCGN2DA(IGRID)
      IF(IUNIT(63).GT.0) THEN
        CALL SGWF2NWT1PNT(IGRID)                                        !seb ADDED CALL TO ENSURE THAT LINMETH IS THE APPROPIATE LOCAL VARIABLE
        IF(LINMETH.EQ.1) THEN
          CALL GMRES7DA(IGRID)
        ELSEIF(LINMETH.EQ.2) THEN
          CALL XMD7DA(IGRID)
!        ELSEIF(LINMETH.EQ.3) THEN
!          CALL SAMG7DA(IGRID)
        END IF
        CALL GWF2NWT1DA(IGRID)
      END IF
      IF(IUNIT(62).GT.0) CALL GWF2UPW1DA(IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7DA(IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7DA(IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7DA(IUNIT(22),
     1                                              IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7DA(IGRID)
      IF(IUNIT(42).GT.0) CALL GMG7DA(IGRID)
      IF(IUNIT(70).GT.0) CALL PCGN2DA(IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7DA(IGRID)  !SWR - JDH
      IF(IUNIT(65).GT.0) CALL GWF2SWI2DA(IGRID)  !SW1 - JDH
ccrth      IF(IUNIT(66).GT.0) CALL GWF2XXX7DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(49).GT.0) CALL LMT7DA(IGRID)
      IF(IUNIT(61).GT.0) CALL FMP3DA(IGRID)
        CALL GWF2BAS7DA(IGRID)
      ENDDO
C
C10-----END OF PROGRAM.
      IF(NCVGERR.GT.0) THEN
        WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',
     1          NCVGERR,' TIME(S)'
      ELSE
        WRITE(*,*) ' Normal termination of simulation'
      END IF
      CALL USTOP(' ')
C
      END PROGRAM MF_OWHM
C
C
      SUBROUTINE GETNAMFIL(ILGR,NGRIDS,FNAME,ILUNIT)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      CHARACTER*(*) FNAME
      CHARACTER*700 COMLIN,LINE
      LOGICAL EXISTS
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
        COMLIN=' '
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
        ICOL = 1
        IF(COMLIN.NE.' ') THEN
          FNAME=COMLIN
        ELSE
   15   WRITE (*,*) ' Enter the name of the NAME FILE or LGR CONTROL ',
     &              'FILE:'
          READ (*,'(A)') FNAME
          CALL URWORD(FNAME,ICOL,ISTART,ISTOP,0,N,R,0,0)
          FNAME=FNAME(ISTART:ISTOP)
          IF (FNAME.EQ.' ') GOTO 15
        ENDIF
        INQUIRE (FILE=FNAME,EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
          NC=INDEX(FNAME,' ')
          FNAME(NC:NC+3)='.nam'
          INQUIRE (FILE=FNAME,EXIST=EXISTS)
          IF(.NOT.EXISTS) THEN
            WRITE (*,480) FNAME(1:NC-1),FNAME(1:NC+3)
  480       FORMAT(1X,'Can''t find name file ',A,' or ',A)
            CALL USTOP(' ')
          ENDIF
        ENDIF
C1A-----CHECK FOR LGR KEYWORD.  IF AN LGR SIMULATION, THEN READ THE 
C1A-----NUMBER OF GRIDS AND LEAVE FILE OPEN FOR PARSING.
C1A-----IF NOT LGR, THEN CLOSE FILE AND CONTINUE AS NORMAL.
      OPEN(UNIT=ILUNIT,FILE=FNAME,STATUS='OLD',ACTION=ACTION(1))
      CALL URDCOM(ILUNIT,0,LINE)
      ICOL=1
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,1,N,R,0,ILUNIT)
      IF(LINE(ISTART:ISTOP) .EQ. 'LGR') THEN
        ILGR = 1
        WRITE(*,*) ' RUNNING MODFLOW WITH LGR '
        CALL URDCOM(ILUNIT,0,LINE)
        ICOL=1
        CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,NGRIDS,R,0,ILUNIT)
        WRITE(*,*) 'NGRIDS = ', NGRIDS 
      ELSE
        CLOSE(ILUNIT)
      ENDIF
C
      RETURN
      END
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LSTCHK
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
      IF(IPRTIM.GT.0) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,'(1X)')
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
        ENDIF
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010     FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(*,1020) NHOURS,NMINS,NRSECS
 1020     FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &      ' Minutes, ',I2,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          WRITE(*,1030) NMINS,NSECS,MSECS
 1030     FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &      I2,'.',I3.3,' Seconds',/)
        ELSE
          WRITE(*,1040) NSECS,MSECS
 1040     FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
          ENDIF
        ELSEIF (NHOURS.GT.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
          ENDIF
        ELSEIF (NMINS.GT.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,1030) NMINS,NSECS,MSECS
          ENDIF
        ELSE
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,1040) NSECS,MSECS
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
      
      
      SUBROUTINE RESTARTHEADS(IOUT)
C     ******************************************************************
C     READ HEADS FOR RESTART AND COPY INTO HNEW
C     ******************************************************************
C
      USE GLOBAL,      ONLY:STRT,NCOL,NROW,NLAY,IUNITSTART,HNEW,IBOUND,
     +                      IXSEC
      USE GWFBASMODULE,ONLY:HNOFLO
      DOUBLE PRECISION HNF
      CHARACTER*24 ANAME(1)
      DATA ANAME(1) /'            RESTART HEAD'/
C        SPECIFICATIONS:
C     ------------------------------------------------------------------      
C
C8G-----READ INITIAL HEADS FOR RESTART.
      IF(IXSEC.EQ.0) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL(STRT(:,:,KK),ANAME(1),NROW,NCOL,KK,IUNITSTART,IOUT)
  300    CONTINUE
      ELSE
         CALL U2DREL(STRT(:,:,1),ANAME(1),NLAY,NCOL,-1,IUNITSTART,IOUT)
      END IF
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
      HNF = HNOFLO
      DO 400 K=1,NLAY
      DO 400 I=1,NROW
      DO 400 J=1,NCOL
      HNEW(J,I,K)=STRT(J,I,K)
      IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
  400 CONTINUE
      RETURN 
      END SUBROUTINE

      SUBROUTINE PRINT_MAIN_HEADER(FN)  !SET TO 6 FOR CMD PROMPT seb
      IMPLICIT NONE
      INTEGER, INTENT(IN)::FN
C1-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER(:),ALLOCATABLE:: OWHM,MFPACSUP,VERSION_OWHM
      CHARACTER(:),ALLOCATABLE:: VERSION_MF, VERSION_FMP
      CHARACTER(:),ALLOCATABLE:: VERSION_SWR,VERSION_LGR
      CHARACTER(:),ALLOCATABLE:: VERSION_SWI,VERSION_NWT
C2------SET NAMES TO PRINT
      OWHM='OWHM 1.0'                      ! TITLE
      !MFPACSUP='FMP3-LGR2-NWT1-SWR1-SWI2'  ! PROCESS SUPPORTED
      VERSION_OWHM='1.0.0   11/05/2014'    
      VERSION_MF  ='1.11.0  08/08/2013'        
      VERSION_FMP ='3.0.0   08/01/2014'       
      VERSION_SWR ='1.03.0  09/24/2013'       
      VERSION_SWI ='2.0.0   07/22/2013'     
      VERSION_LGR ='2.0.0   09/19/2013'      
      VERSION_NWT ='1.0.9   07/01/2014'     
C3------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      WRITE (FN,1) OWHM,VERSION_OWHM,
     + VERSION_MF, 
     + VERSION_FMP,
     + VERSION_SWR,
     + VERSION_SWI,
     + VERSION_LGR,
     + VERSION_NWT
C
    1  FORMAT (/,23X,A, /, 4X,
     + 'U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE ',/ 9x
     + 'ONE-WATER HYDROLOGIC-FLOW MODEL [OWHM]',/,14X,
!     + 'WITH ',A, / 14X !REMOVED MFPACSUP Change 14x to 13x
     + 'Version ',A /,/ 
     + ' INCLUDES:', / 10x
     + 'MODFLOW-2005 Version ',A,        / 10x
     + 'FMP          Version ',A,        / 10x
     + 'SWR          Version ',A,        / 10x
     + 'SWI          Version ',A,        / 10x
     + 'LGR          Version ',A,        / 10x
     + 'NWT          Version ',A / /)
C
      END SUBROUTINE