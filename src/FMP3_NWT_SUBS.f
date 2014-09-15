      MODULE FMPFUNCT 
C     ******************************************************************
C     MODULE CONTAINS FMP FUNCTIONS THAT ARE NOT DEPENDENT ON FMPMODULE
C     ALLOWS FOR FUNCTION IMPLICIT INTERFACE FROM MODULE
C  ***THIS MODULE MUST BE COMPILED AFTER GWFMNW1MODULE and GWFMNW2MODULE***
C     ******************************************************************
      IMPLICIT NONE
      CONTAINS
      FUNCTION RTFUNC(D,M,N,Y,XACC,IC,IR)                               ! seb VARIABLE J REMOVED, ONLY PROVIDES CONVERGENCE ITERATION, BUT NEVER USED. CAN NOT MAKE PURE ROUTINE BECAUSE OF WRITE(*,*) AND STOP STATMENT
C     ******************************************************************
C     SOLUTION OF ANALYTICAL FUNTION FITTING VERTICAL PRESSURE HEAD
C     DISTRIBUTION OVER DEPTH USING BISECTION METHOD.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER,         INTENT(IN ):: IC,IR
      DOUBLE PRECISION,INTENT(IN ):: D,M,N,Y,XACC
      DOUBLE PRECISION:: RTFUNC    
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      INTEGER, PARAMETER:: JMAX=100                                     !MAXIMUM ALLOWED NUMBER OF BISECTIONS.
      DOUBLE PRECISION, PARAMETER:: TOL= 1D-5                           !PRECISION TO SOLVE ROOT AT
      DOUBLE PRECISION:: X1,X2,DX,F,FMID,XMID
      INTEGER:: J
C     ------------------------------------------------------------------
C
C1===== DEFINE EXCLUSIONS
      IF(D.LE.1D0) THEN
        IF(Y.LT.D) RTFUNC=Y
        IF(Y.GE.D) RTFUNC=D
        RETURN
      ELSE IF(D.GT.1D0.AND.(DABS(D-M).LT.XACC.OR.Y.LE.1D0)) THEN
        RTFUNC=Y
        RETURN
      END IF
C
C2===== DEFINE LATERAL BOUNDS X1 AND X2
      IF(Y.GE.D) THEN
          IF(N.GE.1D-30) THEN
              X1 = ((DLOG(Y/D)/DLOG(M/D))**(1D0/N))*(D-1D0)+1D0 - XACC
          ELSE
              X1 = ((DLOG(Y/D)/DLOG(M/D))**(1D30))*(D-1D0)+1D0 - XACC
!              X1 = 1D0 - XACC
          END IF
      END IF
      IF(Y.LT.D) X1=1D0
      IF(X1.LT.1D0) X1=1D0
      IF(N.GE.1D-30) THEN
        X2 = ((DLOG((Y+D)/D)/DLOG(M/D))**(1/N))*(D-1D0)+1D0 +XACC
      ELSE
        X2 = ((DLOG((Y+D)/D)/DLOG(M/D))**(1D30))*(D-1D0)+1D0 +XACC
!        X2 = 1D0 + XACC
      END IF
      IF(X2.GT.Y) X2=Y +XACC
C
C3===== FIND THE ROOT OF A FUNCTION KNOWN TO LIE BETWEEN X1 AND X2.
      FMID=FUNC(X2,D,M,N,Y)
      F=FUNC(X1,D,M,N,Y)
      IF(F*FMID.GE.0D0) STOP "ROOT MUST BE BRACKETED IN RTFUNC"
C
C3A-----ORIENT SEARCH SO THAT F>0 LIES AT X+DX    
      IF(F.LT.0D0)THEN
          RTFUNC=X1
          DX=X2-X1
      ELSE
          RTFUNC=X2
          DX=X1-X2
      ENDIF
C
C3B-----BISECTION LOOP
C       (REDEFINE ROOT, RETURNED AS RTFUNC, UNTIL ITS ACCURACY IS ? XACC)
      DO J=1,JMAX
          DX=DX*.5D0
          XMID=RTFUNC+DX
          FMID=FUNC(XMID,D,M,N,Y)
          IF(FMID.LE.0D0) RTFUNC=XMID
          IF(DABS(DX).LT.XACC .OR. DABS(FMID).LE.TOL) EXIT
      END DO
      IF (J.EQ.JMAX)THEN
        WRITE(*,1) -Y,IR,IC
        STOP
      END IF
    1 FORMAT("TOO MANY BISECTIONS WHEN SOLVING ANALTYICAL FUNCTION ",/,
     1"DEPTH(PSI) FOR A PRESSURE HEAD OF ",F10.2,
     2" CM AT ROW ",I5," AND AT COLUMN ",I5)

C
C4==== END ==============================================================================================       
   12 END FUNCTION RTFUNC
C
C
      PURE FUNCTION FUNC(X,D,M,N,Y)
C     ******************************************************************
C     FORMULATION OF ANALYTICAL FUNTION F(X)=Y AS F(X)-Y=0
C     ******************************************************************
C        SPECIFICATION:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION,INTENT(IN ):: X,D,M,N,Y
      DOUBLE PRECISION:: FUNC
C     ------------------------------------------------------------------
C
      FUNC = D*((M/D)**(((X-1D0)/(D-1D0))**N))+X-D-Y
C
C===== END ==============================================================================================
      END FUNCTION FUNC
C
      END MODULE FMPFUNCT
C
C
      MODULE FMPMAIN
      !IMPLICIT NONE
      USE GLOBAL, ONLY:LSTCHK
      PRIVATE:: LSTCHK
      CONTAINS
      SUBROUTINE FMP3AR(IN,IUNITSFR,IUNITMNW1,IUNITMNW2,
     1                  IUNITUZF,IUNITNWT,IUNITDRT,IGRID,ILGR)
C-----VERSION 2 09/18/2009 FMP3AR
C     ******************************************************************
C     READ COMMENT RECORDS, PARAMETER DIMENSIONS, AND FLAGS;
C     PRINT REPORT TO LIST FILE;
C     ALLOCATE ARRAY STORAGE FOR FARM PROCESS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,NPER,PERLEN
      USE GWFSFRMODULE, ONLY:NSTRM,NSEGDIM,ISTRM,IDIVAR,DVRSFLW,SGOTFLW,
     1                       STRM,SEG
      USE GWFUZFMODULE, ONLY:IUZFBND,IRUNBND,FINF,EXCESPP,VKS,SEEPOUT,
     1                       REJ_INF
      USE GWFMNW1MODULE, ONLY:WELL2
      USE GWFBASMODULE, ONLY:IBDOPT
      USE LGRMODULE,    ONLY:ISFRGRID,LGRDAT
      USE CVT2STR, ONLY: NUM2STR
      USE UTIL_MODULE, ONLY:FILE_IO_ERROR
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER IN,IUNITSFR,IUNITMNW1,IUNITUZF,IGRID,ILGR,IUNITNWT
      INTEGER NPP,MXVL,IUNITMNW2,IUNITDRT                               !Added for NWT connection to Farm wells by rth

C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C      INTEGER IVAR(13)
      INTEGER:: IERR
      CHARACTER*700 LINE
      CHARACTER*46 ANAME(13)
      CHARACTER*8 FLNAM(13)
      CHARACTER*6 SWPR
      CHARACTER*8 FBTYPE
      DATA ANAME(1)  /'ROOT ZONE DEPTHS ............................ '/
      DATA ANAME(2)  /'CONSUMPTIVE USE FLUX ........................ '/
      DATA ANAME(3)  /'REFERENCE EVAPOTRANSPIRATION FLUX ........... '/
      DATA ANAME(4)  /'PRECIPITATION FLUX .......................... '/
      DATA ANAME(5)  /'FRACTIONS OF TRANS. & EVAP. OF CONSUMPTIVE USE'/
      DATA ANAME(6)  /'FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF '/
      DATA ANAME(7)  /'ON-FARM EFFICIENCIES ........................ '/
      DATA ANAME(8)  /'CROP BENEFITS LIST .......................... '/
      DATA ANAME(9)  /'WATER COST COEFFICIENTS FOR EACH FARM ....... '/
      DATA ANAME(10) /'DIVERSION REACHES FOR SEMI-ROUTED DELIVERIES. '/
      DATA ANAME(11) /'REACHES RECEIVING SEMI-ROUTED RETURNFLOW .... '/
      DATA ANAME(12) /'WATER-BALANCE SUBREGIONS (FARMS)............. '/ !Added Notice for IFRMFL when to read flag>0 --rth
      DATA ANAME(13) /'GROUNDWATER ALLOTMENTS (FARMS)............... '/   !Added Notice for IALLOTGW when to read flag>0 --rth
      DATA FLNAM(1)  /'IRTFL   '/
      DATA FLNAM(2)  /'ICUFL   '/
      DATA FLNAM(4)  /'IPFL    '/
      DATA FLNAM(5)  /'IFTEFL  '/
      DATA FLNAM(6)  /'IIESWFL '/
      DATA FLNAM(7)  /'IEFFL   '/
      DATA FLNAM(8)  /'IBEN    '/
      DATA FLNAM(9)  /'ICOST   '/
      DATA FLNAM(10) /'ISRDFL  '/
      DATA FLNAM(11) /'ISRRFL  '/
      DATA FLNAM(12) /'IFRMFL  '/                                       !Added Notice for IFRMFL when to read flag --rth
      DATA FLNAM(13) /'IALLOTGW'/                                       !Added Notice for IFRMFL when to read flag --rth
      INTEGER LLOC,LLOC2,ISTART,ISTOP,N,I,ICB,DATE_TIME(8),NF,NS,NC
      REAL R,SIMLEN 
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
      CALL SGWF2NWT1PNT(Igrid)   !seb lgr
      CALL SGWF2UZF1PNT(Igrid)   !seb lgr
C
C     ALLOCATE SPACE FOR CONSTANTS:
      ALLOCATE(NPFWL,MXPFW,IBLOCK,
     2  MXACTFW,NFARMS,NSOILS,NCROPS,IRTFL,ICUFL,IPFL,IFTEFL,IIESWFL,
     3  IEFFL,IEBFL,IROTFL,IDEFFL,IBEN,ICOST,ICCFL,INRDFL,MXNRDT,ISRDFL,
     4  IRDFL,ISRRFL,IRRFL,IALLOTSW,IFWLCB,IFNRCB,ISDPFL,IOPFL,IPAPFL,
     5  IFBPFL,IPFWEL,IWELLFLD,QBD,MCLOSE,IRTPFL,IFRMFL,IALLOTGW,
     6  MXFWEL,NFWELS,NFWLVL,IFWLAL,IFWLPB,NNPFWL,LENSIM,NAUX,
     7  ISTARTFL,MAXAUX)
      ALLOCATE(PCLOSE,ALLOTSW,QCLOSE,HPCT,RPCT)
      ALLOCATE(IVAR(13))                                                ! Added IFRM and GWAllot variables
      ALLOCATE(FMPOUT, IETPFL)                                          ! Added FMPOUT to hold file names and unit numbers seb
C INITIALIZE VARIABLES
      NPFWL=0; MXPFW=0; IBLOCK=0; 
      MXACTFW=0; NFARMS=0; NSOILS=0; NCROPS=0; IRTFL=0;
      ICUFL=0; IPFL=0; IFTEFL=0; IIESWFL=0; IEFFL=0;
      IEBFL=0; IROTFL=0; IDEFFL=0; IBEN=0; ICOST=0; ICCFL=0; 
      INRDFL=0; MXNRDT=0; ISRDFL=0; IRDFL=0; ISRRFL=0; IRRFL=0; 
      IALLOTSW=0; IFWLCB=0; IFNRCB=0; ISDPFL=0; IOPFL=0;
      IPAPFL=0; IFBPFL=0; IPFWEL=0; IWELLFLD=0; QBD=0; MCLOSE=0; 
      IRTPFL=0; IFRMFL=0; IALLOTGW=0; MXFWEL=0; NFWELS=0; 
      NFWLVL=0; IFWLAL=0; IFWLPB=0; NNPFWL=0; LENSIM=0; NAUX=0; 
      ISTARTFL=0; MAXAUX=0
      !
      PCLOSE=0; ALLOTSW=0; QCLOSE=0; HPCT=0; RPCT=0
      IVAR=0
      IETPFL=0
C
C1===== IDENTIFY PACKAGE AND INITIALIZE NFWELS ==============================================================
      CALL DATE_AND_TIME(VALUES = DATE_TIME)
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1) DATE_TIME(2),DATE_TIME(3),DATE_TIME(1),
     1 DATE_TIME(5),DATE_TIME(6),DATE_TIME(7)
      ENDIF
    1 FORMAT(//,1X,'FMP3 -- FARM PROCESS, VERSION 3.0, ',
     2 I2.2,'/',I2.2,'/',I4,' ',I2.2,':',I2.2,':',I2.2,/,
     1' INPUT READ FROM UNIT',I5/)
      NFWELS=0
      NNPFWL=0
C
C ====== SETTINGS FOR LOCAL GRID REFINEMENT IF SFR PACKAGE OF CURRENT GRID IS INACTIVE
      IF(ILGR.NE.0) THEN
        IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) THEN
        IUNITSFR=1
        ALLOCATE(NSTRM)
        NSTRM=0
        CALL SGWF2SFR7PSV(IGRID)
        ENDIF
      ENDIF
C
C2===== READ COMMENT RECORDS, PARAMETER DIMENSIONS, AND FLAGS ===============================================
C
C2A-----READ COMMENT
      CALL URDCOM(IN,IOUT,LINE)
C
C2B-----READ FLAGS IN FIRST LINE OF INPUT FILE (IF WORD "PARAMETER" IS SPECIFIED):
C       Parameter Dimensions: for Number of farm well paramters 
C                                 Maximum number of parameter farm wells      
      CALL UPARLSTALPRTOCH(IN,IOUT,LINE,NPFWL,MXPFW,ILGR,IGRID)
C
C2C-----CHECK IF FLAG ARE READ IN ONE LINE (COMPATIBLE WITH FMP1) OR IN SEPARATE LINES
C       FOR EACH FLAG BLOCK
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IBLOCK=0
      IF(LINE(ISTART:ISTOP).EQ.'FLAG_BLOCKS') THEN
         IBLOCK=1
         READ(IN,'(A)') LINE   
      ENDIF
C
C2D-----READ FLAGS IN SECOND LINE OR SEPARATE LINES PER FLAG BLOCK
      LLOC=1
C2D1----Parameter Dimensions: for Maximum number of active farm wells incl. paramter- & non-parameter ones,
C                                 Number of farms, Number of crop types,Number of soil types,
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTFW,R,IOUT,IN)
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        IF(INDEX(LINE,' P ').GT.0) THEN
          MXACTFW=MXACTFW+FMPDAT(1)%MXACTFW
          LLOC=INDEX(LINE,' P ')+3       
        ENDIF
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NFARMS,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCROPS,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSOILS,R,IOUT,IN)
C
C2D2----When-to-Read-Flags:   for Root depth, Consumptive use, Precipitation,
C                                 Fraction-of-transpiration-&-evaporation-of-crop-consumptive-use,
C                                 Fraction-of-inefficient-losses-to-SW-runoff flag,
C                                 Efficiency;
ccrth   Added IFRMFL for multiple WBS changes for different stress periods --rth
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
C
ccrth Add IFRMFL for multiple WBS regions in different stress preiods  --rth    
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IFRMFL=FMPDAT(1)%IFRMFL
        ELSE
          LLOC=1 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFRMFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFRMFL,R,IOUT,IN)
      ENDIF
ccrth
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IRTFL=-1
        ELSE
          LLOC=LLOC2 !REWIND TO BEGINNING OF LINE
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRTFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRTFL,R,IOUT,IN)
      ENDIF
C 
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          ICUFL=-2
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICUFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICUFL,R,IOUT,IN)
      ENDIF
C           
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IPFL=-1
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPFL,R,IOUT,IN)
      ENDIF
C
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IFTEFL=-1
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTEFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTEFL,R,IOUT,IN)
      ENDIF
C
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IIESWFL=-1
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IIESWFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IIESWFL,R,IOUT,IN)
      ENDIF
C      
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IEFFL=-1
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IEFFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IEFFL,R,IOUT,IN)
      ENDIF
c      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IEFFL,R,IOUT,IN)
C
C2D3----Water Policy Flags:   for Efficiency Behavior, Crop rotation,
C                                 Deficiency Scenario (optional flags: Crop-Benefits, Water-Cost Coefficients)
ccrth                             Groundwater Allotment Flag added --rth
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IEBFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IROTFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDEFFL,R,IOUT,IN)
      IF(IDEFFL.GT.0) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IBEN,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICOST,R,IOUT,IN)
      ENDIF
C
ccrth      Added reading IALLOTGW with water-policy flags--rth
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IALLOTGW=-1
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IALLOTGW,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IALLOTGW,R,IOUT,IN)
      ENDIF
ccrth
C2D4----Consumptive Use Concept Flag
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          ICCFL=-1
        ELSE
          LLOC=1 !REWIND TO BEGINNING OF LINE
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCFL,R,IOUT,IN)
      ENDIF
c      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCFL,R,IOUT,IN)
C
C2D5----Surface-Water Flags:  for Non-Routed Surface-Water Delivery (optional: Max.# of non-routed delivery types),
C                                 Semi-Routed Surface-Water Delivery,
C                                 Semi-Routed Runoff-Returnflow, 
C                                 Routed Surface-Water Delivery,
C                                 Surface-water allotment (optional: closure criterion for simulated diversions from a river into a diversion-segment (canal))
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          INRDFL=-1
          MXNRDT=FMPDAT(1)%MXNRDT
        ELSE
          LLOC=1 !REWIND TO BEGINNING OF LINE
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INRDFL,R,IOUT,IN)
        ENDIF
      ELSE
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,INRDFL,R,IOUT,IN)
      ENDIF
      IF(INRDFL.EQ.1) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXNRDT,R,IOUT,IN)
      ENDIF
C      
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          ISRDFL=-1 !FLAG MUST DIFFER FROM PARENT FLAG TO SKIP READING
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISRDFL,R,IOUT,IN)
        ENDIF
      ELSE     
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISRDFL,R,IOUT,IN)
      ENDIF
C      
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IRDFL=FMPDAT(1)%IRDFL !PARENT FLAG IS OK AS NOTHING IS READ
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRDFL,R,IOUT,IN)
        ENDIF
      ELSE     
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRDFL,R,IOUT,IN)
      ENDIF
C      
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          ISRRFL=-1 !FLAG MUST DIFFER FROM PARENT FLAG TO SKIP READING
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISRRFL,R,IOUT,IN)
        ENDIF
      ELSE     
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISRRFL,R,IOUT,IN)
      ENDIF
C
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IRRFL=FMPDAT(1)%IRRFL !PARENT FLAG IS OK AS NOTHING IS READ
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRRFL,R,IOUT,IN)
        ENDIF
      ELSE     
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRRFL,R,IOUT,IN)
      ENDIF
C
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        LLOC2=LLOC
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
          IALLOTSW=-1 !FLAG MUST DIFFER FROM PARENT FLAG TO SKIP READING
        ELSE
          LLOC=LLOC2 !REWIND
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IALLOTSW,R,IOUT,IN)
        ENDIF
      ELSE     
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IALLOTSW,R,IOUT,IN)
      ENDIF      
C      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IALLOTSW,R,IOUT,IN)
      IF(IALLOTSW.GT.1) THEN
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,PCLOSE,IOUT,IN)
      ENDIF
C
C2D6----Print Flags or Units: of  cell-by-cell flow terms of Farm wells, Farm net recharge,
C                                 farm-by-farm Farm demand & supply budget;
C                                 farm-by-farm Farm Budget.
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFWLCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNRCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISDPFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFBPFL,R,IOUT,IN)      
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IETPFL,R,IOUT,IN)        !READ IN ET PRINT FLAG seb
      IF(IUNITSFR.GT.0) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IRTPFL,R,IOUT,IN)
      ENDIF      
      IF(IDEFFL.GT.0) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOPFL,R,IOUT,IN)
      ENDIF
      IF(IALLOTSW.GT.1) THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPAPFL,R,IOUT,IN)
      ENDIF
C

C2D7----Optional flags that define auxiliary parameters:
C       "AUX" flag (auxilliary paramters to be read for each farm well from the farm wells list)
      MAXAUX=5                                                          !MAX ROW DIMENSION OF FWLAUX
      NAUX=0
      IFWLAL=0
      IPFWEL=1
      IWELLFLD=0
      QBD=0
      MCLOSE=0
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
      ALLOCATE(FWLAUX(MAXAUX),FWLAUXORDER(MAXAUX))                      !seb Changed from 5 to variable MAXAUX
      FWLAUX=''
      FWLAUXORDER=''
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.5) THEN
            NAUX=NAUX+1
            FWLAUX(NAUX)=LINE(ISTART:ISTOP)
            SELECT CASE (FWLAUX(NAUX))                                  !seb STATIC ASSIGNMENT OF AUX VARIABLES TO FWLAUXORDER THIS ORDER MUST BE PRESERVED IN SUBROUTINE FMP3WELRD
             CASE ("QMAXRESET");FWLAUXORDER(1)=FWLAUX(NAUX)
             CASE ("NOCIRNOQ"); FWLAUXORDER(2)=FWLAUX(NAUX)
             CASE ("LGRGRID");  FWLAUXORDER(3)=FWLAUX(NAUX)
          CASE  DEFAULT
             WRITE(*,*) "ERROR AUX VARIABLE "//TRIM(FWLAUX(NAUX))
     +                //" NOT CURRENTLY ACCEPTED"
             WRITE(IOUT,*) "ERROR AUX VARIABLE "//TRIM(FWLAUX(NAUX))
     +                //" NOT CURRENTLY ACCEPTED"
          END SELECT
         END IF !(NAUX.LT.5)
         GO TO 10 
      ENDIF
C
C2D8----Other optional flags: "CBC" flag (memory allocated to store cell-by-cell flow for each well);
C                        Flag telling not to print the list of farm well attribitues to the list file;
C                        Flag telling that wellfields supply non-routed deliveries to farms;
C                        Flag telling to recompute flow rates at the end of time step loop.
      IF(IBLOCK.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
      ENDIF
   11 IF(IBLOCK.EQ.1) CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CBC') THEN
         IFWLAL=1
         IF(IBLOCK.EQ.0) GO TO 10
         IF(IBLOCK.EQ.1) GO TO 11
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         IPFWEL = 0
         IF(IBLOCK.EQ.0) GO TO 10
         IF(IBLOCK.EQ.1) GO TO 11
      ELSE IF(LINE(ISTART:ISTOP).EQ.'WELLFIELD') THEN
         IWELLFLD = 1
         IF(IBLOCK.EQ.0) GO TO 10
         IF(IBLOCK.EQ.1) GO TO 11
      ELSE IF(LINE(ISTART:ISTOP).EQ.'RECOMP_Q_BD') THEN
         QBD = 1
         IF(IBLOCK.EQ.0) GO TO 10
         IF(IBLOCK.EQ.1) GO TO 11
      ELSE IF(LINE(ISTART:ISTOP).EQ.'MNWCLOSE') THEN
         MCLOSE = 1
         IF(IBLOCK.EQ.0) GO TO 10
         IF(IBLOCK.EQ.1) GO TO 11      
      END IF
C
      IF(MCLOSE.EQ.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,QCLOSE,IOUT,IN)
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,HPCT,IOUT,IN)
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,RPCT,IOUT,IN)
      ENDIF
ccrth==Add allocation of variables needed to control smoothed reduced pumpage for NWT application within FMP
       IF(IUNITNWT.GT.0)THEN
        ALLOCATE(QXTF(NFARMS))
        ALLOCATE(QSAVE(MXACTFW))
        ALLOCATE(PSIRAMPF,SATTHK,IUNITRAMPF)       
        PSIRAMPF = 0.1
        SATTHK=0.1
        IUNITRAMPF=IOUT
        QXTF=0.D0
        QSAVE=0.D0
       ELSEIF(IUNITNWT.LE.0)THEN
        ALLOCATE(QXTF(1)) 
        ALLOCATE(QSAVE(1)) 
        IUNITNWT=0
ccrth        IUNITRAMPF=0
       ENDIF
C
C3===== PRINT A REPORT ABOUT THE INFORMATION READ ABOVE INTO LIST FILE =====================================
C
C3A-----PARAMETER DIMENSIONS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,2) MXACTFW,NFARMS,NSOILS,NCROPS
      ENDIF
    2 FORMAT(/,1X,'DIMENSIONS:',/,
     1       3X,'MAXIMUM OF',I7,' FARM WELLS',/,
     2       3X,'MAXIMUM OF  ',I7,' FARMS',/,                        !now NFARMS is maximum number of Farms for all stress periods --rth
     3       3X,'TOTAL OF  ',I7,' SOIL-TYPES',/,
     4       3X,'TOTAL OF  ',I7,' CROP-TYPES')
C
C ------LGR EXCLUSIONS
      IF(ILGR.GT.0) THEN
        IF(IDEFFL.NE.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,24) "DEFICIENCY SCENARIOS (RESET IDEFFL=0)   "
          ENDIF
          STOP        
        ELSEIF(IALLOTSW.GT.1) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,24) "PRIOR APPROPRIATION (RESET IALLOTSW=0 OR 1)"
          ENDIF
          STOP
        ENDIF
   24   FORMAT(1X,'INPUT-ERROR:',/,1X,
     1  'LINK BETWEEN FARM PROCESS AND LOCAL GRID REFINEMENT PACKAGE ',
     2  'NOT YET AVAILABLE FOR:',/,1X,A40)
      ENDIF
C
C3B-----"WHEN-TO-READ"-FLAGS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,'(/,1X,"WHEN TO READ SETTINGS:")')
      ENDIF
C3B1-----CHECK FOR EXCLUSIONS, PRINT MESSAGE, AND STOP PROGRAM
      IF(IDEFFL.LT.1) THEN                                              !IBEN AND ICOST ARE NOT USED IF IDEFFL < 1
      IBEN=1                                                            !IBEN=1 IS SET AS DUMMY TO ALLOW EXCLUSIONS CHECK
      ICOST=1                                                           !ICOST=1 IS SET AS DUMMY TO ALLOW EXCLUSIONS CHECK
      ENDIF
      IVAR(1)=IRTFL                                                     !IVAR(1 to 7) & IVAR(10 to 11) ARE REQUIRED FLAGS
      IVAR(2)=ICUFL
      IVAR(4)=IPFL
      IVAR(5)=IFTEFL
      IVAR(6)=IIESWFL
      IVAR(7)=IEFFL
      IVAR(8)=IBEN                                                      !IVAR(8 & 9) ARE OPTIONAL FLAGS
      IVAR(9)=ICOST
      IVAR(10)=ISRDFL
      IVAR(11)=ISRRFL
      IVAR(12)=IFRMFL
      IVAR(13)=IALLOTGW
C
      DO I=1,4
      IF((I.EQ.1.AND.(IVAR(I).LT.-1.OR.IVAR(I).GT.3.OR.IVAR(I).EQ.0)).OR
     1  .(I.EQ.2.AND.(IVAR(I).LT.-2.OR.IVAR(I).GT.3.OR.IVAR(I).EQ.0)).OR
     2  .(I.EQ.4.AND.(IVAR(I).NE.-1.AND.IVAR(I).NE.2.AND.IVAR(I).NE.3)))
     3THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,21) ANAME(I),FLNAM(I),FLNAM(I),FLNAM(I)
      ENDIF
   21 FORMAT(1X,'INPUT-ERROR:',/,1X,A46,
     1'MUST BE SPECIFIED FOR EACH STRESS PERIOD (',A7,'= 2),',/,
     247X,'OR CALCULATED FOR EACH TIME STEP (',A7,'= 3)',/,
     447X,'OR MUST BE "P" (',A7,'= P)')
      STOP
      ENDIF
      ENDDO
C     
      DO I=5,13
      IF((I.EQ.6.OR.I.EQ.10.OR.I.EQ.11.OR.I.EQ.12.OR.I.EQ.13).AND.
     1(IVAR(I).LT.-1.OR.IVAR(I).GT.2)) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,23) ANAME(I),FLNAM(I),FLNAM(I),FLNAM(I),FLNAM(I)
      ENDIF
   23 FORMAT(1X,'INPUT-ERROR:',/,1X,A46,
     1' MUST BE SPECIFIED FOR EACH STRESS PERIOD (',A7,'= 2),',/,
     248X,'OR FOR EACH SIMULATION (',A7,'= 1)',/,
     348X,'OR MUST BE ZERO (',A7,'= 0)',/,
     448X,'OR MUST BE "P" (',A7,'= P)')
      STOP
      ELSEIF((I.NE.6.AND.I.NE.10.AND.I.NE.11.and.I.EQ.12.AND.I.EQ.13)
     1.AND.(IVAR(I).NE.-1.AND.IVAR(I).NE.1.AND.IVAR(I).NE.2))THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,22) ANAME(I),FLNAM(I),FLNAM(I),FLNAM(I)
      ENDIF
   22 FORMAT(1X,'INPUT-ERROR:',/,1X,A46,
     1' MUST BE SPECIFIED FOR EACH STRESS PERIOD (',A7,'= 2),',/,
     248X,'OR FOR EACH SIMULATION (',A7,'= 1)',/,
     448X,'OR MUST BE "P" (',A7,'= P)')   
      STOP
      ENDIF
      ENDDO
C3B2-----FOR ALL REQUIRED "WHEN-TO-READ" FLAGS, PRINT MESSAGE
      DO I=1,13
       IF(I.le.7.or.I.eq.12)then                                        !Added IFRMFL to When to read flags--rth
      IF(I.EQ.2) THEN
        IF(IVAR(I).EQ.1) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,3) ANAME(I),ANAME(I+1)
          ENDIF
        ELSEIF(IVAR(I).EQ.-1) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,3)
     1    'CROP COEFFICIENTS ........................... ',ANAME(I+1)  
          ENDIF
        ENDIF
        IF(IGRID.GT.1) THEN
          IF(IVAR(I).EQ.-2) THEN
            IF(FMPDAT(1)%IVAR(I).EQ.1) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,3) ANAME(I),ANAME(I+1)
              ENDIF
            ENDIF
            IF(FMPDAT(1)%IVAR(I).EQ.-1)THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,3)
     1       'CROP COEFFICIENTS ........................... ',ANAME(I+1)
              ENDIF
            ENDIF
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,9)
          ENDIF
          ENDIF
        ENDIF
      GOTO 5
      ENDIF
      IF(IVAR(I).EQ.1) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,4) ANAME(I)
        ENDIF
      ENDIF
      IF(IGRID.GT.1)THEN
        IF(IVAR(I).EQ.-1.AND.FMPDAT(1)%IVAR(I).EQ.1)THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,4) ANAME(I)
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,9)
        ENDIF
        ENDIF
      ENDIF
    5 IF(IVAR(I).EQ.2) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,7) ANAME(I)
        ENDIF
      ENDIF
      IF(IGRID.GT.1)THEN
        IF(IVAR(I).EQ.-1.AND.FMPDAT(1)%IVAR(I).EQ.2)THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,7) ANAME(I)
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,9)
        ENDIF
        ENDIF
      ENDIF
      IF(IVAR(I).EQ.3) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,8) ANAME(I)
        ENDIF
      ENDIF
      IF(IGRID.GT.1)THEN
        IF(IVAR(I).EQ.-1.AND.FMPDAT(1)%IVAR(I).EQ.3)THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,8) ANAME(I)
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,9)
        ENDIF
        ENDIF
      ENDIF
      endif     !end block for when to read flags--rth
      ENDDO
    3 FORMAT(3X,A46,' AND',/,3X,A46,' SPECIFIED FOR EACH STRESS PERIOD')
    4 FORMAT(3X,A46,' SPECIFIED FOR THE ENTIRE SIMULATION')     
    7 FORMAT(3X,A46,' SPECIFIED FOR EACH STRESS PERIOD')   
    8 FORMAT(3X,A46,
     1' CALCULATED AS AVERAGE FOR EACH TIME STEP FROM TIME SERIES')     
   9  FORMAT(50X,
     1'IN PARENT MODEL AND USED IN CHILD MODEL WHERE NEEDED',/)
C
C3C-----WATER POLICY FLAGS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,'(/,1X,"WATER POLICY SETTINGS:")')
      ENDIF
C
C3C1----EFFICIENCY BEHAVIOR FLAG
C
C3C1A---CHECK FOR EXCLUSIONS 
      IF(IEBFL.LT.0.OR.IEBFL.GT.4) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,58) "INPUT ERROR: "
      ENDIF
   58 FORMAT(1X,A13,'EFFICIENCY BEHAVIOR FLAG MUST BE 0,1,2, OR 3')
      STOP
      ENDIF
C
C3C1B---PRINT MESSAGE
      IF(LSTCHK(3)) THEN
      IF((IEBFL.EQ.0.OR.IEBFL.EQ.1).AND.IDEFFL.NE.-1)
     1  WRITE(IOUT,51) "EFFICIENCY BEHAVIOR: "
      ENDIF
   51 FORMAT(3X,A21,
     1'CONSERVATIVE BEHAVIOR: CONSTANT EFFICIENCY OVER A TIME STEP')
      IF(LSTCHK(3)) THEN
      IF((IEBFL.EQ.2.OR.IEBFL.EQ.3).AND.IDEFFL.NE.-1)
     1  WRITE(IOUT,52) "EFFICIENCY BEHAVIOR: "
      ENDIF
   52 FORMAT(3X,A21,
     1'CONSERVATIVE BEHAVIOR: CONSTANT DELIVERY OVER A TIME STEP')
C     EFFICIENCY DEPENDS ON REDUCED DELIVERY AS A RESULT OF DEFICIT IRRIGATION
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.EQ.-1)
     1  WRITE(IOUT,53) "EFFICIENCY BEHAVIOR: "
      ENDIF
   53 FORMAT(3X,A21,
     1'EFFICIENT BEHAVIOR: ',
     2'COMPOSITE FARM EFFICIENCY MAY CHANGE OVER A TIME STEP',/24X,
     3'AS A RESPONSE TO REDUCED DELIVERY DURING DEFICIT IRRIGAITON ',
     4'(EFFICIENCY INCREASES)')
C     EFFICIENCY OR DELIVERY HEAD-DEPENDENT?
      IF(LSTCHK(3)) THEN
        IF(IEBFL.EQ.0.OR.IEBFL.EQ.1) WRITE(IOUT,54)
      ENDIF
   54 FORMAT(24X,
     1'EFFICIENCY DOES NOT VARY WITH CHANGING GROUNDWATER LEVEL, BUT'
     2,/,24X,'DELIVERY MAY VARY WITH CHANGING GROUNDWATER LEVEL')
      IF(LSTCHK(3)) THEN
        IF(IEBFL.EQ.2.OR.IEBFL.EQ.3) WRITE(IOUT,55)
      ENDIF
   55 FORMAT(24X,'
     1EFFICIENCY VARIES WITH CHANGING GROUNDWATER LEVEL, BUT'
     2/,24X,'DELIVERY DOES NOT VARY WITH CHANGING GROUNDWATER LEVEL')
C     EFFICIENCY RESET AT BEGINNING OF EACH STRESS PERIOD OR TIME STEP
      IF(LSTCHK(3)) THEN
        IF(IEBFL.EQ.0.OR.IEBFL.EQ.2) WRITE(IOUT,56)
      ENDIF
   56 FORMAT(24X,'EFF. RESET TO SPEC. EFF. AT BEG. OF EACH STRESS PER.')
      IF(LSTCHK(3)) THEN
        IF(IEBFL.EQ.1.OR.IEBFL.EQ.3) WRITE(IOUT,57)
      ENDIF
   57 FORMAT(24X,'EFF. RESET TO SPEC. EFF. AT BEG. OF EACH TIME STEP')
C
C3C2----DEFICIENCY SCENARIO FLAG
      IF(IDEFFL.GT.0) THEN
C
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.1) WRITE(IOUT,931)
      ENDIF
  931 FORMAT(3X,'IF SUPPLY < DEMAND: OPTIMIZE ACREAGE')   
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.3) WRITE(IOUT,932)
      ENDIF
  932 FORMAT(3X,'OPTIMIZE ACREAGE')   
      IF((IDEFFL.EQ.2.OR.IDEFFL.EQ.4).AND.IUNITSFR.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,933)
      ENDIF
  933 FORMAT(1X,'INPUT-ERROR: OPTION OF ACREAGE OPTIMIZATION (WITH WATER
     1 CONSERVATION POOL) CANNOT BE CHOSEN',/,14X,
     2'IF STREAMFLOW ROUTING PACKAGE IS NOT SPECIFIED IN NAME-FILE!',/,
     314X,'==> SELECT IDEFFL=1, OR SPECIFY SFR PACKAGE IN NAME FILE!')
      STOP
      ENDIF
      IF((IDEFFL.EQ.2.OR.IDEFFL.EQ.4).AND.IRDFL.EQ.0.AND.ISRDFL.EQ.0)
     1THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,934)
      ENDIF
  934 FORMAT(1X,'INPUT-ERROR: OPTION OF ACREAGE OPTIMIZATION (WITH WATER
     1 CONSERVATION POOL) CANNOT BE CHOSEN',/,14X,
     2'IF (SEMI-)ROUTED DELIVERIES FROM CANALS TO FARMS DO NOT EXIST!',/
     3,14X,'==> SELECT IDEFFL=1, OR SELECT IRDFL=0 OR ISRDFL=1,OR=2!')
      STOP
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.2) WRITE(IOUT,935)
      ENDIF
  935 FORMAT(3X,
     1'IF SUPPLY < DEMAND: OPTIMIZE ACREAGE (WITH CONSERVATION POOL)')
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.4) WRITE(IOUT,936)
      ENDIF
  936 FORMAT(3X,
     1'OPTIMIZE ACREAGE (WITH CONSERVATION POOL)')
C      
      DO I=8,9
      IF(LSTCHK(3)) THEN
        IF(IVAR(I).EQ.1) WRITE(IOUT,4) ANAME(I)
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(IVAR(I).EQ.2) WRITE(IOUT,7) ANAME(I)
      ENDIF
      ENDDO
C
      ENDIF
C
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.-1) WRITE(IOUT,941)
      ENDIF
  941 FORMAT(3X,'IF SUPPLY < DEMAND: DEFICIT IRRIGATION')
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.-2) WRITE(IOUT,942)
      ENDIF
  942 FORMAT(3X,
     1'IF SUPPLY < DEMAND: DEFICIT IRRIGATION (WITH WATER STACKING)')
      IF(LSTCHK(3)) THEN
        IF(IDEFFL.EQ.0) WRITE(IOUT,943)
      ENDIF
  943 FORMAT(3X,'NO DEFICIENCY SCENARIO APPLIED (ZERO SCENARIO)',
     1/,5X,'ASSUMPTION FOR DEFICIENCY CASE: SUPPLY = DEMAND, I.E.',/,5X,
     2'SUPPLY SUFFICIENTLY SUPPLEMENTED FROM OUTSIDE MODELING DOMAIN') 
C
C3C3----FLAG FOR CROP ROTATION AND NON-IRRIGATION SEASON
      IF(LSTCHK(3)) THEN
        IF(IROTFL.EQ.0) WRITE(IOUT,911)
      ENDIF
  911 FORMAT(3X,'NO NON-IRRIGATION-SEASON HAS BEEN SELECTED')
      IF(LSTCHK(3)) THEN
        IF(IROTFL.GT.0) WRITE(IOUT,921) IROTFL
      ENDIF
  921 FORMAT(3X,'NON-IRRIGATION-SEASON = STRESS PERIOD ',I3)
      IF(LSTCHK(3)) THEN
        IF(IROTFL.LT.0) WRITE(IOUT,922)
      ENDIF
  922 FORMAT(3X,'NO NON-IRRIGATION-SEASON HAS BEEN SELECTED, BUT ',
     1'CROP TYPE CHANGES TEMPORALLY & SPATIALLY AT EVERY STRESS PERIOD')
C
ccrth
C3C3A----FLAG FOR Ground Water Allotments
      IF(LSTCHK(3)) THEN
        IF(IALLOTGW.EQ.0) WRITE(IOUT,937)
      ENDIF
  937 FORMAT(3X,'NO GROUNDWATER ALLOTMENTS (Volumetric Rate) HAVE BEEN',
     1 ' SELECTED -- Maximum Groundwater Delivery limited by Total',
     2 ' Capacity of all Farm wells per Farm per Stress Period')     
      IF(LSTCHK(3)) THEN
        IF(IALLOTGW.EQ.1) WRITE(IOUT,938) IALLOTGW
      ENDIF
  938 FORMAT(3X,'GROUNDWATER ALLOTMENTS (Volumetric Rate) SELECTED',
     1   ' ENTIRE SIMULATION ',I3)
      IF(LSTCHK(3)) THEN
        IF(IALLOTGW.EQ.2) WRITE(IOUT,939) IALLOTGW
      ENDIF
  939 FORMAT(3X,'GROUNDWATER ALLOTMENTS (Volumetric Rate) SELECTED',
     1      ' AT EVERY STRESS PERIOD',I3)
ccrth
C3D-----CONSUMPTIVE USE FLAG
      IF(LSTCHK(3)) THEN
        IF(ICCFL.EQ.2.OR.ICCFL.EQ.4) WRITE(IOUT,952)
      ENDIF
  952 FORMAT(/,1X,
     1'CONSUMPTIVE USE CONCEPT 2: Tgw-act-max = Tc-act-max = Tpot')
      IF(LSTCHK(3)) THEN
        IF(ICCFL.EQ.1) WRITE(IOUT,953)
      ENDIF
  953 FORMAT(/,1X,
     1'CONSUMPTIVE USE CONCEPT 1: Tgw-act-max < Tc-act-max < Tpot',/,3X,
     2'(REDUCTION OF ACTUAL TRANSPIRATION ~',
     3' RED. OF ROOT ZONE BY ZONES OF WILTING & ANOXIA',/,4X,
     4'APPROXIMATED BY CROP- & SOIL-TYPE SPECIFIC ANALYTICAL SOLUTION)')
      IF(LSTCHK(3)) THEN
        IF(ICCFL.EQ.3) WRITE(IOUT,953)
      ENDIF
  954 FORMAT(/,1X,
     1'CONSUMPTIVE USE CONCEPT 3: Tgw-act-max < Tc-act-max < Tpot',/,3X,
     2'(REDUCTION OF ACTUAL TRANSPIRATION ~',
     3' RED. OF ROOT ZONE BY ZONES OF WILTING & ANOXIA',/,4X,
     4'APPROXIMATED BY CROP- & SOIL-TYPE SPECIFIC ANALYTICAL SOLUTION)',
     5'CONNECTED TO UZF FOR INFILTRATION BELOW ROOT ZONE--BE SURE THAT',
     6'ET FLAG IN UZF=0)')
      IF(ICCFL.LT.-1.OR.ICCFL.GT.4.OR.ICCFL.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,'(/,1X,A)')
     1'CROP CONSUMPTIVE USE FLAG MUST BE 1,2,3,4, or "P" !'
      ENDIF
      STOP
      ENDIF
C
C3E-----SURFACE-WATER FLAGS
      IF(LSTCHK(3)) THEN
        write(IOUT,'(/,1X,
     1"SURFACE-WATER SETTINGS (SUPPLY / RETURNFLOW / RIGHTS:")')
      ENDIF
C3E1----NON-ROUTED SURFACE-WATER DELIVERY
      IF(LSTCHK(3)) THEN
        IF(INRDFL.EQ.0) WRITE(IOUT,101)
      ENDIF
  101 FORMAT(3X,'NO NON-ROUTED SURFACE-WATER DELIVERIES EXIST')
      IF(LSTCHK(3)) THEN
        IF(INRDFL.NE.0) WRITE(IOUT,102) MXNRDT
      ENDIF
  102 FORMAT(3X,'FARM DELIVERY REQUIREMENT SUPPLIED BY:',
     1/,5X,'NON-ROUTED SURFACE-WATER DELIVERIES (FIRST PRIORITY).',
     2/,5X,'MAXIMUM NUMBER OF NON-ROUTED DELIVERY TYPES = ',I4)
      IF(INRDFL.LT.-1.OR.INRDFL.GT.1) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,*)"NON-ROUTED SW-DELIVERY FLAG MUST BE 0, 1, OR 'P'!"
      ENDIF
      STOP
      ENDIF
C
C3E2----SEMI-ROUTED SURFACE-WATER DELIVERY
      IF(LSTCHK(3)) THEN
        IF(ISRDFL.EQ.0) WRITE(IOUT,103)
      ENDIF
  103 FORMAT(3X,'NO SEMI-ROUTED SURFACE-WATER DELIVERIES EXIST')
      IF(ISRDFL.GT.0) THEN
      IF(IUNITSFR.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,104)
      ENDIF
  104 FORMAT(1X,'INPUT-ERROR: SEMI-ROUTED SURFACE-WATER DELIVERIES',/,
     114X,'CANNOT OCCUR IF SFR1 PACKAGE IS NOT SPECIFIED IN NAME FILE.',
     2/,14X,'==> SELECT ISRDFL=0 OR SPECIFY SFR1 PACKAGE IN NAME FILE!')
      STOP
      ENDIF
      IF(INRDFL.EQ.0) SWPR="FIRST-"
      IF(INRDFL.NE.0) SWPR="SECOND"
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,105) SWPR
      ENDIF
  105 FORMAT(3X,'FARM DELIVERY REQUIREMENT SUPPLIED BY:',
     1/,5X,'SEMI-ROUTED SURFACE-WATER DELIVERIES (',A6,' PRIORITY).')
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(ISRDFL.EQ.1) WRITE(IOUT,4) ANAME(10)
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(ISRDFL.EQ.2) WRITE(IOUT,7) ANAME(10)
      ENDIF
C
C3E3----FULLY ROUTED SURFACE-WATER DELIVERY
      IF(LSTCHK(3)) THEN
        IF(IRDFL.EQ.0) WRITE(IOUT,106)
      ENDIF
  106 FORMAT(3X,'NO ROUTED SURFACE-WATER DELIVERY EXISTS')
      IF(IRDFL.NE.0) THEN
        IF(IRDFL.LT.-1.OR.IRDFL.GT.1)THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,*) "IRDFL MUST BE -1, 0, OR 1!"
        ENDIF
        STOP      
      ENDIF      
      IF(IUNITSFR.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,107)
      ENDIF
  107 FORMAT(1X,'INPUT-ERROR: FULLY ROUTED SURFACE-WATER DELIVERIES',/,
     114X,'CANNOT OCCUR IF SFR1 PACKAGE IS NOT SPECIFIED IN NAME FILE.',
     2/,14X,'==> SELECT IRDFL=0, OR SPECIFY SFR1 PACKAGE IN NAME FILE!')
      STOP
      ENDIF   
      IF(INRDFL.EQ.0) SWPR="FIRST-"
      IF(INRDFL.NE.0) SWPR="SECOND"
      IF(ISRDFL.EQ.0) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,108) SWPR
      ENDIF
  108 FORMAT(3X,'FARM DELIVERY REQUIREMENT SUPPLIED TO ALL FARMS:',/
     1,5X,'BY FULLY ROUTED SURFACE-WATER DELIVERIES (',A6,' PRIORITY).')
      ELSE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,109) SWPR
      ENDIF
  109 FORMAT(3X,'FARM DELIVERY REQUIREMENT SUPPLIED BY:',/,5X,
     1'FULLY ROUTED SURFACE-WATER DELIVERIES (',A6,' PRIORITY).',/,5X,
     2'TO FARMS WITHOUT SPECIFIED SEMI-ROUTED DIVERSION LOCATIONS')
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(IRDFL.EQ.1) WRITE(IOUT,110)
      ENDIF
  110 FORMAT(5X,'FROM THE UPPERMOST REACH OF A DIVERSION SEGMENT',
     1' LOCATED WITHIN EACH FARM')
      IF(LSTCHK(3)) THEN
        IF(IRDFL.EQ.-1) WRITE(IOUT,111)
      ENDIF
  111 FORMAT(5X,'FROM THE UPPERMOST REACH OF ANY STREAM SEGMENT',
     1' LOCATED WITHIN EACH FARM')
      ENDIF
C
C3E4----SEMI-ROUTED RUNOFF-RETURNFLOW
      IF(LSTCHK(3)) THEN
        IF(ISRRFL.EQ.0) WRITE(IOUT,112)
      ENDIF
  112 FORMAT(3X,'NO SEMI-ROUTED RUNOFF-RETURNFLOW EXISTS')
      IF(ISRRFL.GT.0) THEN
      IF(IUNITSFR.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,113)
      ENDIF
  113 FORMAT(1X,'INPUT-ERROR: SEMI-ROUTED RUNOFF-RETURNFLOW',/,
     114X,'CANNOT OCCUR IF SFR1 PACKAGE IS NOT SPECIFIED IN NAME FILE.',
     2/,14X,'==> SELECT ISRRFL=0 OR SPECIFY SFR1 PACKAGE IN NAME FILE!')
      STOP
      ENDIF
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,114)
      ENDIF
  114 FORMAT(3X,'FARM RUNOFF RETURNED TO STREAM-NETWORK BY:',
     1/,5X,'SEMI-ROUTED SURFACE-WATER RETURNFLOW',/,5X,'(NON-ROUTED TO',
     2' & ROUTED FROM SPECIFIED REACH THAT IS OUTSIDE THE FARM)')
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(ISRRFL.EQ.1) WRITE(IOUT,4) ANAME(11)
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(ISRRFL.EQ.2) WRITE(IOUT,7) ANAME(11)
      ENDIF
C
C3E5----FULLY ROUTED SURFACE-WATER RUNOFF-RETURNFLOW
      IF(IRRFL.EQ.0) THEN
        IF(IUNITSFR.EQ.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,115)
          ENDIF
  115     FORMAT(3X,'NO ROUTED RUNOFF-RETURNFLOW EXISTS',/,3X,
     1    '(SFR PACKAGE NOT USED)')
        ELSEIF(IUNITSFR.GT.0) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,116)
          ENDIF
  116     FORMAT(3X,'NOT A VALID ENTRY IF SFR PACKAGE IS USED',/,3X,
     1    'PLEASE ENTER IRRFL=-1 OR IRRFL=1')
          STOP
        ENDIF
      ELSEIF(IRRFL.NE.0) THEN
        IF(IUNITSFR.EQ.0) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,117)
          ENDIF
  117     FORMAT(1X,'INPUT-ERROR: FULLY ROUTED RUNOFF-RETURNFLOW',/,14X,
     1    'CANNOT OCCUR IF SFR PACKAGE IS NOT SPECIFIED IN NAME FILE.',
     2    /,14X,'==> SPECIFY SFR PACKAGE IN NAME FILE!')
          STOP
        ELSEIF(IUNITSFR.GT.0) THEN
          IF(IRRFL.LT.-2.OR.IRRFL.GT.1)THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,*) "ROUTED SW-RETURNFLOW FLAG MUST BE -1 OR 1!"
            ENDIF
            STOP
          ENDIF
          IF(ISRRFL.EQ.0) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,118)
            ENDIF
  118       FORMAT(3X,'FARM RUNOFF RETURNED TO STREAM-NETWORK BY:',
     1      /,5X,'FULLY ROUTED SURFACE-WATER RETURNFLOW FROM ALL FARMS')
          ELSEIF(ISRRFL.GT.0) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,119)
            ENDIF
  119       FORMAT(3X,'FARM RUNOFF RETURNED TO STREAM-NETWORK BY:',/,5X,
     1      'FULLY ROUTED SURFACE-WATER RETURNFLOW',/,5X,'FROM FARMS',
     2      ' WITHOUT SPECIFIED SEMI-ROUTED RETURNFLOW LOCATIONS')      
          ENDIF
          IF(LSTCHK(3)) THEN
            IF(IRRFL.EQ.1) WRITE(IOUT,120)
          ENDIF
  120       FORMAT(5X,'TO ALL REACHES OF NON-DIVERSION STREAM SEGMENTS',
     1      ' LOCATED WITHIN EACH FARM')
          IF(LSTCHK(3)) THEN
            IF(IRRFL.EQ.-1) WRITE(IOUT,121)
          ENDIF
  121       FORMAT(5X,'TO ALL REACHES OF ANY STREAM SEGMENT',
     1      ' LOCATED WITHIN EACH FARM')     
        ENDIF
      ENDIF
C
C3E6----APPLIED SURFACE-WATER RIGHT
      IF(IALLOTSW.GT.0.AND.IUNITSFR.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,9401)
      ENDIF
 9401 FORMAT(1X,'INPUT-ERROR: SURFACE-WATER DELIVERY CONSTRAINTS',/,
     114X,'CANNOT OCCUR IF SFR1 PACKAGE IS NOT SPECIFIED IN NAME FILE.',
     2/,14X,'==> SELECT IALLOTSW=0 OR SPECIFY SFR PACKAGE IN NAME FILE')
      STOP
      ENDIF
      IF(IALLOTSW.GT.0.AND.IRDFL.EQ.0.AND.ISRDFL.EQ.0) THEN
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,9402)
      ENDIF
 9402 FORMAT(1X,'INPUT-ERROR: SURFACE-WATER DELIVERY CONSTRAINTS',/,
     114X,'CANNOT OCCUR IF (SEMI-)ROUTED DELIVERIES FROM CANALS TO FARMS
     2DO NOT EXIST.',/,14X,
     3'==> SELECT IALLOTSW=0 OR SELECT IRDFL=0 OR ISRDFL=1, OR=2')  
      STOP
      ENDIF
      IF(LSTCHK(3)) THEN
        IF(IALLOTSW.EQ.1) WRITE(IOUT,950)
      ENDIF
  950 FORMAT(3X,'EQUAL APPROPRIATION SW ALLOTMENT HEIGHT SELECTED')
      IF(LSTCHK(3)) THEN
        IF(IALLOTSW.GE.2) WRITE(IOUT,960) PCLOSE
      ENDIF
  960 FORMAT(3X,'SW PRIOR APPROPRIATION SELECTED; FLOW RESIDUAL=',E11.4)
      IF(LSTCHK(3)) THEN
        IF(IALLOTSW.EQ.2) WRITE(IOUT,970)
      ENDIF
  970 FORMAT(3X,'SW WATER RIGHTS CALLS ARE READ FOR EACH STRESS PERIOD')
C
C
C3F-----PRINT FLAGS AND ACCORDING SETTINGS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,'(/,1X,"PRINT SETTINGS:")')
      ENDIF
C
C3F1----LOOK FOR "COMPACT BUDGET" IN OUTPUT CONTROL
      ICB=0
      IF(IBDOPT.EQ.2) ICB=1
C
C3F2----PRINT SETTINGS FOR FARM WELL FLOW RATES
C
C3F2A---TO LIST FILE
      IF(LSTCHK(3)) THEN
        IF(IFWLCB.LT.0) WRITE(IOUT,61)
      ENDIF
   61 FORMAT(3X,'FARM WELL FLOW RATES:',/,5X,
     1'A LIST (FARM-WELL ID, FARM ID, LAY, ROW, COL, RATE) IS PRINTED ',
     2'TO LIST FILE',/,5X,'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTROL'
     3,/,5X,'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
C
C3F2B---TO ASCII FILE
      IF(LSTCHK(3)) THEN
        IF(IFWLCB.EQ.1) WRITE(IOUT,62)
      ENDIF
   62 FORMAT(3X,'FARM WELL FLOW RATES:',/,5X,
     1'A LIST (FARM-WELL ID, FARM ID, LAY, ROW, COL, RATE) IS SAVED ',
     2/,5X,'ON ASCII FILE "FWELLS.OUT" FOR ALL TIME STEPS')
C
C3F2C---TO BINARY FILE
      IF(LSTCHK(3)) THEN
        IF(IFWLCB.GT.1.AND.ICB.EQ.1) WRITE(IOUT,63) IFWLCB
      ENDIF
   63 FORMAT(3X,'FARM WELL FLOW RATES:',/,5X,     
     1'A LIST (NODE, RATE) WILL BE SAVED AS BINARY FILE ON UNIT',I5,/,5X
     2,'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     3'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
      IF(LSTCHK(3)) THEN
        IF(IFWLCB.GT.1.AND.ICB.EQ.0) WRITE(IOUT,64) IFWLCB
      ENDIF
   64 FORMAT(3X,'FARM WELL FLOW RATES:',/,5X,     
     1'A CELL-BY-CELL 2D-ARRAY WILL BE SAVED AS BINARY FILE ON UNIT',
     2 I4,/,5X,'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     3'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
C
C3F3----PRINT SETTINGS FOR FARM NET RECHARGE CELL-BY-CELL FLOW RATES, AND FOR
C       CUMULATIVE FARM-BY-FARM NET RECHARGE
C
C3F3A---TO LIST FILE
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.LT.0) WRITE(IOUT,71)
      ENDIF
   71 FORMAT(3X,'FARM NET RECHARGE FLOW RATES:',/,5X,
     1'A CELL-BY-CELL 2D-ARRAY IS PRINTED TO LIST FILE',/,5X,
     2'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTROL'
     3,/,5X,'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
C
C3F3B---TO ASCII FILE
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.EQ.1) WRITE(IOUT,72)
      ENDIF
   72 FORMAT(3X,'FARM NET RECHARGE FLOW RATES:',/,5X,
     1'A CELL-BY-CELL 2D-ARRAY IS SAVED ON ASCII FILE "FNRCH_ARRAY.OUT"'
     2,/,5X,'FOR ALL TIME STEPS')
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.EQ.2) WRITE(IOUT,73)
      ENDIF
   73 FORMAT(3X,'CUMULATIVE FARM NET RECHARGE FLOW RATES FOR EACH FARM:'
     1,/,5X,'A LIST (PER, TSTP, TIME, FARM ID, RATE) WILL BE SAVED AS',/
     2,5X,'ASCII FILE "FNRCH_LIST.OUT" FOR ALL TIME STPES')
C
C3F3C---TO BINARY FILE
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.EQ.3) WRITE(IOUT,74)
      ENDIF
   74 FORMAT(3X,'CUMULATIVE FARM NET RECHARGE FLOW RATES FOR EACH FARM:'
     1,/,5X,'A LIST (PER, TSTP, TIME, FARM ID, RATE) WILL BE SAVED AS',/
     2,5X,'BINARY FILE "FNRCH_LIST_BIN.OUT" FOR ALL TIME STEPS')
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.GT.3.AND.ICB.EQ.0) WRITE(IOUT,75) IFNRCB
      ENDIF
   75 FORMAT(3X,'CUMULATIVE FARM NET RECHARGE FLOW RATES FOR EACH FARM:'
     1,/,5X,'A LIST (FARM ID, RATE) WILL BE SAVED AS BINARY FILE ON UNIT
     2',I4,/,5X,'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     3'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.GT.3.AND.ICB.EQ.1.AND.NLAY.EQ.1) WRITE(IOUT,76) IFNRCB
      ENDIF
   76 FORMAT(3X,'FARM NET RECHARGE FLOW RATES:',/,5X,     
     1'A CELL-BY-CELL 2D-ARRAY WILL BE SAVED AS BINARY FILE ON UNIT',
     2 I4,/,5X,'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     3'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     4' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
      IF(LSTCHK(3)) THEN
        IF(IFNRCB.GT.3.AND.ICB.EQ.1.AND.NLAY.GT.1) WRITE(IOUT,77) IFNRCB
      ENDIF
   77 FORMAT(3X,'FARM NET RECHARGE FLOW RATES:',/,5X,     
     1'A 2D INTEGER-ARRAY OF EACH CELLS UPPERMOST ACTIVE LAYER, AND',/,
     2 5X,'A 2D REAL-ARRAY OF EACH CELLS NET RECHARGE RATE ',/,5X,
     3'WILL BE SAVED AS BINARY FILE ON UNIT',I5,
     4' FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     5'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     6' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)')
C
C3F4----PRINT SETTINGS FOR FARM SUPPLY AND DEMAND TABLES
C
C3F4A---TO LIST FILE
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.EQ.-3) WRITE(IOUT,80)
      ENDIF
   80 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     1'A LIST(A) OF CURRENT DEMAND & SUPPLY FLOW RATES',/,8X, 
     2'WILL BE PRINTED TO THE LIST FILE AT EACH ITERATION:',/,8X,
     3'LIST(A) = (FID, OFE, TFDR, NR-SWD, R-SWD, QREQ);',/,5X,
     4'A LIST(B) OF FINAL DEMAND AND SUPPLY FLOW RATES',/,8X,
     5'WILL BE PRINTED TO THE LIST FILE FOR EACH TIME STEP:',/,8X,
     6'LIST(B) = (FID, OFE, TFDR, NR-SWD, R-SWD, QREQ, Q, [COMMENTS])')
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.EQ.-2) WRITE(IOUT,81)
      ENDIF
   81 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     4'A LIST OF FINAL DEMAND AND SUPPLY FLOW RATES',/,5X,
     5'WILL BE PRINTED TO THE LIST FILE FOR EACH TIME STEP:',/,5X,
     6'LIST = (FID, OFE, TFDR, NR-SWD, R-SWD, QREQ, Q, [COMMENTS])')
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.EQ.-1) WRITE(IOUT,82)
      ENDIF
   82 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     1'A LIST OF FINAL DEMAND AND SUPPLY FLOW RATES ',
     2'WILL BE PRINTED TO THE LIST FILE',/,5X,
     3'FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     4'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     5' OR ICBCFL IS NOT 0 (USING NUMERIC CODES)',/,5X,
     6'LIST = (FID, OFE, TFDR, NR-SWD, R-SWD, QREQ, Q, [COMMENTS])')
C
C3F4B---TO ASCII FILE
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.EQ.1) WRITE(IOUT,83)
      ENDIF
   83 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     1'A LIST OF INITIAL DEMAND & SUPPLY FLOW RATES, AND OF',/,5X,
     2'FINAL DEMAND AND SUPPLY FLOW RATES AFTER ',
     3'THE APPLICATION OF A DEFICIENCY SCENARIO',/,5X,
     4'WILL BE SAVED ON ASCII-FILE "FDS.OUT" FOR ALL TIME STEPS.',2X,
     5'LIST =',/,5X,'(PER,TSTP,TIME,FID,OFE,',/,6X,'TFDR-INI,NR-SWD-INI,
     6','R-SWD-INI,QREQ,TFDR-FIN,NR-SWD-FIN,R-SWD-FIN,QREQ,Q,DEF-FLAG)',
     7/,5X,'(<-- BEFORE DEFICIENCY SCENARIO -->',
     8' <--       AFTER DEFICIENCY SCENARIO       -->)')
C
C3F4C---TO BINARY FILE
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.GT.1.AND.ICB.EQ.1) WRITE(IOUT,84) ISDPFL
      ENDIF
   84 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     1'A LIST OF INITIAL DEMAND & SUPPLY FLOW RATES, AND OF',/,5X,
     2'FINAL DEMAND AND SUPPLY FLOW RATES AFTER ',
     3'THE APPLICATION OF A DEFICIENCY SCENARIO',/,5X,
     4'WILL BE SAVED AS BINARY FILE ON UNIT',I5' FOR TIME STEPS, ',
     5'FOR WHICH IN OUTPUT CONTOL',/,5X,
     6'"SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     7' OR ICBCFL IS NOT 0 (USING NUMERIC CODES).  ',
     8'LIST =',/,5X,'(PER,TSTP,TIME,FID,OFE,',/,6X,'TFDR-INI,NR-SWD-INI,
     9','R-SWD-INI,QREQ,TFDR-FIN,NR-SWD-FIN,R-SWD-FIN,QREQ,Q,DEF-FLAG)',
     +/,5X,'(<-- BEFORE DEFICIENCY SCENARIO -->',
     1' <--       AFTER DEFICIENCY SCENARIO       -->)')
      IF(LSTCHK(3)) THEN
        IF(ISDPFL.GT.1.AND.ICB.EQ.0) WRITE(IOUT,85) ISDPFL
      ENDIF
   85 FORMAT(3X,'FARM DEMAND & SUPPLY FLOW RATES:',/,5X,
     1'A LIST OF INITIAL DEMAND & SUPPLY FLOW RATES, AND OF',/,5X,
     2'FINAL DEMAND AND SUPPLY FLOW RATES AFTER ',
     3'THE APPLICATION OF A DEFICIENCY SCENARIO',/,5X,
     4'WILL BE SAVED AS BINARY FILE ON UNIT',I5' FOR ALL TIME STEPS.  ',
     5'LIST =',/,5X,'(PER,TSTP,TIME,FID,OFE,',/,6X,'TFDR-INI,NR-SWD-INI,
     6','R-SWD-INI,QREQ,TFDR-FIN,NR-SWD-FIN,R-SWD-FIN,QREQ,Q,DEF-FLAG)',
     7/,5X,'(<-- BEFORE DEFICIENCY SCENARIO -->',
     8' <--       AFTER DEFICIENCY SCENARIO       -->)')
C
C3F5----PRINT SETTINGS FOR FARM BUGDGET TABLES
C
C3F5A---TO LIST FILE (NOT NEEDED SO FAR)
C
C3F5B---TO ASCII FILE
      IF(IFBPFL.EQ.1) THEN
      FBTYPE="COMPACT "
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,201) FBTYPE,"FB_COMPACT.OUT"
      ENDIF
  201 FORMAT(3X,A8,' FARM BUDGET:',/,5X,        
     1'A LIST OF VOLUMETRIC FLOW RATES FOR ANY TIME STEP [L^3/T],',
     2' AND OF CUMULATIVE VOLUMES [L^3] ',/,5X,
     4'WILL BE SAVED ON ASCII-FILE "',A14,'" FOR ALL TIME STEPS.')
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,202)
      ENDIF
  202 FORMAT(6X,'LIST = (PER,TSTP,TIME,FID;',/,6X,'RATES INTO FARM: ',
     1'Q-p-in, Q-sw-in, Q-gw-in, Q-ext-in, Q-tot-in,',/,6X,
     2'RATES OUT OF FARM: ',
     3'Q-et-out,Q-ineff-out, Q-sw-out, Q-gw-out, Q-tot-out,',/,6X,
     4'RATE BUDGET ERROR: Q-in-minus-out, Q-Discrepancy [%];',/,6X,
     1'CUMULATIVE VOLUMES INTO FARM: ',
     1'V-p-in, V-sw-in, V-gw-in, V-ext-in, V-tot-in,',/,6X,
     1'CUMULATIVE VOLUMUES OUT OF FARM: ',
     1'V-et-out,V-ineff-out, V-sw-out, V-gw-out, V-tot-out,',/,6X,
     1'VOLUMETRIC BUDGET ERROR: V-in-minus-out, V-Discrepancy [%])')
      ENDIF
c
      IF(IFBPFL.EQ.2) THEN
      FBTYPE="DETAILED"
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,201) FBTYPE,"FB_DETAILS.OUT"
      ENDIF
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,203)
      ENDIF
  203 FORMAT(6X,'LIST = (PER,TSTP,TIME,FID;',/,6X,'RATES INTO FARM: ',
     1'Q-p-in, Q-nrd-in, Q-srd-in, Q-rd-in, Q-wells-in, Q-egw-in, ',
     2'Q-tgw-in, Q-ext-in, Q-tot-in,',/,6X,'RATES OUT OF FARM: ',
     3'Q-ei-out, Q-ep-out, Q-egw-out, Q-ti-out, Q-tp-out, Q-tgw-out ',
     4'Q-run-out, Q-dp-out, Q-nrd-out, Q-srd-out, Q-rd-out, ',
     5'Q-wells-out, Q-tot-out,',/,6X,
     6'RATE BUDGET ERROR: Q-in-minus-out, Q-Discrepancy [%];',/,6X,
     7'CUMULATIVE VOLUMES INTO FARM: ',
     8'V-p-in, V-nrd-in, V-srd-in, V-rd-in, V-wells-in, V-egw-in, ',
     9'V-tgw-in, V-ext-in, V-tot-in,',/,6X,
     &'CUMULATIVE VOLUMES OUT OF FARM: ',
     &'V-ei-out, V-ep-out, V-egw-out, V-ti-out, V-tp-out, V-tgw-out ',
     &'V-run-out, V-dp-out, V-nrd-out, V-srd-out, V-rd-out, ',
     &'V-wells-out, V-tot-out,',/,6X,
     &'VOLUMETRIC BUDGET ERROR: Q-in-minus-out, Q-Discrepancy [%])')
      ENDIF
C
C3F5C---TO BINARY FILE
      IF(IFBPFL.GT.2) THEN
C
      FBTYPE="COMPACT "
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) FBTYPE="DETAILED"
C
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,204) FBTYPE, IFBPFL
      ENDIF
  204 FORMAT(3X,A8,' FARM BUDGET:',/,5X,
     1'A LIST OF VOLUMETRIC FLOW RATES [L^3/T], AND OF ',
     2'CUMULATIVE VOLUMES [L^3] ',/,5X,
     3'WILL BE SAVED AS BINARY FILE ON UNIT',I5)
C
      IF(LSTCHK(3)) THEN
        IF(ICB.EQ.1) WRITE(IOUT,205)
      ENDIF
  205 FORMAT(5X,' FOR TIME STEPS, FOR WHICH IN OUTPUT CONTOL',/,5X,
     1' "SAVE BUDGET" IS SPECIFIED (USING WORDS),',
     2' OR ICBCFL IS NOT 0 (USING NUMERIC CODES).')
      IF(LSTCHK(3)) THEN
        IF(ICB.EQ.0) WRITE(IOUT,206)
      ENDIF
  206 FORMAT(5X,' FOR ALL TIME STEPS.')
C
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,203)
      ENDIF
      ELSE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,202)
      ENDIF
      ENDIF
      ENDIF
C
C3F6----OPTIONAL PRINT SETTINGS FOR ROUTING INFORMATION
C
C3F6A---ROUTING INFORMATION WRITE TO LIST FILE OR ROUT.OUT
      IF(IUNITSFR.GT.0) THEN
       IF(LSTCHK(3)) THEN                                               !seb CHANGED FROM A SERIES OF IF STATMENTS TO A SINGLE SELECT CASE
        SELECT CASE (IRTPFL)
        CASE(-2)
          WRITE(IOUT,86) 'LISTING FILE FOR THE FIRST STRESS PERIOD ONLY'
        CASE(-1)
          WRITE(IOUT,87) 'LISTING FILE FOR EVERY STRESS PERIOD'
        CASE(0)
          WRITE(IOUT,88) 'NOT WRITTEN '
        CASE(1)
          WRITE(IOUT,89) 'ASCII FILE "ROUT.OUT" FOR EVERY STRESS PERIOD'
        CASE(2)
          WRITE(IOUT,891)'ASCII FILE "ROUT.OUT" FOR FIRST STRESS ',
     +                   'PERIOD ONLY'
        END SELECT
   86   FORMAT(3X,'ROUTING INFOMATION:',/,5X,'WRITTEN TO ',A)
   87   FORMAT(3X,'ROUTING INFOMATION:',/,5X,'WRITTEN TO ',A) 
   88   FORMAT(3X,'ROUTING INFOMATION:',/,5X,A)
   89   FORMAT(3X,'ROUTING INFOMATION:',/,5X,'WRITTEN TO ',A)
  891   FORMAT(3X,'ROUTING INFOMATION:',/,5X,'WRITTEN TO ',2A)
       END IF
      END IF
C
C        IF(IRTPFL.EQ.-2) WRITE(IOUT,86) 'ROUTING INFOMATION:' 
C      ENDIF
C   86 FORMAT(3X,A19,/,5X,'WRITTEN TO LISTING FILE FOR THE ',
C     1'FIRST STRESS PERIOD ONLY')
C      IF(LSTCHK(3)) THEN
C        IF(IRTPFL.EQ.-1) WRITE(IOUT,87) 'ROUTING INFOMATION:'
C      ENDIF
C   87 FORMAT(3X,A19,/,5X,'WRITTEN TO LISTING FILE FOR EVERY ',
C     1'STRESS PERIOD') 
C      IF(LSTCHK(3)) THEN
C        IF(IRTPFL.EQ.0) WRITE(IOUT,88) 'ROUTING INFOMATION:'
C      ENDIF
C   88 FORMAT(3X,A19,/,5X,'NOT WRITTEN ')
C      IF(LSTCHK(3)) THEN
C        IF(IRTPFL.EQ.1) WRITE(IOUT,89) 'ROUTING INFOMATION:'
C      ENDIF
C   89 FORMAT(3X,A19,/,5X,'WRITTEN TO ASCII FILE "ROUT.OUT" ',
C     1'FOR EVERY STRESS PERIOD')
C      IF(LSTCHK(3)) THEN
C        IF(IRTPFL.EQ.2) WRITE(IOUT,891) 'ROUTING INFOMATION:'
C      ENDIF
C  891 FORMAT(3X,A19,/,5X,'WRITTEN TO ASCII FILE "ROUT.OUT" ',
C     1'FOR FIRST STRESS PERIOD ONLY')    
C      ENDIF
C
C3F7----OPTIONAL PRINT SETTINGS FOR ACREAGE OPTIMIZATION
C
C3F7A---TO LIST FILE
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.-4) WRITE(IOUT,90)
      ENDIF
   90 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'A TABLEAU MATRIX WILL BE PRINTED TO THE LIST FILE',/,5X,
     2'FOR ITERATIONS, DURING WHICH OPTIMIZATION OCCURS.')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.-3) WRITE(IOUT,91)
      ENDIF
   91 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'ORIGINAL & OPTIMIZED FLOW RATES OF RESOURCE CONSTRAINTS; AND',/,
     25X,'A LIST OF FRACTIONS OF OPTIMIZED CELL AREAS WILL BE PRINTED',/
     3,5X,'TO THE LIST FILE',/,5X,
     4'FOR ANY FARM & ITERATION THAT ARE SUBJECT TO OPTIMIZATION',/,5X,
     5'LIST= (               A-tot-opt/ A-gw-opt/  A-sw-opt/  A-nr-opt/'
     6,/,5X,
     7'       Row  Col  CID  A-tot-max  A-tot-opt  A-tot-opt  A-tot-opt)
     8')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.-2) WRITE(IOUT,92)
      ENDIF
   92 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'ORIGINAL & OPTIMIZED FLOW RATES OF RESOURCE CONSTRAINTS',/
     2,5X,'WILL BE PRINTED TO THE LIST FILE',/,5X,
     3'FOR ANY FARM & ITERATION THAT ARE SUBJECT TO OPTIMIZATION.')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.-1) WRITE(IOUT,93)
      ENDIF
   93 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'A 2D-ARRAY OF FRACTIONS OF ACTIVE CELL ACREAGE WILL BE PRINTED',/
     2,5X,'TO THE LIST FILE FOR ALL TIME STEPS.')
C
C3F7B---TO ASCII FILE
      IF(LSTCHK(3)) THEN
      IF(IOPFL.EQ.4)   WRITE(IOUT,94)
      ENDIF
   94 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'A TABLEAU MATRIX IS SAVED ON ASCII FILE "ACR_OPT.OUT"',/,5X,
     2'FOR ITERATIONS, DURING WHICH OPTIMIZATION OCCURS.')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.3) WRITE(IOUT,95)
      ENDIF
   95 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'ORIGINAL & OPTIMIZED FLOW RATES OF RESOURCE CONSTRAINTS; AND',/,
     25X,'A LIST OF FRACTIONS OF OPTIMIZED CELL AREAS ',/
     3,5X,'IS SAVED ON ASCII FILE "ACR_OPT.OUT"',/,5X,
     4'FOR ANY FARM & ITERATION THAT ARE SUBJECT TO OPTIMIZATION',/,5X,
     5'LIST= (               A-tot-opt/ A-gw-opt/  A-sw-opt/  A-nr-opt/'
     6,/,5X,
     7'       Row  Col  CID  A-tot-max  A-tot-opt  A-tot-opt  A-tot-opt)
     8')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.2) WRITE(IOUT,96)
      ENDIF
   96 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'ORIGINAL & OPTIMIZED FLOW RATES OF RESOURCE CONSTRAINTS',/
     2,5X,'ARE SAVED ON ASCII FILE "ACR_OPT.OUT"',/,5X,
     3'FOR ANY FARM & ITERATION THAT ARE SUBJECT TO OPTIMIZATION.')
C
      IF(LSTCHK(3)) THEN
        IF(IOPFL.EQ.1) WRITE(IOUT,97)
      ENDIF
   97 FORMAT(3X,'ACREAGE OPTIMIZATION:',/,5X,
     1'A CELL-BY-CELL 2D-ARRAY OF FRACTIONS OF ACTIVE CELL ACREAGE',/
     2,5X,'IS SAVED ON ASCII FILE "ACR_OPT.OUT" FOR ALL TIME STEPS.')
C
C3F8----OPTIONAL PRINT SETTINGS FOR PRIOR APPROPRIATION
C
C3F8A---TO LIST FILE
C
      IF(LSTCHK(3)) THEN
        IF(IPAPFL.EQ.-1) WRITE(IOUT,40)
      ENDIF
   40 FORMAT(3X,'PRIOR APPROPRATION:',/,5X,
     1'A BUDGET AT THE POINT OF A CANAL-DIVERSION FROM THE RIVER, AND',/
     2,5X,'A BUDGET AT THE POINT OF A FARM-DIVERSION FROM THE CANAL',
     3' WILL BE PRINTED TO THE LIST FILE FOR ALL ITERATIONS.')
C
C3F8B---TO ASCII FILE
C
      IF(LSTCHK(3)) THEN
        IF(IPAPFL.EQ.1) WRITE(IOUT,41)
      ENDIF
   41 FORMAT(3X,'PRIOR APPROPRATION:',/,5X,
     1'A BUDGET AT THE POINT OF A CANAL-DIVERSION FROM THE RIVER, AND',/
     2,5X,'A BUDGET AT THE POINT OF A FARM-DIVERSION FROM THE CANAL',
     3' WILL BE SAVED ON ASCII FILE "PRIOR.OUT" FOR ALL ITERATIONS.')
C

C3G-----ADDITIONAL FLAGS
      IF(IFWLAL.EQ.1) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,12)
        ENDIF
   12   FORMAT(/,1X,'MEMORY IS ALLOCATED FOR CELL-BY-CELL BUDGET TERMS')
      ENDIF
      IF(NAUX.GT.0.AND.NAUX.LT.5) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,*)
        ENDIF
        DO N=1,NAUX
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,13) FWLAUX(N)
           ENDIF
   13      FORMAT(1X,'AUXILIARY FARM WELL PARAMETER: ',A)
        ENDDO
      ENDIF
      IF(IPFWEL.EQ.0) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,131)
        ENDIF
  131   FORMAT(/,1X,'LISTS OF FARM WELL ATTRIBUTES WILL NOT BE PRINTED')
      ENDIF
      IF(IWELLFLD.EQ.1) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,132)
        ENDIF
  132   FORMAT(/,1X,'WELLFIELDS SUPPLY NON-ROUTED DELIVERIES TO FARMS') 
      ENDIF
      IF(QBD.EQ.1) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,133)
        ENDIF
  133   FORMAT(/,1X,'RECACULATE FLOWRATES AT END OF TIME-STEP LOOP')
      END IF
      IF(MCLOSE.EQ.1) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,134) QCLOSE,HPCT*100,RPCT*100
        ENDIF
  134   FORMAT(/,1X,'CRITERION FOR ACTUAL MNW PUMPING RATE TO CONVERGE',
     1  /,'   TO FMP PUMPING REQUIREMENT (QCLOSE) = ',E11.5,/,3X
     2  ,'REDUCTION OF HEAD CHANGE CLOSURE CRITERION BY ',F5.1,' %',/,3X
     3  ,'REDUCTION OF RESIDUAL CHANGE CLOSURE CRITERION BY ',F5.1,' %')
      END IF
C
C4===== ALLOCATE SPACE FOR REAL AND DOUBLE PRECISION VARIABLES AND INITIALIZE ===========================
C
C4A-----CALCULATE THE AMOUNT OF SPACE FOR THE LIST OF FARM WELLS
C       (6 values read for each cell: ROW, COL.,LAYER, FARM-WELL ID, FARM ID, MAX.CAPAC. +
C        1 more for Qrate calculation)      
      NFWLVL=7+NAUX+IFWLAL
C-------Allocate space in the double precision array for the Farm Well list and initialize
      IFWLPB=MXACTFW+1
      MXFWEL=MXACTFW+MXPFW
      ALLOCATE(FWELL(NFWLVL,MXFWEL))
      ALLOCATE(AUXV(MAXAUX,MXFWEL))
      FWELL=0.D0
      AUXV=0
C     (The FMP1 rule 'Deactivating the FARM PROCESS because MXACTFW=0' was dropped in FMP3)
C
      IF(IUNITMNW2.GT.0)THEN                                            !seb ADD MNW2NAM FOR MNW2 LINK
        ALLOCATE(MNW2NAM(MXFWEL),MNW2LOC(MXACTFW))                      !FIRST MXACTFW DIM IS SCRATCH SPACE USED TO DEFINE NAMES OF MWN2WELLS USED FOR CURRENT STRESS PERIOD LAST MXACTFW DIM CONTAINS A NONCHANGING RECORD OF PARAMETER DEFINED MNW2 LINKED WELLS (SAME DESIGN AS HFB PARAMETERS)
      ELSE
        ALLOCATE(MNW2NAM(1),MNW2LOC(1))                                 !VARIABLE NOT USED, ALLOCATE TO PREVENT DEALLOCATE ERROR
      END IF
      MNW2LOC=0
C11 
C4B-----Allocate space for double precision arrays for farm well specific lists 
c       (QMXF,QREQ,QAVF,QDEF,QEXC)  and initialize
      ALLOCATE(QMAXF(NFARMS),QREQ(NFARMS),QREQOLD(NFARMS),              !DP
     1  QMAXFW(NFARMS),QFRACF(NFARMS))                                  !added QMAXFW to hold original QMAF values and QFRACF to holde fraction QMAXFW/ALLOTGW
      QMAXF=0D0
      QREQ=0D0
      QREQOLD=0D0
      QMAXFW=0D0
      QFRACF=1D0
      ALLOCATE(QAVF(NFARMS),QDEF(NFARMS),QEXC(NFARMS))
      QAVF=0.D0
      QDEF=0.D0
      QEXC=0.D0
C
ccrth
C4B2-----Allocate space for double precision arrays for groundwater allotments for farm specific lists
      IF(IALLOTGW.GT.0)THEN
       ALLOCATE(ALLOTGW(2,NFARMS))
       ALLOTGW=0D0
      ELSE
       ALLOCATE(ALLOTGW(2,1))
       ALLOTGW=0D0
      ENDIF
ccrth
C4C-----Allocate space for the double precision array for the Ground Surface Elevation array (GSURF) and initialize
      ALLOCATE(GSURF(NCOL,NROW))
C      ALLOCATE(GSURFR(NCOL,NROW))
c      GSURFR=0.
      GSURF=0.D0
C
C4D-----Allocate space for the double precision Farm Efficiency list and array (EFF,EF2D) and initialize
      ALLOCATE(EFF((NCROPS+3),NFARMS),EF2D(NCOL,NROW))
      EFF=0D0
      EF2D=0D0
C
C4E-----Allocate space for the double precision Consumptive use list and array (CU,CU2D) and initialize
      ALLOCATE(CU(3,NCROPS),CU2D(NCOL,NROW))
      CU=0.D0
      CU2D=0.D0
C
C4F-----Allocate space for the double precision Fraction of In-efficient Losses to SW-Runoff (FIESW)
c       and for double precision Rise-Run Arrays (RISERUN) and initialize
      IF(IIESWFL.GT.0) THEN
        ALLOCATE(FIESW(3,NCROPS),RISERUN(1,1))
        FIESW=0D0
      ELSEIF(IIESWFL.EQ.0) THEN
        ALLOCATE(FIESW(1,1),RISERUN(NCOL,NROW))
        RISERUN=0D0        
      ELSEIF(IIESWFL.EQ.-1.AND.ILGR.GT.0.AND.IGRID.GT.1) THEN
        IF(FMPDAT(1)%IIESWFL.GT.0) THEN
          ALLOCATE(FIESW(3,NCROPS),RISERUN(1,1))
          FIESW=0D0
        ELSEIF(FMPDAT(1)%IIESWFL.EQ.0) THEN
          ALLOCATE(FIESW(1,1),RISERUN(NCOL,NROW))
          RISERUN=0D0        
        ENDIF
      ENDIF
C
C4G-----Allocate space for the double precision array for 
c       fraction of transpiration and evaporation on Crop Coefficient (FTE) and initialize
      ALLOCATE(FTE(4,NCROPS))
      FTE=0D0
C
C4H-----Allocate space for the double precision array for the Total Delivery Requirement array (TDR, cell-by-cell) 
c       and the Total Farm Delivery Requirement lists (TFDR,TFDROLD, farm-by-farm) and initialize 
      ALLOCATE(TDR(NCOL,NROW),TFDR(NFARMS),TFDROLD(NFARMS))
      TDR=0D0
      TFDR=0D0
      TFDROLD=0D0
C
C4I-----Allocate space for double precision arrays for list & array
c       of soil-type specific parameters (SOIL2D, SOIL) and initialize 
c       (soil-id; capillary fringe; 5 coefficients to define analytical solution)    
      ALLOCATE(SOIL(7,NSOILS),SOIL2D(6,NCOL,NROW))
      SOIL=0D0
      SOIL2D=0D0
C
C4J-----Allocate space for the double precision array for the critical pressure heads of a
C       crop specific Stress Response Function (PSI) and initialize
      IF(ICCFL.EQ.1.OR.ICCFL.EQ.3) THEN
        ALLOCATE(PSI(5,NCROPS))
        PSI=0D0
      ELSEIF(ICCFL.EQ.2.OR.ICCFL.EQ.4) THEN
        ALLOCATE(PSI(1,1))
      ELSEIF(ICCFL.EQ.-1.AND.ILGR.GT.0.AND.IGRID.GT.1) THEN
        IF(FMPDAT(1)%ICCFL.EQ.1.OR.FMPDAT(1)%ICCFL.EQ.3) THEN
          ALLOCATE(PSI(5,NCROPS))
          PSI=0D0
        ELSEIF(FMPDAT(1)%ICCFL.EQ.2.OR.FMPDAT(1)%ICCFL.EQ.4) THEN
          ALLOCATE(PSI(1,1))
        ENDIF
      ENDIF
      IF(IUNITUZF.EQ.0.AND.ICCFL.EQ.3)THEN
          WRITE(LINE,'(3(/A))') 
     +    'FMP ERROR: ICCFL = 3 BUT UZF IS NOT IN USE. ',
     +    'ICCFL = 3 USES THE UZF PACKAGE FOR INFILTRATION ',
     +    'PROGRAM WILL NOW TERMINATE'
          WRITE(IOUT,'(A)')LINE
          CALL USTOP(TRIM(LINE))
      END IF
C
C4K-----Allocate space the double precision arrays for Root Zone Depth list and array (ROOT, RT2D) and initialize
      ALLOCATE(ROOT(2,NCROPS),RT2D(NCOL,NROW))
      ROOT=0.D0
      RT2D=0.D0
C
C4L-----Allocate space for the double precision arrays for Precipitation, 
C       Transpiration, Evaporation, Deep Percolation, and Farm Net Recharge arrays, and initialize
      ALLOCATE(PFLX(NCOL,NROW),PFLR(NCOL,NROW),
     1 TPPOT(NCOL,NROW),EPPOT(NCOL,NROW),EGW(NCOL,NROW),TGW(NCOL,NROW))
      PFLX=0.D0
      PFLR=0.D0
      TPPOT=0.D0
      EPPOT=0.D0
      EGW=0.D0
      TGW=0.D0
      ALLOCATE(TGWO(NCOL,NROW),TGWA(NCOL,NROW),EGWA(NCOL,NROW),
     1TTOT(NCOL,NROW),ETOT(NCOL,NROW),DPERC(NCOL,NROW),FNRCH(NCOL,NROW))!DP
      TGWO=0D0
      TGWA=0D0
      EGWA=0D0
      TTOT=0D0
      ETOT=0D0
      DPERC=0D0
      FNRCH=0D0
      IF(ICUFL.GT.1) THEN
        ALLOCATE(ETR(1,1))
      ELSEIF(ICUFL.EQ.1.OR.ICUFL.EQ.-1) THEN
        ALLOCATE(ETR(NCOL,NROW))
        ETR=0.D0
      ELSEIF(ICUFL.EQ.-2.AND.ILGR.GT.0.AND.IGRID.GT.1) THEN
        IF(FMPDAT(1)%ICUFL.GT.1) THEN
          ALLOCATE(ETR(1,1))
        ELSEIF(FMPDAT(1)%ICUFL.EQ.1.OR.FMPDAT(1)%ICUFL.EQ.-1) THEN
          ALLOCATE(ETR(NCOL,NROW))
          ETR=0.D0
        ENDIF
      ENDIF
C
C4M-----Allocate space for the double precision array for Surface-water Runoff per cell
C       (allocated even if no SFR7 package = non-routed SW Runoff leaving the domain)
      ALLOCATE(SWRUN(NCOL,NROW))
      SWRUN=0.D0
C
C4N1----Allocate only one array element if Acreage-Optimization is not used!
      IF(IDEFFL.LE.0) THEN
C.......for the Crop Benefit & the Water Cost Coefficient lists
        ALLOCATE(CROPBEN(1,1),WATERCOST(1,1))                           !DP
C.......for variables HLIFT (effective headlift) and ELHL (elev. related to effective headlift)
        ALLOCATE(HLIFT(1),ELHL(1))
C.......for variable DWE (effective distance to wells)
        ALLOCATE(DWE(1,1))
c.......[DO NOT] Allocate space [HERE] for the double precision array for DNR (distance to nearest reach)
c       and ELNR (elev. of nearest reach)
c       (moved into SFR if statement: whether or not DNR and ELNR are allocated fully depends on SFR link)
C.......for Acreage-Optimization list (LCOPT) and for percentages of
C       Groundwater- / Surface-water- / Non-Routed-SW- Irrigation for Acreage-reduced Cells
        ALLOCATE(OPT(1,1))
        ALLOCATE(REDPCT(1,1),GWREDPCT(1,1),SWREDPCT(1,1),NRREDPCT(1,1))
      ELSEIF(IDEFFL.GT.0) THEN
C
C4N2----Allocate space for double precision arrays if Acreage-Optimization is used!
c
c.......Allocate space for the double precision array for the Crop Benefit and Water Cost Coefficienc lists and initialize
        ALLOCATE(CROPBEN(4,NCROPS),WATERCOST(9,NFARMS))
        CROPBEN=0D0
        WATERCOST=0D0
c.......Allocate space for the double precision array for HLIFT (effective headlift) and initialize
C       and ELHL (elev. related to effective headlift)
        ALLOCATE(HLIFT(NFARMS),ELHL(NFARMS))
        HLIFT=0.D0
        ELHL=0.D0
c.......Allocate space for the double precision array for DWE (effective distance to wells) and initialize
        ALLOCATE(DWE(NCOL,NROW))
        DWE=0.D0
c.......[DO NOT] Allocate space [HERE] for the double precision array for DNR (distance to nearest reach)
c       and ELNR (elev. of nearest reach)
c       (moved into SFR if statement: whether or not DNR and ELNR are allocated fully depends on SFR link)
c.......Allocate space for the double precision array for Acreage-Optimization list (OPT) and initialize
C       (11: gw-, sw-, nrd-profit; irrig-flux; area; ic-cell; ir-cell; reduction-factor; gw-,sw-,nrd-red-pct) 
C       (max. # of cells per one farm is <= # of cells in domain; if equal then: domain = farm)
        ALLOCATE(OPT(11,NCOL*NROW)) 
        OPT=0D0
c.......Allocate space for the double precision array for arrays of percentage of
c       Groundwater- / Surface-water- / Non-Routed-SW- Irrigation for Acreage-reduced Cells and initialize
        ALLOCATE(REDPCT(NCOL,NROW),GWREDPCT(NCOL,NROW),
     1  SWREDPCT(NCOL,NROW),NRREDPCT(NCOL,NROW))
        REDPCT=0D0
        GWREDPCT=0D0
        SWREDPCT=0D0
        NRREDPCT=0D0
      ENDIF
C
C4O1----Allocate space for the double precision arrays if climate data time series (CLIMATE) and 
c       a list of crop specific parameters (CROPLIST) are not activated
c       (if non of the following 3 parameters is to be calculated
C        as averages over each time-step (for ICUFL = 3, or IRTFL = 3, or IPFL = 3):
c        -    Consumptive Use
c        -    Root Depth
c        -    Precipitation
      IF(IRTFL.NE.3.AND.ICUFL.NE.3.AND.IPFL.NE.3) THEN
        ALLOCATE(CROPLIST(1,1),CLIMATE(1,1))                            !DP
        ALLOCATE(ETC(1,1),RTD(1,1))
      ELSEIF(IRTFL.EQ.-1.AND.ILGR.GT.0.AND.IGRID.GT.1) THEN
        IF(FMPDAT(1)%IRTFL.NE.3.AND.FMPDAT(1)%ICUFL.NE.3.
     2     AND.FMPDAT(1)%IPFL.NE.3) THEN
        ALLOCATE(CROPLIST(1,1),CLIMATE(1,1))                            !DP
        ALLOCATE(ETC(1,1),RTD(1,1))
        ENDIF
C
C4O2----Allocate space for the double precision arrays for climate data time series (CLIMATE) and for 
c       a list of crop specific parameters (CROPLIST) (includes: coefficients to calculate time series of:
c                                                      Growing Degree Days (GDD - local variable),
c                                                      Cumulative Growing Degree Days (CGDD - local variable),
c                                                      Crop Coefficients (KC - local variable),
c                                                      Crop ET (ETC - global variable), and
c                                                      Root Depths (RTD - global vaiable) 
c       (only if one of the following 3 parameters is to be calculated
C        as averages over each time-step (for ICUFL = 3, or IRTFL = 3, or IPFL = 3):
c        -    Consumptive Use
c        -    Root Depth
c        -    Precipitation
c        and initialize
      ELSE  
        SIMLEN=0.
        DO N=1,NPER
         SIMLEN=SIMLEN+PERLEN(N)
         LENSIM=INT(SIMLEN)
        ENDDO 
        ALLOCATE(CROPLIST(12,NCROPS),CLIMATE(5,LENSIM))                 !DP
        CROPLIST=0D0
        CLIMATE=0D0
        ALLOCATE(ETC(NCROPS,LENSIM),RTD(NCROPS,LENSIM))
        ETC=0.D0
        RTD=0.D0
      ENDIF
C
C4P-----Allocate space for the double precision lists of Unranked (UNRD), Ranked (RNRD), & actually used (NRD)
c       Non-Routed Delivery Lists and initialize
      IF(INRDFL.EQ.0) THEN
        MXNRDT=1                                                        !DUMMY ONE
        ALLOCATE(UNRD(4,NFARMS))                                        !DUMMY 4       !DP
        ALLOCATE(RNRD(2,1,NFARMS))                                      !DUMMY 2,1     !DP
        ALLOCATE(NRD(2,NFARMS))                                         !DP
        NRD=0D0
      ELSE!IF(INRDFL.NE.0) THEN                                         !seb commented out later part to improve speed
        ALLOCATE(UNRD((MXNRDT*3+1),NFARMS))                             !DP (1=FID; 3 per type = NRD-vol., NRD-rank, NRD-use-flag)
        IF(ILGR.NE.0) THEN
        ALLOCATE(RNRD(2,MXNRDT*2,NFARMS))                               !DP (2 per ranked type = NRD-vol., NRD-use-flag) 2 times the MXNRDT to save old RNDR before
        ELSE                                                            !scaling it down as a result of prorating for parent and child farm NRDs.        
        ALLOCATE(RNRD(2,MXNRDT,NFARMS))                                 !DP (2 per ranked type = NRD-vol., NRD-use-flag)
        ENDIF
        ALLOCATE(NRD(2,NFARMS))                                         !DP (2: NRD-new, NRD-old)
        UNRD=0D0
        RNRD=0D0
        NRD=0D0
      ENDIF
C
C4Q1----Allocate space for one array element only for real and double precision arrays if FMP is
c       not linked to Streamflow Routing Package
      IF(IUNITSFR.EQ.0) THEN
c.......for variables of the sfr1 package
        ALLOCATE(STRM(1,1),SEG(1,1))
        ALLOCATE(DVRSFLW(1),SGOTFLW(1))
c.......for fmp variables not related to (semi-)routed deliveries
        ALLOCATE(SFRADD(1),FDSEGL(1))
c.......for fmp variables related to both routed and semi-routed deliveries
        ALLOCATE(FCSEGL(1),RDR(1),DIVADD(1),DIVTMP(1))
        ALLOCATE(FLOWINMIN(1),QSTRMIN(1))                               !DP
c.......[NOT HERE] for fmp variables related only to semi-routed deliveries
c       (srd coord./location see integer array) 
c.......for fmp variables DNR (distance to nearest reach) and ELNR (elev. of nearest reach)
        ALLOCATE(DNR(1,1),ELNR(1,1))
c.......for fmp variables WATER RIGHTS CALL for prior appropriation
        ALLOCATE(WRC(1,1))                                              !DP
c.......for fmp variable FARM ALLOTMENT for EQUAL appropriation       
        ALLOCATE(FALLOT(1))
      ELSE
C
C4Q2----Allocate space for real and double precision arrays if FMP is linked to Streamflow Routing Package
c
c.......(variables of the sfr1 package are allocated in sfr-allocate routine GWF1SFR1ALP).
c
C.......Allocate space for double precision array to lock SFR-pre-specified runoff in RP (SFRADD),
c       so that it can be amended in FMP3FM by:
c          "to be subtracted 'minus runoff' (RDEL)" for canal reaches, and by
c          "to be added 'positive runoff' (SWRUNSUM) " into drain reaches.
c          [RDEL = act. delivery from head gate reach of canal into farm (local variable);
c           SWRUNSUM = cumulative runoff from all cels in a farm (local variable)]  
c          (SFRADD is always allocated in case of an sfr-link, even if no (semi-)routed delivery from canals exists:
c           Reason for that: runoff to drains can occur although no (semi-)routed delivery from canals exists)
c       and initialize
        ALLOCATE(SFRADD(NSTRM))
        SFRADD=0.D0
c.......Allocate space for a double precision array for Farm Drain Segment Length (FDSEGL) and initialize
        ALLOCATE(FDSEGL(NFARMS))
        FDSEGL=0.D0
c
c.......Allocate space for double precision arrays depending on
c       whether (semi-)routed deliveries are desired or not!
c
c. . . . . Allocate space space for one array element only if (semi-)routed deliveries are not desired
        IF(IRDFL.EQ.0.AND.ISRDFL.EQ.0) THEN
           ALLOCATE(FCSEGL(1),RDR(1),DIVADD(1),DIVTMP(1))
           ALLOCATE(FLOWINMIN(1),QSTRMIN(1))                            !DP
c          for variables DNR (distance to nearest reach) and ELNR (elev. of nearest reach)
           ALLOCATE(DNR(1,1),ELNR(1,1))
c          for fmp variables CALL for prior appropriation
           ALLOCATE(WRC(1,1))                                           !DP
c          for fmp variable FARM ALLOTMENT for EQUAL appropriation       
           ALLOCATE(FALLOT(1))
        ELSE
C. . . . . Allocate space for the double precision array for Farm Segment Length (FSEGL) and
c          Reach Delivery Requirement (RDR) and initialize 
           ALLOCATE(FCSEGL(NFARMS),RDR(NSTRM))
           FCSEGL=0.D0
           RDR=0.D0
c. . . . . Allocate space for the double precision array to lock pre-specified Flow into (Diversion-) Segment for RP (DIVADD),
c          so that it can be amended in FM by "to be subtracted 'un-needed amount of ADELSW' (ADSW-SWSUM)"
c          in case an acreage-reduction is necessary.
c          (local var.: ADSW = act. delivery from SW; SWSUM = optimized sum of SW needed for all cells of a farm)
c          and initialize
           ALLOCATE(DIVADD(NSEGDIM),DIVTMP(NSEGDIM))                    !DIVTMP describes the reduced diversion rate 
           DIVADD=0.D0                                                  !from the last iteration of a previous stress period
           DIVTMP=0.D0
c. . . . . Allocate space for double precision arrays related to Prior Appropriation
c          for minimum canal-streamflow (FLOWINMIN) & min. river-streamflow (QSTRMIN) and initialize
           ALLOCATE(FLOWINMIN(NSTRM),QSTRMIN(NFARMS))                   !DP
           FLOWINMIN=0D0
           QSTRMIN=0D0
c. . . . . Allocate space for double precision arrays related to Acreage Optimization
c          for DNR (distance to nearest reach) and ELNR (elev. of nearest reach) and initialize
           IF(IDEFFL.LE.0) THEN
             ALLOCATE(DNR(1,1),ELNR(1,1))
           ELSEIF(IDEFFL.GT.0) THEN
             ALLOCATE(DNR(NCOL,NROW),ELNR(NCOL,NROW))
             DNR=0.D0
             ELNR=0.D0
           ENDIF
c          for fmp variables WATER RIGHTS CALL for prior appropriation and initialize
           IF(IALLOTSW.NE.2) THEN
             ALLOCATE(WRC(1,1))                                         !DP
           ELSEIF(IALLOTSW.EQ.2) THEN
             ALLOCATE(WRC(2,NFARMS))                                    !DP  (2: FID, CALL)
             WRC=0D0
           ENDIF
c          for fmp variable FARM SURFACE-WATER ALLOTMENT for EQUAL appropriation       
           IF(IALLOTSW.NE.1.AND.IALLOTSW.NE.-1) THEN
             ALLOCATE(FALLOT(1))
           ELSEIF(IALLOTSW.EQ.1.OR.IALLOTSW.EQ.-1) THEN
             ALLOCATE(FALLOT(NFARMS))
             FALLOT=0D0
           ENDIF
        ENDIF
      ENDIF
C
C4R1----Allocate space for one array element only if FMP is not linked to the Multi-Node Well Package
      IF(IUNITMNW1.EQ.0) THEN
c.......for variables of the mnw1 package
        ALLOCATE(WELL2(1,1))
C4R2----NOT NECESSARY: Allocate space if FMP is linked the Multi-Node Well Package
c.......(variables of the mnw1 package are allocated in mnw-allocate routine GWF1MNW1AL).
      ENDIF
C
C4S1----Allocate space in the DP array for Cumulative Volumes of the Compact Farm Budget
      ALLOCATE(VFB(24,NFARMS))
      VFB=0D0
C
C5===== ALLOCATE SPACE FOR INTEGER VARIABLES AND INITIALIZE ==============================================
C
C5A-----Allocate space for the integer array for the NWPERF and KNTR lists and initialize
      ALLOCATE(NWPERF(NFARMS),KNTR(NFARMS),LFID(NFARMS))                !added Farm-ID Pointer logical array to keep track of current farms--rth
      NWPERF=0
      KNTR=0
      LFID=.FALSE.                                                      !initialize logical pointer array as all false prior to reading --rth
C5B-----Allocate space for the integer arrays for the FID, CID, and SID arrays and initialize
      ALLOCATE(IFID(NCOL,NROW),IFIDOLD(NCOL,NROW),
     1 ICID(NCOL,NROW),ISID(NCOL,NROW))
      IFID=0
      IFIDOLD=0
      ICID=0
      ISID=0
C5C-----Allocate space for the integer arry for the Fallow list and initialize
      IF(IDEFFL.NE.-2) THEN
        ALLOCATE(IFALLOW(1,1))
      ELSE
        ALLOCATE(IFALLOW(2,NCROPS))
        IFALLOW=0
      ENDIF
C
C5D1-----Allocate space for one array element only if FMP is not linked to Streamflow Routing Package
      IF(IUNITSFR.EQ.0) THEN
c.......for variables of the sfr1 package
        ALLOCATE(ISTRM(1,1),IDIVAR(1,1))
c.......for fmp variables not related to (semi-)routed deliveries
        ALLOCATE(IFDRID(1,1))
c.......for fmp variables related to (semi-)routed deliveries
        ALLOCATE(IFCRID(1,1))
        ALLOCATE(NFSEG(1),IRDRFL(1))
c.......for fmp variables related only to semi-routed deliveries
        ALLOCATE(ISRD(1,1))
c.......for fmp variables related only to semi-routed runoff-returnflow
        ALLOCATE(ISRR(1,1))
      ELSE
c
C5D2----Allocate space for the integer array if FMP is linked to Streamflow Routing Package
c
c.......(variables of the sfr1 package are allocated in sfr-allocate routine GWF1SFR1ALP).
c
c.......Allocate space for the integer array for the Farm Drain-Reach ID list (FDRID) and initialize
        ALLOCATE(IFDRID(NFARMS,NSTRM))
        IFDRID=0
c
c.......Allocate space for the integer array depending on whether (semi-)routed deliveries are desired
c
c. . . . . Allocate space for the integer array if (semi-)routed deliveries are not desired
        IF(IRDFL.EQ.0.AND.ISRDFL.EQ.0) THEN
           ALLOCATE(IFCRID(1,1))
           ALLOCATE(NFSEG(1),IRDRFL(1))
        ELSE
c. . . . . Allocate space for the integer array for the Farm Canal-Reach ID list (FCRID) and initialize
           ALLOCATE(IFCRID(NFARMS,NSTRM))
           IFCRID=0
c. . . . . Allocate space for the integer array for a list of "number of a segment within a farm" (NFSEG) and initialize
           ALLOCATE(NFSEG(NFARMS))
           NFSEG=0
c. . . . . Allocate space for the integer array for a farm specific RDR flag and initialize
           ALLOCATE(IRDRFL(NFARMS))
           IRDRFL=0
        ENDIF
c
c.......Allocate space for the integer array depending on whether semi-routed deliveries are desired 
c
c. . . . . Allocate space for the integer array if semi-routed deliveries are not desired (just for information)
C        IF(ISRDFL.EQ.0) THEN
C           ALLOCATE(ISRD(1,1)) INCORRECT: THE "OR" OPERATOR IN THE FOLLOWING STATEMENT IN C8D4 AND C8D5 REQUIRES FULL ALLOCATION:
C           (ISRDFL.EQ.0.OR.(ISRDFL.GT.0.AND.ISRD(2,NF).EQ.0.AND.ISRD(3,NF).EQ.0.AND.ISRD(4,NF).EQ.0.AND.ISRD(5,NF).EQ.0))
C           ALLOCATING ONLY ONE ARRAY ELEMENT WOULD REQUIRE NOT USING THE OR OPERATOR AND THE REPETITION OF THE LONG IF STATEMENT IN C8D1
C        ELSE
c. . . . . Allocate space for the integer for list of diversion-point coordinates/location
c          of Semi-Routed Deliveries (SRD) and initialize
           ALLOCATE(ISRD(5,NFARMS))                                     !(5: Farm-ID, Row, Col., Seg., Reach)
           ISRD=0
C        ENDIF
c
c.......Allocate space for the integer array depending on whether semi-routed runoff returnflow is desired 
c
c. . . . . Allocate space for the integer array if semi-routed runoff returnflow is not desired (just for information)
C        IF(ISRRFL.EQ.0) THEN
C           ALLOCATE(ISRR(1,1)) INCORRECT: THE "OR" OPERATOR IN THE FOLLOWING STATEMENT IN C8D1 REQUIRES FULL ALLOCATION:
C           (ISRRFL.EQ.0.OR.(ISRRFL.NE.0.AND.ISRR(2,NF).EQ.0.AND.ISRR(3,NF).EQ.0.AND.ISRR(4,NF).EQ.0.AND.ISRR(5,NF).EQ.0))
C           ALLOCATING ONLY ONE ARRAY ELEMENT WOULD REQUIRE NOT USING THE OR OPERATOR AND THE REPETITION OF THE LONG IF STATEMENT IN C8D1
C        ELSE
c. . . . . Allocate space for the integer array for list of coordinates/locations of reaches, into which
c          Semi-Routed Runoff Returnflows are recharged (SRR) and initialize
           ALLOCATE(ISRR(5,NFARMS))                                     !(5: Farm-ID, Row, Col., Seg., Reach)
           ISRR=0
C        ENDIF
C
      ENDIF
C
C5E------Allocate space for one array element only if FMP is not linked to Unsaturated Zone Flow Package and initialize
      IF(IUNITUZF.EQ.0) THEN
        IF(ICCFL.EQ.1.OR.ICCFL.EQ.2) THEN
        ALLOCATE(IUZFBND(1,1),IRUNBND(1,1),
     1  FINF(1,1),EXCESPP(1,1),VKS(1,1),SEEPOUT(1,1),REJ_INF(1,1))
        IUZFBND=0
        ELSEIF(ICCFL.EQ.-1.AND.ILGR.GT.0.AND.IGRID.GT.1) THEN
          IF(FMPDAT(1)%ICCFL.EQ.1.OR.FMPDAT(1)%ICCFL.EQ.2) THEN
          ALLOCATE(IUZFBND(1,1),IRUNBND(1,1),
     1    FINF(1,1),EXCESPP(1,1),VKS(1,1),SEEPOUT(1,1),REJ_INF(1,1))
          IUZFBND=0
          ENDIF
        ENDIF
      ENDIF
C      
C5F------Allocate space for ... lgr link
      ALLOCATE(IFA(NFARMS),ISA(NSOILS),ICA(NCROPS))      
      IF(ILGR.EQ.0.OR.(ILGR.NE.0.AND.IGRID.EQ.1)) THEN      
        DO NF=1,NFARMS
          IFA(NF)=NF                                    !SCOTT IFA IS ALWAYS NONZERO. SHOULD IT CHANGE DURING EACH RP?
        ENDDO
        DO NS=1,NSOILS
          ISA(NS)=NS
        ENDDO          
        DO NC=1,NCROPS
          ICA(NC)=NC
        ENDDO     
      ELSE
        IFA=0
        ISA=0
        ICA=0
      ENDIF
C
      IF(ILGR.NE.0) THEN
      IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) IUNITSFR=0
      ENDIF
C ALLOCATE FARM LOCATION TYPE VARIABLE WHICH WILL EVENTUALLY BE ALLOCATED AGAIN TO HOLD LOCATION OF EACH FARM seb
      ALLOCATE(FMLOC(NFARMS))
C ALLOCATE DRT LOCATION AND RETURN FLOW TO FMP ARRAY
      IF(IUNITDRT.NE.0)THEN
        ALLOCATE(DRTFLOW(NFARMS))
      ELSE
        ALLOCATE(DRTFLOW(1))
      END IF
      DRTFLOW%FID=0
      DRTFLOW%FLO=0D0
C====== OPEN APPROPIATE OUTPUT FILES ======================== seb
      FMPOUT%UNIT=0                               !Initialize variables Max Length hardwired to 11
      IF(IGRID.EQ.1) THEN
       FMPOUT%NAME(1)=  'FDS.OUT'
       FMPOUT%NAME(2)=  'FWELLS.OUT'
       FMPOUT%NAME(3)=  'FNRCH_ARRAY.OUT'
       FMPOUT%NAME(4)=  'FNRCH_LIST.OUT'
       FMPOUT%NAME(5)=  'FNRCH_LIST_BIN.OUT'
       FMPOUT%NAME(6)=  'ACR_OPT.OUT'
       FMPOUT%NAME(7)=  'FB_COMPACT.OUT'
       FMPOUT%NAME(8)=  'FB_DETAILS.OUT'
       FMPOUT%NAME(9)=  'ROUT.OUT'
       FMPOUT%NAME(10)= 'RED_FMP_PMP.OUT'
       FMPOUT%NAME(11)= 'PRIOR.OUT'
       FMPOUT%NAME(12)= 'ET_ARRAY.OUT'
       FMPOUT%NAME(13)= 'ET_LIST.OUT'
      ELSE
       FMPOUT%NAME(1)=  'FDS'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(2)=  'FWELLS'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(3)=  'FNRCH_ARRAY'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(4)=  'FNRCH_LIST'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(5)=  'FNRCH_LIST_BIN'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(6)=  'ACR_OPT'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(7)=  'FB_COMPACT'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(8)=  'FB_DETAILS'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(9)=  'ROUT'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(10)= 'RED_FMP_PMP'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(11)= 'PRIOR'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(12)= 'ET_ARRAY'//NUM2STR(IGRID)//'.OUT'
       FMPOUT%NAME(13)= 'ET_LIST'//NUM2STR(IGRID)//'.OUT'
      END IF
      IF(IRTPFL.GT.0) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(9),FILE=FMPOUT%NAME(9),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'ROUT.OUT', FMPOUT%UNIT(9)=0 means no write
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(9),FNAME=FMPOUT%NAME(9),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IRTPFL = '//NUM2STR(IRTPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      ELSEIF(IRTPFL.LT.0) THEN                                          !WRITE TO LST FILE
        FMPOUT%UNIT(9)=IOUT
      ENDIF

      IF(ISDPFL.EQ.1) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(1),FILE=FMPOUT%NAME(1),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FDS.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(1),FNAME=FMPOUT%NAME(1),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'ISDPFL = '//NUM2STR(ISDPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      IF(IFBPFL.EQ.1) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(7),FILE=FMPOUT%NAME(7),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FB_COMPACT.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(7),FNAME=FMPOUT%NAME(7),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFBPFL = '//NUM2STR(IFBPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      ELSEIF(IFBPFL.EQ.2) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(8),FILE=FMPOUT%NAME(8),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FB_DETAILS.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(8),FNAME=FMPOUT%NAME(8),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFBPFL = '//NUM2STR(IFBPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      IF(1.LE.IOPFL .AND. IOPFL.LE.4)THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(6),FILE=FMPOUT%NAME(6),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'ACR_OPT.OUT' 
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(6),FNAME=FMPOUT%NAME(6),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IOPFL = '//NUM2STR(IOPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      ELSEIF (-4.LE.IOPFL .AND. IOPFL.LE.-1) THEN                       !WRITE TO LST FILE
        FMPOUT%UNIT(6)=IOUT
      END IF
      IF(IFWLCB.EQ.1) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(2),FILE=FMPOUT%NAME(2),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FWELLS.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(2),FNAME=FMPOUT%NAME(2),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFWLCB = '//NUM2STR(IFWLCB),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      
      IF(IUNITNWT.NE.0)THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(10),FILE=FMPOUT%NAME(10),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'RED_FMP_PMP.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(10),FNAME=FMPOUT%NAME(10),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FOR FMP FOR '
     +            //'REDUCED FARM WELL PUMPING RECORD FROM NWT PACKAGE',
     +           INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      IF(IFNRCB.EQ.1) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(3),FILE=FMPOUT%NAME(3),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FNRCH_ARRAY.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(3),FNAME=FMPOUT%NAME(3),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFNRCB = '//NUM2STR(IFNRCB),INFILE=IN,OUTPUT=IOUT)
        END IF
      ELSEIF(IFNRCB.EQ.2) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(4),FILE=FMPOUT%NAME(4),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'FNRCH_LIST.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(4),FNAME=FMPOUT%NAME(4),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFNRCB = '//NUM2STR(IFNRCB),INFILE=IN,OUTPUT=IOUT)
        END IF
      ELSEIF(IFNRCB.EQ.3) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(5),FILE=FMPOUT%NAME(5),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE',
     2       FORM=FORM)                                                 !OPENS 'FNRCH_LIST_BIN.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(5),FNAME=FMPOUT%NAME(5),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IFNRCB = '//NUM2STR(IFNRCB),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      IF(IPAPFL.EQ.-1)THEN
        FMPOUT%UNIT(11)=IOUT
      ELSEIF(IPAPFL.EQ.1) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(11),FILE=FMPOUT%NAME(11),IOSTAT=IERR,
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'PRIOR.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(11),FNAME=FMPOUT%NAME(11),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IPAPFL = '//NUM2STR(IPAPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
C      
      IF(IETPFL.EQ.-1 .OR. IETPFL.EQ.-2 .OR. IETPFL.EQ.-3) THEN         !WRITE ET INFORMATION TO LST FILE
        FMPOUT%UNIT(12)= IOUT
        FMPOUT%UNIT(13)= IOUT
      END IF
      IF (IETPFL.EQ.1 .OR. IETPFL.EQ.2 .OR. IETPFL.EQ.4) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(12),FILE=FMPOUT%NAME(12),
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'ET_ARRAY.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(12),FNAME=FMPOUT%NAME(12),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IETPFL = '//NUM2STR(IETPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
      IF(IETPFL.EQ.3 .OR. IETPFL.EQ.4) THEN
        OPEN(NEWUNIT=FMPOUT%UNIT(13),FILE=FMPOUT%NAME(13),
     1       STATUS='REPLACE',POSITION='REWIND',ACTION='WRITE')         !OPENS 'ET_LIST.OUT'
        IF(IERR.NE.0) THEN 
           CALL FILE_IO_ERROR(IERR,
     +           UNIT=FMPOUT%UNIT(12),FNAME=FMPOUT%NAME(12),
     +           LINE=' NO LINE READ, INSTEAD OPENING FILE FROM FLAG: '
     +             //'IETPFL = '//NUM2STR(IETPFL),INFILE=IN,OUTPUT=IOUT)
        END IF
      END IF
C      
C7===== SAVE POINTERS FOR GRID AND RETURN. ========================================================
      CALL SFMP3PSV(IGRID)

      CALL SGWF2NWT1PSV(Igrid)   !seb lgr
      CALL SGWF2UZF1PSV(Igrid)   !seb lgr
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3RQ(IN,IUNITSFR,IUNITMNW1,IUNITMNW2,
     1  IUNITNWT,IUNITDRT,IGRID,ILGR)
C-----VERSION 2 09/18/2009 FMP3RQ
C     ******************************************************************
C     READ FARM WELL PARAMETERS AND PARAMETER-FARM-WELLS LISTS, AND
C     READ OTHER INFORMATION FOR THE ENTIRE SIMULATION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE, ONLY:NFWLVL,IFWLAL,NPFWL,FWELL,IFWLPB,MXFWEL,MXPFW,
     1    IPFWEL,NFARMS,IFID,IFIDOLD,EFF,EF2D,ICID,GSURF,SOIL,ISID,
     2    SOIL2D,PSI,IFALLOW,CROPBEN,WATERCOST,ROOT,RT2D,IRTFL,ICUFL,
     3    IPFL,ICCFL,IFTEFL,IIESWFL,IEFFL,IEBFL,IROTFL,IDEFFL,IBEN,ICOST
     4    ,ISRDFL,ISRRFL,NCROPS,NSOILS,CROPLIST,CU,LENSIM,CLIMATE,ETC,
     5    RTD,FIESW,FTE,RISERUN,ISRD,ISRR,FWLAUX,NAUX,IFA,ISA,ICA,fmpdat
     6    ,IFRMFL,LFID,IALLOTGW,ALLOTGW,FMLOC,DRTFLOW,SFMP3PNT                  !Multiple WBS --rth
      USE GLOBAL,    ONLY:IOUT,NCOL,NROW,NLAY,DELR,DELC
      USE LGRMODULE, ONLY:NPLBEG,NPRBEG,NPCBEG,NPLEND,
     1                     NPREND,NPCEND,LGRDAT,ISFRGRID
      USE PARAMMODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER IGRID,IN,IUNITMNW1,IUNITSFR,ILGR,IUNITNWT,
     +  IUNITMNW2,IUNITDRT                                              !added IUNITNWT for NWT link to Farm Wells
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
c     ------------------------------------------------------------------
      CHARACTER*700 LINE
      CHARACTER*90 LABEL                                               !seb STORES LABEL USED FOR PRINTING IN FMP3WELRD
      CHARACTER*20 SOILTYPE(NSOILS)
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'     FARM IDENTIFICATION'/
      DATA ANAME(2) /'     CROP IDENTIFICATION'/
      DATA ANAME(3) /'          GROUND SURFACE'/
      DATA ANAME(4) /'SOIL TYPE IDENTIFICATION'/
      INTEGER LSTSUM,K,LSTBEG,IP,NUMINST,NLST,NINLST,I,NF,NC,NS,N,      !FORMERLY IMPLICIT INTEGER
     1 IEXITFLAG,IR,IC,IR1,IR2,IR3,IC1,IC2,IC3,IMX,IMY,ID,L,IL,
     2 LLOC,ISTART,ISTOP,II,JJ,EOR
      DOUBLE PRECISION DZDX,DZDY,TBASE,TMINCUT,TMAXCUT,CO,C1,C2,C3,RMAX,
     1 RCOE,RBEG,TMAX,TMIN,ETREF,GDD,CUMGDD,CC,RD
      REAL R
C     ------------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
C
      IF(ILGR.NE.0) THEN
        IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) THEN
          CALL SGWF2SFR7PNT(Igrid)        
          IUNITSFR=1
        ENDIF
      ENDIF
      LABEL=                                                            ! seb LABEL USED BY FMP3WELRD
     + 'WELL NO. LAYER    ROW    COL  F-WELL ID    FARM ID      QMAXfac'
C      
C1===== READ NAMED FARM WELL PARAMETERS AND RELATED PARAMETER-FARM-WELLS LISTS ===================
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,3) NPFWL
      ENDIF
    3 FORMAT(1X,//1X,I5,' Farm Well parameters')
      IF(NPFWL.GT.0) THEN
       IF(IUNITMNW2.GT.0)LABEL=TRIM(LABEL)//'     Linked MNW2 Well Name'
        !LABEL=TRIM(LABEL)//'   ...AUX VARIABLES...'   !seb NOT NEEDED BECAUSE ULSTLB AUTOMATICALLY APPENDES THE CORRECT AUX NAMES
        LSTSUM=IFWLPB
        DO 5 K=1,NPFWL
          LSTBEG=LSTSUM
C1A-----READ AND STORE LIST PARAMETER DEFINITION INFORMATION
          IF(ILGR.NE.0.AND.IGRID.GT.1.AND.K.EQ.NPFWL) THEN
c     1      (NPFWL.GT.1.AND.K.EQ.NPFWL) ) THEN
             READ(IN,'(A)') LINE
             EOR=INDEX(LINE,'#')
             IF(EOR.EQ.0) EOR=LEN(LINE)
             IF(INDEX(LINE(1:EOR),' P ').GT.0) THEN
               LINE=LINE(1:INDEX(LINE,' P ')-1)//' 0'
               OPEN(NEWUNIT=II, STATUS='SCRATCH',POSITION='REWIND')     !seb CHANGED TO SCRATCH FILE FROM JUST GENERAL WRITE TO FORT.9999
               WRITE(II,'(A)') LINE
               REWIND(II)
               CALL UPARLSTRP(LSTSUM,MXFWEL,II,IOUT,IP,'FWEL','QMAX',
     1              1,NUMINST)
               CLOSE(II, STATUS = 'DELETE')                             !SCRATCH FILES BY DEFAULT ARE DELETED BUT THIS ENSURES IT
             ELSE
               BACKSPACE IN             
               CALL UPARLSTRP(LSTSUM,MXFWEL,IN,IOUT,IP,'FWEL','QMAX',1,
     1              NUMINST)
             ENDIF
          ELSE
             CALL UPARLSTRP(LSTSUM,MXFWEL,IN,IOUT,IP,'FWEL','QMAX',1,
     1            NUMINST)
          ENDIF
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.EQ.0) THEN
C1B-----READ PARAMETER WITHOUT INSTANCES.
            IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
C              READ(IN,'(A)') LINE
c              LLOC=1
c              CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
c              IF(LINE(ISTART:ISTOP).EQ.'P') THEN
               IF(NLST.EQ.0) THEN
                IF(LSTCHK(3)) THEN
                  WRITE(IOUT,*)
                ENDIF
                CALL ULSTLB(IOUT,
     1          TRIM(LABEL),
     2          FWLAUX,5,NAUX)
                GOTO 4
              ELSE
C                BACKSPACE IN !REWIND TO BEGINNING OF LINE
            CALL FMP3WELRD(NLST,FWELL,LSTBEG,NFWLVL,MXFWEL,IFWLAL+1,IN, ! seb CHANGED PASSED STRING FOR VARIABLE LABEL
     1         IOUT,
     2    TRIM(LABEL),
     3    FWLAUX,5,NAUX,NCOL,NROW,NLAY,6,6,IPFWEL,IUNITMNW1,
     4    IUNITMNW2,IUNITNWT)
              ENDIF
            ELSE
            CALL FMP3WELRD(NLST,FWELL,LSTBEG,NFWLVL,MXFWEL,IFWLAL+1,IN, ! seb CHANGED PASSED STRING FOR VARIABLE LABEL
     1         IOUT,
     2    TRIM(LABEL),
     3    FWLAUX,5,NAUX,NCOL,NROW,NLAY,6,6,IPFWEL,IUNITMNW1,
     4    IUNITMNW2,IUNITNWT)
            ENDIF            
    4       IF(ILGR.NE.0.AND.IGRID.GT.1.AND.
     1        ((NPFWL.EQ.1.AND.NLST.LT.MXPFW).OR.
     2         (NPFWL.GT.1.AND.K.EQ.NPFWL.AND.NLST.EQ.0)) ) THEN
               IL=LSTBEG+NLST           !pull parent grid wells for list numbers starting after the child grid parameter wells
               CALL FMP3PWELTOCWEL(2,IL,LSTBEG,IPFWEL,IUNITMNW2)
               IPLOC(2,IP)=IL-1          !make sure the end of the parameter list gets updated by the pulled wells or else in RP it goes back to NLST
            ENDIF
          ELSE
C1C-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 10 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPFWEL)
C1D-----READ AND PRINT A LIST OF ATTRIBUTES ASSOCIATED WITH PARAMETER-FARM-WELLS 
            CALL FMP3WELRD(NINLST,FWELL,LSTBEG,NFWLVL,MXFWEL,IFWLAL+1,  ! seb CHANGED PASSED STRING FOR VARIABLE LABEL
     1        IN,IOUT,
     2    TRIM(LABEL),
     3    FWLAUX,5,NAUX,NCOL,NROW,NLAY,6,6,IPFWEL,IUNITMNW1,
     4    IUNITMNW2,IUNITNWT)
            LSTBEG=LSTBEG+NINLST   
   10       CONTINUE
          ENDIF  
    5   CONTINUE
      END IF
C
C2===== READ GROUND SURFACE =====================================================================
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
C2A-----PULL CHILD GSE FROM PARENT MODEL        
          CALL FMP3PRTOCH_BILINEAR(FMPDAT(1)%GSURF,GSURF)
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DDP(GSURF,ANAME(3),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSE
        CALL U2DDP(GSURF,ANAME(3),NROW,NCOL,0,IN,IOUT)
      ENDIF
C
C3===== READ FARM-ID 2D-ARRAY AND LIST OF FARM-SPECIFIC EFFICIENCY =====
C
C3A-----READ FARM-ID 2D-ARRAY
C       NOTE: The FARM ID array comprises the entire respective area,
C             disregarding small scale details like no-flow or constant head cells.
C             Net-Recharge will not be calculated for such cells!
ccrth
      IF(ILGR.NE.0.AND.IGRID.GT.1.and.IFRMFL.eq.1) THEN               !Only read IFID array if WBS same for entire simulation IFRMFL=1 --rth
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
CC3A2---PULL CHILD FID FROM PARENT MODEL        
          CALL FMP3PRTOCH(1)
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DINT(IFID,ANAME(1),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSEIF(IFRMFL.eq.1) THEN                                       !Only read IFID array if WBS same for entire simulation IFRMFL=1 --rth
        CALL U2DINT(IFID,ANAME(1),NROW,NCOL,0,IN,IOUT)
        IF(ILGR.NE.0.AND.IGRID.EQ.1) IFIDOLD=IFID
      ENDIF
C seb BUILD NEW INDEX OF THE LOCATION OF ALL FARMS AND WHAT ARE ACTIVE FOR ENTIRE SIMULATION 
      IF(IFRMFL.EQ.1) THEN                                              !ONLY BUILD INDEX WHEN FARMS ARE NOT CHANGING FOR ENTIRE SIMULATION
       DO CONCURRENT (JJ=1:NCOL,II=1:NROW,IFID(JJ,II).GT.0)
         LFID(IFID(JJ,II))=.TRUE.
       END DO
       FMLOC%Count=0                                                    !INITIAL FARM CELL COUNT FOR ALL FARMS
       DO NF=1, NFARMS
         N=COUNT(NF.EQ.IFID)                                            !COUNT THE NUMBER OF CELLS THAT CONTAIN FARM NF
         FMLOC(NF)%Count=N                                              !STORE FARN COUNT IN LOGICAL VARIABLE
         IF (N>0) THEN
           ALLOCATE(FMLOC(NF)%RC(2,N))
           II=0                                                         !COUNTER FOR NUMBER OR RC READS
           DO IR=1,NROW                                                 !SEARCH FOR FARM NF IN MODEL AND STORE ITS ROW/COL LOCATION
           DO IC=1,NCOL
             IF(IFID(IC,IR).EQ.NF) THEN
              II=II+1
              FMLOC(NF)%RC(1,II)=IR
              FMLOC(NF)%RC(2,II)=IC
             END IF
           END DO
           END DO
          IF (II.NE.N) STOP 'FARM COUNT ERROR II.NE.N'                    ! seb debug check, remove with final version
         END IF
       END DO
      END IF
C IF DRT IS ACTIVE BUILD NEW INDEX OF DRAIN RETURN FLOWS
      IF( IUNITDRT.NE.0 ) THEN
        DRTFLOW%FID=0
        DRTFLOW%FLO=0D0
        I=1
        DO NF=1, NFARMS
         IF(LFID(NF))THEN
           DRTFLOW(I)%FID=NF
           I=I+1
         END IF
        END DO
      END IF
ccrth
C
C3B-----INITIALIZE AND READ FARM-SPECIFIC EFFICIENCY LIST/MATRIX 
      IF(IEFFL.EQ.1) THEN !the do loop seems redundant, ... already initialized in allocate routine
c        DO NF=1,NFARMS
c        DO NC=1,1+NCROPS
c           EFF(NC,NF)=0D0
c        ENDDO
c        ENDDO
       EFF=0D0
       CALL FMP3DPLSTRD(EFF,NCROPS+3,NFARMS,IN,IOUT,2,NCROPS+1,NCROPS+1)
      ENDIF
      IF(IEFFL.EQ.-1.AND.FMPDAT(1)%IEFFL.EQ.1) THEN
        EFF=FMPDAT(1)%EFF
        IEFFL=1
      ENDIF
C
C4===== READ SOIL-ID 2D-ARRAY AND LIST OF SOIL-SPECIFIC ATTRIBUTES, AND CREATE 2D-ARRAY OF CAPILLARY FRINGE ===
C
C4A-----READ SOIL-ID 2D-ARRAY
      IF(ILGR.NE.0.AND.ILGR.NE.0.AND.IGRID.GT.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
CC4A2---PULL CHILD SID FROM PARENT MODEL        
          CALL FMP3PRTOCH(2)
          SOIL=FMPDAT(1)%SOIL
          GOTO 19
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DINT(ISID,ANAME(4),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSE
        CALL U2DINT(ISID,ANAME(4),NROW,NCOL,0,IN,IOUT)
      ENDIF
C
C4B-----READ LIST OF SOIL TYPE PARAMETERS:
C       for ICCFL=1,2,3,4:    SOIL-ID; CAPILLARY FRINGE; and
C       for ICCFL=1 and 3 only:
C             optionally: 3 COEFFICIENTS TO CALCULATE DEPLETED ROOT ZONE AT STEADY STATE;
C                         2 COEFFICIENTS TO CALCULATE SINUOSITY COEFFICIENT N;
C             or:         READ IN SOILTYPE AS A WORD IN ORDER TO USE DEFAULT 
C                         ANALYTICAL SOLUTIONS FOR THE RESPECTIVE SOILTYPE IN FM-ROUTINE      
      CALL FMP3DPWDLSTRD(SOIL,SOILTYPE,7,NSOILS,IN,IOUT,2,2)
      IF((ICCFL.EQ.1.OR.ICCFL.EQ.3).OR.(ICCFL.EQ.-1.AND.
     1   (FMPDAT(1)%ICCFL.EQ.1.OR.FMPDAT(1)%ICCFL.EQ.3) ) ) THEN
      DO NS=1,NSOILS
      IF(ISA(NS).NE.0) THEN
      IF(SOILTYPE(NS).EQ."SILT")      SOIL(3,NS)=-999D0
      IF(SOILTYPE(NS).EQ."SANDYLOAM") SOIL(3,NS)=-998D0
      IF(SOILTYPE(NS).EQ."SILTYCLAY") SOIL(3,NS)=-997D0
      ENDIF
      ENDDO
      ENDIF
   19 IEXITFLAG=0
C
C4C-----CHECK FOR EXCLUSIONS AND STOP PROGRAM 
         DO IR=1,NROW
         DO IC=1,NCOL 
            IF(ISID(IC,IR).EQ.0.AND.IFID(IC,IR).NE.0) THEN            !SCOTT Note that IFID=0 FOR WHEN IFRMFL.EQ.2
            IEXITFLAG=IEXITFLAG+1
            IF(LSTCHK(1)) THEN
              IF(IEXITFLAG.EQ.1) WRITE(IOUT,20)
            ENDIF
   20 FORMAT(//,1X,
     1'SOIL-ID CANNOT BE ZERO IN CELLS THAT HOLD A NON-ZERO FARM-ID:',/,
     2' -------------------------------------------------------------',
     3/,1X,'(   ROW) (COLUMN)')
            IF(LSTCHK(1)) THEN
              IF(IEXITFLAG.GE.1) WRITE(IOUT,21) IR,IC
            ENDIF
   21 FORMAT(2X,I6,3X,I6)
            ENDIF
         ENDDO
         ENDDO
C
C4D-----CREATE 2D-ARRAYS OF CAPILLARY FRINGE
      IF(IEXITFLAG.GT.0) STOP
      DO NS=1,NSOILS
      IF(ISA(NS).NE.0) THEN
         DO IR=1,NROW
         DO IC=1,NCOL
            IF(ISID(IC,IR).EQ.INT(SOIL(1,NS))) THEN
            DO N=1,6
            SOIL2D(N,IC,IR)=SOIL(1+N,NS)
            ENDDO
            ENDIF
         ENDDO
         ENDDO
      ENDIF
      ENDDO
C
C5===== READ CROP-ID 2D-ARRAY AND LISTS OF CROP-SPECIFIC ATTRIBUTES, AND CREATE 2D-ARRAYS OF ATTRIBUTES ======
C
C5A-----READ CROP-ID 2D-ARRAY (IROTFL>=0: CROP-PATTERN DOES NOT CHANGE)
C       NOTE: The CROP ID array comprises the entire respective area,
C             disregarding small scale details like no-flow or constant head cells.
C             Net-Recharge will not be calculated for such cells!
      IF(IROTFL.GE.0) THEN                                
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN            ! What does this mean --rth  seb: Orginal code: ILGR.NE.0.AND.ILGR.NE.0.AND.IGRID.GT.1  !REMOED REDUNDANT CHECK
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
CC5A2---PULL CHILD CID FROM PARENT MODEL        
          CALL FMP3PRTOCH(3)
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DINT(ICID,ANAME(2),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSE
        CALL U2DINT(ICID,ANAME(2),NROW,NCOL,0,IN,IOUT)
      ENDIF
      ENDIF
C
C5B-----READ CROP-SPECIFIC DEPTH OF ROOTZONE & CALCULATE 2D-ARRAY OF ROOTZONE-DEPTHS IF
C       ROOT ZONE DEPTH IS TO BE READ AS A CONSTANT FOR THE ENTIRE SIMULATION
      IF(IRTFL.EQ.1) CALL FMP3LSTRD(ROOT,2,NCROPS,IN,IOUT,2,2,2)
      IF(IRTFL.EQ.-1.AND.FMPDAT(1)%IRTFL.EQ.1) THEN
        ROOT=FMPDAT(1)%ROOT
        IRTFL=1
      ENDIF
      IF(IRTFL.EQ.1) THEN
      IF(IROTFL.GE.0.AND.IRTFL.EQ.1) THEN                               !IF IROTFL=-1: ARRAY WILL BE BUILD IN RP AFTER CROP-PATTERN IS READ IN!
      DO IR=1,NROW
      DO IC=1,NCOL
        DO NC=1,NCROPS      
          IF(ICA(NC).NE.0) THEN
            IF(ICID(IC,IR).EQ.INT(ROOT(1,NC))) THEN
              IF(ROOT(2,NC).LE.0.D0) THEN
                IF(LSTCHK(1)) THEN
                  WRITE(IOUT,*)
     1          "INPUT ERROR: ROOT ZONE DEPTH MUST BE A POSITIVE NUMBER"
                ENDIF
                STOP
              ENDIF
              RT2D(IC,IR)=ROOT(2,NC)
            ENDIF
          ENDIF
        ENDDO
        IF(ICID(IC,IR).EQ.-1) THEN
           RT2D(IC,IR)=1.D-3                                            !IN CASE ET CONCEPT = 1, TERM TRZ/RRZ SHOULD NOT BE "DEVIDE BY ZERO"
        ENDIF
      ENDDO
      ENDDO
      ENDIF
      ENDIF
C      
C5C-----READ TRANSPIRATORY AND EVAPORATIVE FRACTIONS OF CROP CONSUMPTIVE USE
C       (ONE LUMPED VALUE PER ENTIRE SIMULATION)
      IF(IFTEFL.EQ.1) CALL FMP3DPLSTRD(FTE,4,NCROPS,IN,IOUT,2,4,4)
      IF(IFTEFL.EQ.-1.AND.FMPDAT(1)%IFTEFL.EQ.1) FTE=FMPDAT(1)%FTE
      IF(IFTEFL.EQ.1.OR.(IFTEFL.EQ.-1.AND.FMPDAT(1)%IFTEFL.EQ.1)) THEN
      
      IF(LSTCHK(3)) THEN
        write(iout,103)
      ENDIF
  103 format(1x,/
     1'Transpiratory and Evaporative Fractions of Crop Consumptive Use',
     1/,'    % Transpiration % Evap. for Precip. % Evap. for Irrig.')
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
      IF(LSTCHK(3)) THEN
        write(IOUT,104)
     1INT(FTE(1,NC)),FTE(2,NC),FTE(3,NC),FTE(4,NC)
      ENDIF
  104 format(1X,I4,3F15.3)
      ENDIF
      ENDDO
      ENDIF
C
C5D-----READ FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF FROM PRECIPITATION AND FROM IRRIGATION
C       OR ELSE CALCULATE SLOPE AND RELATE IN-EFFICIENT LOSSES TO SLOPE
C
C5D1----READ FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF FROM PRECIPITATION AND FROM IRRIGATION
      IF(IIESWFL.EQ.1) CALL FMP3DPLSTRD(FIESW,3,NCROPS,IN,IOUT,2,3,3)
      IF(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.1) FIESW=FMPDAT(1)%FIESW
      IF(IIESWFL.EQ.1.OR.(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.1))THEN
      IF(LSTCHK(3)) THEN
        write(iout,106)
      ENDIF
  106 format(1x,/
     1'FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF FROM ',/,
     2'     PRECIPITATION:  IRRIGATION:')
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
      IF(LSTCHK(3)) THEN
        write(IOUT,107) IDINT(FIESW(1,NC)),FIESW(2,NC),FIESW(3,NC)
      ENDIF
  107 format(1X,I4,2F15.3)
      ENDIF
      ENDDO
C5D2----CALCULATE SLOPE AS RISE-RUN TO RELATE FIESWP AND FIESWI IN FM TO SLOPE    
      ELSEIF
     1 (IIESWFL.EQ.0.OR.(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.0))THEN
      CALL RISE_RUN()
      ENDIF
C
C5E-----READ ROOT UPTAKE PARAMETERS FOR EACH CROPS (STRESS RESPONSE FUNCTION) FOR ET CONCEPT 2
      IF(ICCFL.EQ.1.OR.ICCFL.EQ.3) THEN
      CALL FMP3DPLSTRD(PSI,5,NCROPS,IN,IOUT,2,5,5)
      ENDIF
      IF(ICCFL.EQ.-1.AND.
     1  (FMPDAT(1)%ICCFL.EQ.1.OR.FMPDAT(1)%ICCFL.EQ.3) ) THEN
      PSI=FMPDAT(1)%PSI
      ICCFL=FMPDAT(1)%ICCFL
      ENDIF       
C
C5F-----READ CROPLIST (GDD-DAYS, Kc, AND ROOT-DEPTH RELEVANT PARAMETERS)
      IF(IRTFL.EQ.3.OR.ICUFL.EQ.3.OR.IPFL.EQ.3) THEN
      CALL FMP3DPLSTRD(CROPLIST,12,NCROPS,IN,IOUT,0,0,12)
      ENDIF
      IF(IRTFL.EQ.-1.AND.(FMPDAT(1)%IRTFL.EQ.3.OR.FMPDAT(1)%ICUFL.EQ.3.
     1   OR.FMPDAT(1)%IPFL.EQ.3 ) ) THEN
      CROPLIST=FMPDAT(1)%CROPLIST
      ENDIF
C
C6===== READ CLIMATE LIST AND CALCULATE ETC AND RTD IF ROOT ZONE DEPTH OR    ===============================
C       CONSUMPTIVE USE OR PRECIPITATION ARE TO BE READ AS DAILY TIME SERIES
C
      IF(IRTFL.EQ.3.OR.ICUFL.EQ.3.OR.IPFL.EQ.3) THEN
C6A-----READ CLIMATE LIST
      CALL FMP3DPLSTRD(CLIMATE,5,LENSIM,IN,IOUT,0,0,5)
      ENDIF
      IF(IRTFL.EQ.-1.AND.(FMPDAT(1)%IRTFL.EQ.3.OR.FMPDAT(1)%ICUFL.EQ.3.
     1   OR.FMPDAT(1)%IPFL.EQ.3 ) ) THEN
      CLIMATE=FMPDAT(1)%CLIMATE
      IF(IRTFL.EQ.-1.AND.FMPDAT(1)%IRTFL.EQ.3) IRTFL=3
      IF(ICUFL.EQ.-2.AND.FMPDAT(1)%ICUFL.EQ.3) ICUFL=3
      IF(IPFL .EQ.-1.AND.FMPDAT(1)%IPFL .EQ.3) IPFL =3
      ENDIF
C
C6B-----CALCULATE GROWING DEGREE DAYS (GDD), CROP COEFFICIENT (CC),   [NOT STORED]
C       CROP EVAPOTRANSPIRATION (ETC) AND ROOT DEPTH (RTD) [STORED AS TIME-SERIES]
      IF(IRTFL.EQ.3 .OR. ICUFL.EQ.3 .OR. IPFL.EQ.3) THEN
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
      CU(1,NC)=CROPLIST(1,NC)
      TBASE=CROPLIST(2,NC)     
      TMINCUT=CROPLIST(3,NC)
      TMAXCUT=CROPLIST(4,NC)
      CO=CROPLIST(5,NC)
      C1=CROPLIST(6,NC)
      C2=CROPLIST(7,NC)
      C3=CROPLIST(8,NC)
      RBEG=CROPLIST(9,NC)      
      RMAX=CROPLIST(10,NC)
      RCOE=CROPLIST(11,NC)
      CU(3,NC)=CROPLIST(12,NC)
      CUMGDD=0.D0
      GDD=0.D0
      RD=0.D0
      DO L=1,LENSIM
      TMAX=CLIMATE(2,L)
      TMIN=CLIMATE(3,L)                                                 !CLIMATE(4,L) is precipitation, but not needed here (-> AD)
      ETREF=CLIMATE(5,L)
C
      IF(TMAX.GE.TMAXCUT .AND. TMIN.LE.TMINCUT) THEN
        GDD=(TMAXCUT+TMINCUT)/2D0 - TBASE
      ELSEIF(TMAX.GE.TMAXCUT .AND. TMIN.GT.TMINCUT) THEN
               GDD=(TMAXCUT+TMIN)/2D0 - TBASE
      ELSEIF(TMAX.LT.TMAXCUT .AND. TMIN.LE.TMINCUT) THEN
                     GDD=(TMAX+TMINCUT)/2D0 - TBASE
      ELSEIF(TMAX.LT.TMAXCUT .AND. TMIN.GT.TMINCUT) THEN
                           GDD=(TMAX+TMIN)/2D0 - TBASE
      ENDIF
      IF(GDD.LT.0) GDD=0
      CUMGDD=CUMGDD+GDD
      CC=CO+CUMGDD*C1+(CUMGDD**2)*C2+(CUMGDD**3)*C3
      IF(CC.LT.0.D0) CC=0.D0
      ETC(NC,L)=CC*ETREF
      IF(RCOE*CUMGDD.GT.RMAX) THEN
         RD=RMAX
      ELSEIF(RCOE*CUMGDD.LT.RBEG) THEN
         RD=RBEG
      ELSE
         RD=RCOE*CUMGDD
      ENDIF
      RTD(NC,L)=RD
      IF(RTD(NC,L).LE.0.D0) THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,*)
     1   "INPUT ERROR: ROOT ZONE DEPTH MUST BE A POSITIVE NUMBER"
        ENDIF
         STOP
      ENDIF
      ENDDO
      ENDIF
      ENDDO
C
      ENDIF
C
C7===== READ FALLOW FLAG IF WATER-STACKING FOR NON-FALLOWABLE CROPS IS WANTED ===============================
C       FOR EACH CROP READ FLAG IF IT CAN GO FALLOW DURING WATER SHORTAGE OR
C       DURING NON-IRRIGATION SEASONS:
C       FALLOW(1,NC): DUMMY VARIABLE FOR NUMBER OF CROP TYPE
C       FALLOW(2,NC): FALLOW FLAG (1- CAN GO FALLOW / 0- CAN'T GO FALLOW)
      IF(IDEFFL.EQ.-2) THEN
      CALL FMP3INTLSTRD(IFALLOW,2,NCROPS,IN,IOUT) 
      ENDIF
C
C8===== FOR ACREAGE-OPTIMIZATION, READ CROP-BENEFITS LIST AND WATERCOST COEFFICIENTS ========================
C
      IF(IDEFFL.GT.0) THEN
C8A-----FOR EACH CROP READ LIST OF CROP-BENEFITS FOR ENTIRE SIMULATION:
C       CROPBEN(1,NC): DUMMY VARIABLE FOR NUMBER OF CROP TYPE
C       CROPBEN(2,NC): SLOPE OF WATERPRODUCTION FUNCTION (ET[L],Y[MASS/AREA])
C       CROPBEN(3,NC): INTERCEPT OF WPF: SHOULD BE ZERO FOR 2 REASONS:
C                                        1. NEGLIGIBLY SMALL COMPARED TO SLOPE OF ET[L],Y[MASS/AREA]
C                                        2. FOR SMALL ET PER DAILY TIME UNIT, INTERCEPT HAS TO BE ZERO!
C       CROPBEN(4,NC): CROP MARKET PRICE [$/MASS]
        IF(IBEN.EQ.1) CALL FMP3DPLSTRD(CROPBEN,4,NCROPS,IN,IOUT,0,0,4)
C
C8B-----READ FOR EACH FARM GROUNDWATER AND SURFACE-WATER COST COEFFICIENTS FOR ENTIRE SIMULATION:
C       WATERCOST(1,NF): DUMMY VARIABLE FOR FARM-ID
C       WATERCOST(2,NF): BASE MAINTENANCE COST FOR PUMP PER VOLUME OF GROUNDWATER [$/L3]
C       WATERCOST(3,NF): COST PER VERTICALLY PUMPED VOLUME OF GROUNDWATER PER UNIT LIFT IN WELL [$/(L3*L)]
C       WATERCOST(4,NF): COST PER VERTICALLY PUMPED VOLUME OF GROUNDWATER PER UNIT LIFT ON SURFACE    [$/(L3*L)]
C       WATERCOST(5,NF): COST PER HORIZONTALLY DELIVERED VOLUME OF GW PER UNIT DISTANCE (e.g. dep. on friction) [$/(L3*L)]
C       WATERCOST(6,NF): FIXED PRICE PER VOLUME OF ROUTED OR SEMI-ROUTED SURFACE-WATER [$/L3]
C       WATERCOST(7,NF): COST PER VERTICALLY PUMPED VOLUME OF SURFACE-W. PER UNIT LIFT ON SURFACE [$/(L3*L)]
C       WATERCOST(8,NF): COST PER HORIZONTALLY DELIVERED VOLUME OF SW PER UNIT DISTANCE (e.g. dep. on friction) [$/(L3*L)]
C       WATERCOST(9,NF): FIXED PRICE PER VOLUME OF NON-ROUTED SURFACE-WATER [$/L3]
        IF(ICOST.EQ.1)
     1  CALL FMP3DPLSTRD(WATERCOST,9,NFARMS,IN,IOUT,2,9,9)
      ENDIF
C
C8C
ccrth
      IF(IALLOTGW.EQ.1) THEN
        CALL FMP3DPLSTRD(ALLOTGW,2,NFARMS,IN,IOUT,2,2,2)       
      ENDIF
      IF(IALLOTGW.EQ.-1.AND.FMPDAT(1)%IALLOTGW.EQ.1) THEN
         ALLOTGW=FMPDAT(1)%ALLOTGW
      ENDIF                    
ccrth
C
C9===== READ INFORMATION IF LINKED TO STREAMFLOW ROUTING PACKAGE ============================================
      IF(IUNITSFR.GT.0) THEN
C
C9A-----READ SEMI-ROUTED DELIVERY PER FARM
      IF(ISRDFL.EQ.1) THEN
      CALL FMP3INTLSTRD(ISRD,5,NFARMS,IN,IOUT)
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,110)
      ENDIF
  110 FORMAT(/,1X,
     1'SEMI-ROUTED DELIVERIES TO FARMS (LOCATION OF DIVERSION-POINT):',/
     2,1X,'FARM-ID    ROW    COLUMN   SEGMENT     REACH')
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,111) (ISRD(N,NF),N=1,5)
      ENDIF
  111 format(1X,2I7,4I10)
      ENDIF
      ENDDO
      ENDIF
C
C9B-----READ SEMI-ROUTED RUNOFF-RETURNFLOW PER FARM
      IF(ISRRFL.EQ.1) THEN
      CALL FMP3INTLSTRD(ISRR,5,NFARMS,IN,IOUT)
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,112)
      ENDIF
  112 FORMAT(/,1X,
     1'SEMI-ROUTED RUNOFF-RETURNFLOW FROM FARMS ',
     2'(LOCATION WHERE RUNOFF ENTERS A STREAM REACH):',/
     3,1X,'FARM-ID    ROW    COLUMN   SEGMENT     REACH')
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,113) (ISRR(N,NF),N=1,5)
      ENDIF
  113 format(1X,2I7,4I10)
      ENDIF
      ENDDO
      ENDIF
      ENDIF
C
C10==== CREATE 2D-ARRAY OF EFFICIENCY IEBFL=1 (AT END OF RQ: IFID & ICID ARE ALREADY READ) ================
C       (IEBFL=1: Conservative Behavior - Constant Efficiency over Time Step)
      IF(IEFFL.EQ.1.AND.IEBFL.LE.1.and.IFRMFL.eq.1) THEN                !Check efficiencies here if IFRMFL =1 for entire simulation
        DO NF=1,NFARMS
        IF(IFA(NF).NE.0) THEN
        DO NC=2,1+NCROPS
           DO IR=1,NROW
           DO IC=1,NCOL                                                 !seb FMP NO LONGER SUPPORTS OFE [FID, OFE], NOW ONLY [FID OFE(CID)]
!              IF(NC.GT.2.AND.EFF(NC,NF).EQ.0D0) THEN
!                IF(IFID(IC,IR).EQ.INT(EFF(1,NF))) THEN
!                EF2D(IC,IR)=EFF(2,NF)
!                ENDIF
!              ELSE
!                IF(IFID(IC,IR).EQ.INT(EFF(1,NF)).AND.
!     1          ICID(IC,IR).EQ.NC-1) THEN
!                EF2D(IC,IR)=EFF(NC,NF)
!                ENDIF
!              ENDIF
              IF(IFID(IC,IR).EQ.INT(EFF(1,NF)).AND.
     +                                         ICID(IC,IR).EQ.NC-1) THEN !SCOTT Note that IFID=0 FOR WHEN IFRMFL.EQ.2
              EF2D(IC,IR)=EFF(NC,NF)
              ENDIF
           ENDDO
           ENDDO
        ENDDO
        ENDIF
        ENDDO
      ENDIF
C
      IF(ILGR.NE.0) THEN
      IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) IUNITSFR=0
      ENDIF
C
C11==== RETURN ================================================================
      RETURN
      END SUBROUTINE
C
C     
      SUBROUTINE FMP3RP(IN,KPER,IUNITSFR,IUNITMNW1,IUNITMNW2,
     1  IUNITNWT,IGRID,NGRIDS,ILGR)
C-----VERSION 2 09/18/2009 FMP3RP
C     ******************************************************************
C     READ NEW FARM WELL LOCATIONS AND STRESS RATES, AND
C     READ OTHER INFORMATION FOR EACH STRESS PERIOD
C     ******************************************************************
C        SPECIFICATIONS:
C     -------------------------------------------------------------------
      USE FMPMODULE, ONLY:FWLAUX,NAUX,FWELL,NFWELS,MXFWEL,NFWLVL,IFWLAL,
     1  QMAXF,NWPERF,NFARMS,NNPFWL,NPFWL,IFWLPB,IPFWEL,RT2D,ROOT,IRTFL,
     2  ICUFL,IPFL,IFTEFL,IIESWFL,IEFFL,IEBFL,IFID,IFIDOLD,EFF,EF2D,CU,
     3  ICID,CU2D,ETR,EGW,NCROPS,PFLX,PFLR,TPPOT,EPPOT,FIESW,FTE,CROPBEN
     4  ,WATERCOST,NRD,UNRD,RNRD,MXNRDT,ISRD,ISRR,ALLOTSW,TGW,FCSEGL,
     5  FDSEGL,IFCRID, IFDRID,NFSEG,SFRADD,DIVADD,DIVTMP,IDEFFL,IBEN,
     6  ICOST,INRDFL,ISRDFL,ISRRFL,IRDFL,IRRFL,IALLOTSW,IROTFL,ICCFL,
     7  WRC,GSURF,IRTPFL,IFA,ICA,ISTARTFL,FMPDAT,IFRMFL,LFID,IALLOTGW,  !added IFRMFL flag for changing WBS (FID's) for each stress period
     8  ALLOTGW,QMAXFW,QFRACF,FMPOUT,FWLAUXORDER,AUXV,MNW2LOC,MNW2NAM,  !Added GW allotment arrays -- rth   added FMPOUT,FWLAUXORDER  seb
     9  FMLOC,SFMP3PNT  
      USE GLOBAL, ONLY:IOUT,NCOL,NROW,NLAY,DELR,DELC,PERLEN,NPER,IBOUND
      USE GWFSFRMODULE, ONLY:ISTRM,STRM,NSTRM,IDIVAR,NSEGDIM,SEG
      USE LGRMODULE,    ONLY:NPLBEG,NPRBEG,NPCBEG,NPLEND,
     1                       NPREND,NPCEND,LGRDAT,ISFRGRID
      USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD,WELLID,MNWMAX,CapTable,MNWPRNT!seb MNW2 LINK DATA
      IMPLICIT NONE
C     -------------------------------------------------------------------
C        ARGUMENTS:
C     -------------------------------------------------------------------
      INTEGER IGRID,IUNITSFR,IN,IUNITMNW1,IUNITNWT,NGRIDS,
     1 KPER,ILGR,IUNITMNW2
C     -------------------------------------------------------------------
C        LOCAL VARIABLES:
      CHARACTER*700 LINE
      CHARACTER*90 LABEL                                               !seb STORES LABEL USED FOR PRINTING IN FMP3WELRD
      CHARACTER*24 ANAME(4)
      DATA ANAME(1) /'      PRECIPITATION FLUX'/
      DATA ANAME(2) /'     CROP IDENTIFICATION'/
      DATA ANAME(3) /' REF. EVAPOTRANSPIRATION'/
      DATA ANAME(4) /'WATER-BALANCE REGION FID'/                          ! Add annotation for reading variable FARM-ID's written to list file
      INTEGER N,ITMP,ITMP2,NP,IOUTU,MXACTFW,NREAD,NF,NWPF,NC,IR,IC,L,IT,  !FORMERLY IMPLICIT INTEGER
     1 IRT,LOLD1,LOLD2,IFCRIDOLD,NRCH,ICNT,NR,IA,IB,IL,NFF,M,LLOC,
     2 ISTART,ISTOP,II,JJ
      DOUBLE PRECISION QSUMF,FCANALSEGL,FDRAINSEGL,GSURFOLD,ASUM,BSUM,C,!FORMERLY IMPLICIT REAL
     1 COLD,COUNT
      REAL R
C     -------------------------------------------------------------------
      INTEGER:: I,J,FID,WID,CAPMULT                                     !seb
      INTEGER:: FIRSTNODE,LASTNODE
      DOUBLE PRECISION:: QMNW2DES,QMNW2ACT,PUMPCAP
C     -------------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
      CALL SGWF2UZF1PNT(Igrid)   !seb lgr
      CALL SGWF2MNW2PNT(IGRID)   !seb lgr
      CALL SGWF2SFR7PNT(Igrid)   !seb lgr
C
      IF(ILGR.NE.0) THEN
        IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) THEN
          CALL SGWF2SFR7PNT(IGRID)
          IUNITSFR=1
        ENDIF  
      ENDIF
C      
cc-----DISABLE PARENT FARMS AND ALL ASSOCIATED PROCESSES WHERE CHILD MODELS EXIST.
CC     (HAD TO BE COMMENTED OUT TO
CC     (A) NOT ZERO OUT FOR PARENT ONLY ITERATION AT LGRITER=1, AND
CC     (B) ALLOW ORIGINAL PARENT FARM IDs TO BE USED FOR FINDING RETURNFLOW REACHES PASSING THROUGH OR
CC         NEAREST TO A LOWEST ELEVATION WHEN A CHILD FARM'S DELIVERY AND RETURNFLOW SYSTEM IS SERVICED BY THE PARENT FARM)
c      IF(ILGR.GT.0.AND.KPER.EQ.1.AND.IGRID.EQ.1) THEN
c        DO IL=1,NLAY
c        DO IR=1,NROW
c        DO IC=1,NCOL
c           DO N=1,NGRIDS
c             IF(IL.GE.LGRDAT(N)%NPLBEG.AND.IL.LE.LGRDAT(N)%NPLEND.AND.
c     1          IR.GE.LGRDAT(N)%NPRBEG.AND.IR.LE.LGRDAT(N)%NPREND.AND.
c     2          IC.GE.LGRDAT(N)%NPCBEG.AND.IC.LE.LGRDAT(N)%NPCEND) THEN
c                IFID(IC,IR)=0
c             ENDIF
c           ENDDO
c        ENDDO
c        ENDDO
c        ENDDO
c      ENDIF
c      IF(KPER.EQ.1) THEN
c        WRITE(1234,*) IGRID
c        IF(IGRID.EQ.1) WRITE(1234,'(20I7)') IFID,IBOUND
c        IF(IGRID.EQ.2) WRITE(1234,'(9I7)') IFID,IBOUND
c        IF(IGRID.EQ.3) WRITE(1234,'(12I7)') IFID,IBOUND
c      ENDIF
C
C1===== SET ISTARTFL FLAG ============================================================================
C
C1A-----SET FLAG TO INDICATE START OF STRESS PERIOD AS START OF CHANGING CONDITIONS OF CONSUMPTIVE USE 
c       (info: if consumptive use changes at each time-step, then istartfl is set zero in FMP3AD.)
      ISTARTFL=0
      !IF(IFRMFL.EQ.1)THEN                                         ! seb Added .TRUE. for when not changing for SP
      !  LFID=.TRUE.                                               ! SCOTT THIS MAY HAVE BEEN A MISTAKE ON MY PART, I THINK IT SHOULD BE COMMENTED OUT TO USE LFID DEFINTED IN THE AR ROUTINE SINCE FARMS ARE NOT CHANGING FOR STRESS PERIOD
      !ELSEIF(IFRMFL.EQ.2)THEN                                     !For changing Farms always reset pointers back to false for each stress period --rth
      !  LFID=.FALSE.
      !END IF  
c
C1B-----FOR USE OF SFR1 AND (SEMI-)ROUTED DELIVERIES, SET FLAG TO SKIP FMP3FM IN 1ST ITERATION
C       
      IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0)) THEN
C
C1B1----FOR USE OF ACREAGE-OPTIMIZATION WITH "WATER-CONSERVATION POOL" ACCOUNTING ...
      if(IDEFFL.EQ.2) then
          DO N=1,NSEGDIM                                          
C
C     ... SET FLAG SUCH THAT FMP3FM BE SKIPPED IN 1ST ITERATION (MF2K5 WITH SFR7 ONLY):
c         Current diversion rate (seg(2,n) <> "locked," orig. diversion rate (divadd(n)(possible for 2 cases):
c         1.) for SFR1-re-use option: if red. diversion < spec. diversion,
c         2.) for SFR1-nonreuse-option: if reduced or non-reduced diversion <> new spec. diversion
c         If new rate (divadd) is higher than old one (seg(2,n)), then set istartfl=-1 (istartfl=0 in FMP3FM),
c         * to cause istartfl=0 in FMP3FM, and to skip 1st FMP3FM iteration:
c              Only allow the 1st SFR-FM iteration, in order to get a good initial value for the 2nd iteration.
c              Otherwise old reach-inflows FLOWIN and reach-leakages FLOBOT (calc. during the last iteration
c              of the previous time step) would be subtracted from the new diversion rate.
c         * to cause istartfl=1 during the 2nd FMP3FM iteration, in order to reset
c           reduced cell-area percentages to allow a new acreage-optimization, if needed.
          if(seg(2,n).ne.SNGL(divadd(n))) istartfl=-1
C
C     ... RESET REDUCED DIVERSION RATE OF CANAL SEGMENTS FROM LAST ITERATION OF PREVIOUS STRESS PERIOD
C         TO ORIGINAL RE-USED OR NEW VALUE SPECIFIED FOR STRESS PERIOD:
c         Current div.rate (seg(2,n)) = red. div.rate from last iter. of prev. stress-per. (divtmp(n))
c         Reduced diversion rates (due to little profitability of SW-irrigation) are only reused from
c         iteration to iteration, but reset to original stress-period defined diversion rate 
c         once a new stress period (implying a new demand TFDR) starts.
c         Info: To express a condition which checks, if the reused or new rate is higher than a previous one,
c               seg(2,n) is stored as temporay parameter divtmp(n) in FMP3FM and compared with the new rate.      
          IF(SEG(2,N).EQ.SNGL(DIVTMP(N))) THEN
            IF(IDIVAR(1,N).GT.0.AND.DIVTMP(N).LT.DIVADD(N))THEN
c           Seg(2,n) also represents a red. rate from prev. stress-period, if reuse-option is chosen, but should 
c           be redefined (overwritten) by a "locked," specified diversion rate (DIVADD) of new stress-period.
            SEG(2,N)=SNGL(DIVADD(N))
            ENDIF                                 
          ENDIF                                   
          ENDDO                                   
      else                                        
C
C1B2----FOR ALL OTHER DEFICIENCY FLAGS
C       skipping FMP3FM in 1st iteration is strictly not necessary if diversion rate doesn't change
c       (as caused by "water-conservation pool"), but found better by standard for faster convergence!            
          istartfl=-1                             
      endif                                       
      ENDIF                                                   
C                                                             
C
C2===== READ FARM WELLS INFORMATION AND CALCULATE NUMBER OF WELLS PER FARM AND MAXIMUM CAPACITIES ============
C
C2A-----READ NUMBER OF FARM WELLS, NUMBER OF FARM WELL PARAMETERS, AND LIST OF NON-PARAMETER FARM WELLS
C
C2A1----READ NUMBER OF FARM WELLS OR FLAG SAYING REUSE FARM WELL DATA).
C       AND NUMBER OF PARAMETERS, AND FLAG SAYING TO PULL FARM WELLS FOR ACCORDING FROM PARENT MODEL FARM
C      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
C        READ(IN,'(A)') LINE
C        LLOC=1
C        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
C        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
C          ITMP=-2
C          NNPFWL=0
C          IF(NPFWL.GT.0) THEN
C            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
C          ELSE
C            NP=0
C          ENDIF
C        ELSE
C          BACKSPACE IN
C          IF(NPFWL.GT.0) THEN
C            READ(IN,*) ITMP,NP
C          ELSE
C            NP=0
C            READ(IN,*) ITMP
C          ENDIF
C        ENDIF
      ITMP2=0
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMP,R,IOUT,IN) 
        IF(INDEX(LINE,' P ').GT.0) THEN
          ITMP2=1
          LLOC=INDEX(LINE,' P ')+3       
          IF(NPFWL.GT.0) THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
          ELSE
            NP=0
          ENDIF
        ELSE
          IF(NPFWL.GT.0) THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
          ELSE
            NP=0
          ENDIF
        ENDIF
      ELSE
        IF(NPFWL.GT.0) THEN
          READ(IN,*) ITMP,NP
        ELSE
          NP=0
          READ(IN,*) ITMP
        ENDIF
      ENDIF
C
C     Calculate some constants.
      IOUTU = IOUT
      IF (IPFWEL.EQ.0) IOUTU=-IOUTU
C
C2A2----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C       IF ITMP=>0, SET NUMBER OF NON-PARAMETER FARM WELLS EQUAL TO ITMP.
      IF(ITMP.EQ.-1) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,6)
         ENDIF
    6    FORMAT(1X,/
     1    1X,'REUSING NON-PARAMETER FARM WELLS FROM LAST STRESS PERIOD')
      ELSEIF(ITMP.GE.0) THEN
         NNPFWL=ITMP
      END IF
C
C2A3----IF THERE ARE NEW NON-PARAMETER FARM WELLS,
C       READ LIST OF ATTRIBUTES ASSOCIATED WITH NON-PARAMETER-FARM-WELLS.
      LABEL=                                                            ! seb LABEL USED BY FMP3WELRD
     + 'WELL NO. LAYER    ROW    COL  F-WELL ID    FARM ID         QMAX'
      IF(IUNITMNW2.GT.0) LABEL=TRIM(LABEL)//'     Linked MNW2 Well Name'
      !LABEL=TRIM(LABEL)//'   ...AUX VARIABLES...'   !seb NOT NEEDED BECAUSE ULSTLB AUTOMATICALLY APPENDES THE CORRECT AUX NAMES
      MXACTFW=IFWLPB-1
C      IF(ITMP.GT.0.OR.ITMP.EQ.-2) THEN
      IF(ITMP.GT.0.OR.ITMP2.EQ.1) THEN
         IF(NNPFWL.GT.MXACTFW) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,99) NNPFWL,MXACTFW
            ENDIF
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE FARM WELLS (',I6,
     1                     ') IS GREATER THAN MXACTFW(',I6,')')
            CALL USTOP(' ')
         END IF
C         IF(ITMP.EQ.-2) THEN
         IF(ITMP.EQ.0.AND.ITMP2.EQ.1) THEN
         IF(LSTCHK(3)) THEN
         CALL ULSTLB(IOUT,
     1   TRIM(LABEL),
     2   FWLAUX,5,NAUX)
         ENDIF
         ENDIF
         IF(ITMP.GT.0) THEN
         CALL FMP3WELRD(NNPFWL,FWELL,1,NFWLVL,MXFWEL,IFWLAL+1,IN,IOUT,
     1    TRIM(LABEL),
     2    FWLAUX,5,NAUX,NCOL,NROW,NLAY,6,6,IPFWEL,IUNITMNW1,
     3    IUNITMNW2,IUNITNWT)
         ENDIF
C         IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
         IF(ILGR.NE.0.AND.IGRID.GT.1.AND.ITMP2.EQ.1) THEN
           IL=1+NNPFWL               !pull parent grid wells for list numbers starting after the child grid non-parameter wells
           CALL FMP3PWELTOCWEL(1,IL,1,IPFWEL,IUNITMNW2)
           NNPFWL=IL-1               !make sure the end of the non-paramter list gets updated by the pulled wells or else it goes back to original NNPFWL
         ENDIF
      END IF
      NFWELS=NNPFWL
C
C2A4----IF THERE ARE ACTIVE FARM WELL PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('QMAX')
      NREAD=NFWLVL-IFWLAL-1
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL FMP3WELPARRD(IN,'FMP',IOUTU,'QMAX',FWELL,NFWLVL,MXFWEL,
     1            NREAD,MXACTFW,NFWELS,6,6,
     2            TRIM(LABEL),
     3            FWLAUX,5,NAUX,IUNITMNW2)                              !seb ADDED IUNITMNW2 TO PASS IN
c         IF(IGRID.GT.1) THEN
c         DO L=1,NFWELS
c         DO M=1,NAUX
c             WRITE(IOUT,*) FWELL(6+M,L)           
c           IF(FWLAUX(M).EQ."LGRGRID") THEN
c              IF((FWELL(6+M,L)).NE.IGRID.AND.
c     1          (FWELL(6+M,L)).NE.0.D0) THEN
c                 CALL FMP3PWELTOCWEL(2,L,IPFWEL,IUNITMNW2)
c              ENDIF
c           ENDIF
c         ENDDO
c         ENDDO
c         ENDIF
C         IF(IGRID.GT.1) CALL FMP3PWELTOCWEL(2,0,IPFWEL,IUNITMNW2) 
   30    CONTINUE
      END IF
C
C2B-----PRINT NUMBER OF FARM WELLS IN CURRENT STRESS PERIOD.
      IF(LSTCHK(3)) THEN
        WRITE (IOUT,100) NFWELS
      ENDIF
  100 FORMAT(1X,/1X,'NUMBER OF ACTIVE FARM WELLS: ',I8)
C seb MOVED C2C TO AFTER IFID READ
C seb MOVED IALLOTGW READ AND READING OF IFID TO BEFORE MNW2/MNW1 PROCESSING
ccrth
      IF(IALLOTGW.EQ.2) THEN
        CALL FMP3DPLSTRD(ALLOTGW,2,NFARMS,IN,IOUT,2,2,2)
      ENDIF
      IF(IALLOTGW.EQ.-1.AND.FMPDAT(1)%IALLOTGW.EQ.2) THEN
         ALLOTGW=FMPDAT(1)%ALLOTGW
      ENDIF
      
ccrth
ccrth ===== IF CONSTANT FID THEN UPDATE QMAXF ==============rth
      IF(IFRMFL.EQ.1) THEN
C
      DO NF=1,NFARMS
      QMAXF(NF) =0.0                                                    !seb INITIALIZE SUMS
      QMAXFW(NF)=0.0
      NWPERF(NF)=0.0
      IF(.NOT. LFID(NF)) CYCLE                                         !seb SKIP INACTIVE FARMS
      IF(IFA(NF).NE.0) THEN
      QSUMF=0.D0
      NWPF=0         
         DO L=1,NFWELS
         FID=IDINT(FWELL(5,L))
         IF(FID.EQ.NF) THEN         
         IF(IDINT(FWELL(1,L)).GT.0) THEN
           IF(IBOUND(IDINT(FWELL(3,L)),IDINT(FWELL(2,L)),               !--> ... FOR LGR APPLICATIONS DON'T ZERO OUT FWELL(6,L) AS DONE ABOVE: STILL NEEDED FOR CHILD MODEL FARM WELLS ... SO JUST BYPASS THE QMAX
     1                                 IDINT(FWELL(1,L))).EQ.0) THEN
             IF(LSTCHK(2)) THEN
               WRITE(IOUT,992) IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
             ENDIF
  992        FORMAT(1X,/1X,'THE SINGLE LAYER FMP- OR MNW- WELL IN ROW ',
     1       I6,' AND ','COLUMN ',I6,' IS LOCATED IN AN INACTIVE CELL ',
     2       '(IBOUND=0) AND HAS BEEN EXCLUDED FROM THE TOTAL MAXIMUM ',
     3       'CAPACITY PER FARM')
           GOTO 993
           ENDIF
         ENDIF         
         QSUMF=QSUMF+FWELL(6,L)
         NWPF=NWPF+1
         ENDIF
 993     ENDDO
      QMAXF(NF)=QSUMF 
      QMAXFW(NF)=QSUMF                                                  !Added to store original QMAXF for each Farm --rth
      NWPERF(NF)=NWPF
      ENDIF
      ENDDO
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,101) 
      ENDIF
  101 FORMAT(1X,/1X,
     1'FARM-ID  NUMBER OF WELLS PER FARM  TOTAL MAX.CAPACITY PER FARM')
      Do NF=1,NFARMS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,102) NF, NWPERF(NF), QMAXFW(NF)
      ENDIF
  102 FORMAT(4X,I4,20X,I6,9X,F20.4)
      ENDDO
      !
      !SET UP AUX VARIABLES IF THEY ARE REQUESTED
      IF(NAUX.GT.0) THEN
        DO L=1,NFWELS
          DO N=1,NAUX                                                   !IF AUX VARIABLES ARE IN USE COPY QMAX TO THEIR APPROPIATE LOCATION
            IF(AUXV(N,L).EQ.1) THEN                                     !AUXIXILARY FLAG TO HOLD Q VALUES IN FWELL 
            FWELL(6+N,L)=FWELL(6,L)
            ENDIF
          END DO
        END DO
      END IF
ccrth ===== READ new FID's (WBS) for this stress period ==============rth
      ELSEIF(IFRMFL.EQ.2) THEN
      LFID=.FALSE.                                                      ! seb RESET LFID BECAUSE IT WILL BE REINITIALIZED DEPENDING ON FARMS THAT ARE READ IN
C3rth===== READ FARM-ID 2D-ARRAY AND LIST OF FARM-SPECIFIC EFFICIENCY =====rth
C
C3A-----READ FARM-ID 2D-ARRAY
C       NOTE: The FARM ID array comprises the entire respective area,
C             disregarding small scale details like no-flow or constant head cells.
C             Net-Recharge will not be calculated for such cells! 
      IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
CC3A2---PULL CHILD FID FROM PARENT MODEL        
          CALL FMP3PRTOCH(1)
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DINT(IFID,ANAME(4),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSE
        CALL U2DINT(IFID,ANAME(4),NROW,NCOL,0,IN,IOUT)
        IF(ILGR.NE.0.AND.IGRID.EQ.1) IFIDOLD=IFID
      ENDIF
C seb BUILD NEW INDEX OF ACTIVE FARMS FOR CURRENT STRESS PERIOD
      DO CONCURRENT (JJ=1:NCOL,II=1:NROW,IFID(JJ,II).GT.0)
        LFID(IFID(JJ,II))=.TRUE.
      END DO
C seb BUILD NEW INDEX OF THE LOCATION OF ALL FARMS  
      FMLOC%Count=0                                                    !RESET ALL FARM CELL COUNTS
      DO NF=1, NFARMS
        N=COUNT(NF.EQ.IFID)                                             !COUNT THE NUMBER OF CELLS THAT CONTAIN FARM NF
        FMLOC(NF)%Count=N                                               !STORE FARN COUNT IN LOGICAL VARIABLE
        IF (N>0) THEN
          IF(ALLOCATED(FMLOC(NF)%RC)) DEALLOCATE(FMLOC(NF)%RC)          !THE CELL COUNT FOR FARM CAN CHANGE FOR SP>1 SO REALLOCATE TO EXACT SIZE
          ALLOCATE(FMLOC(NF)%RC(2,N))
          II=0                                                          !COUNTER FOR NUMBER OR RC READS
          DO IR=1,NROW                                                  !SEARCH FOR FARM NF IN MODEL AND STORE ITS ROW/COL LOCATION
          DO IC=1,NCOL
            IF(IFID(IC,IR).EQ.NF) THEN
             II=II+1
             FMLOC(NF)%RC(1,II)=IR
             FMLOC(NF)%RC(2,II)=IC
            END IF
          END DO
          END DO
!         IF (II.NE.N) STOP 'FARM COUNT ERROR II.NE.N'                    ! seb debug check, remove with final version
        END IF
      END DO
C seb MOVED NEXT BLOCK OF CODE FROM BEING BEFORE IFID READ
C
C2C-----CALCULATE SUM OF MAXIMUM CAPACITIES AND NUMBER OF WELLS PER FARM
C       Note: Basically QMAXF and NWPERF could be defined for the entire simulation,
C             but in order to keep the option to enter non-parameter wells for each
C             stress period, the algorithm computing QMAXF and NWPERF is located here!
C       CHECK FOR EXCLUSION (MNW-LINK CAN'T BE COMBINED WITH PRIOR APPROPRIATION)
C       CHECK FOR FARM-WELL ID ERROR FOR MULTI-NODE WELLS, AND
c       DISABLE WELL FOR IBOUND=0 BY SETTING MAX. CAPACITY TO ZERO.
C       SET A ZERO LAYER FOR A MULTIAQUIFER MNW-WELL TO LAYER ONE (TO AVOID ARRAY ERROR IN IBOUND)
C      !
      DO L=1,NFWELS
         WID=IDINT(FWELL(4,L))
         FID=IDINT(FWELL(5,L))                                          
         IF(.NOT. LFID(FID)) FWELL(6,L)=0D0                             !seb ZERO OUT FARM WELLS THAT ARE ASSOCIATED WITH INACTIVE FARMS
         !
         IF( (IUNITMNW1.GT.0.OR.IUNITMNW2.GT.0)                         !CHECK IF MNW1 or MNW2 IS ACTIVE  seb
     1      .AND.WID.LT.0.AND.IALLOTSW.GT.1)THEN          !CHECK IF WELL IS LNKED TO MNW AND ALLOTMENT
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,990) IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
              ENDIF
  990         FORMAT(1X,/1X,'THE LINK OF WELL IN ROW ',I6,' AND ',
     1        'COLUMN ',I6,' TO THE MNW-PACKAGE',/,
     2        ' CANNOT BE COMBINED WITH IALLOTSW>1')
              CALL USTOP(' ')
         ENDIF
         IF(IUNITMNW1.GT.0.AND.IDINT(FWELL(1,L)).EQ.0.AND.   !SHOULD THIS CHECK STILL BE HERE BECAUSE THE NEXT LINE CHECKS IF THE WID>0 SCOTT
     1      IDINT(FWELL(4,L)).GE.0) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,991) IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
              ENDIF
  991         FORMAT(1X,/1X,'THE MULTI-AQUIFER WELL Ver. 1 IN ROW ',I6,
     1        ' AND ','COLUMN ',I6,' MUST HAVE A NEGATIVE FARM-WELL ID')
              CALL USTOP(' ')
         ENDIF
C         IF(IDINT(FWELL(1,L)).GT.0) THEN                               !--> FOR NON-LGR APPLICATIONS YOU CAN DO THAT BUT ...
C           IF(IBOUND(IDINT(FWELL(3,L)),IDINT(FWELL(2,L)),
C     1                                 IDINT(FWELL(1,L))).EQ.0) THEN
C             FWELL(6,L)=0D0
C               WRITE(IOUT,992) IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
C  992        FORMAT(1X,/1X,'THE SINGLE LAYER FMP- OR MNW- WELL IN ROW ',
C     1       I6,' AND ','COLUMN ',I6,' IS LOCATED IN AN INACTIVE CELL ',
C     2       '(IBOUND=0) AND HAS BEEN DISABLED BY SETTING QMAX TO ZERO')
C           ENDIF
C         ENDIF
         DO N=1,NAUX                                                    !IF AUX VARIABLES ARE IN USE COPY QMAX TO THEIR APPROPIATE LOCATION
           IF(AUXV(N,L).EQ.1) THEN                                      !AUXIXILARY FLAG TO HOLD Q VALUES IN FWELL 
           FWELL(6+N,L)=FWELL(6,L)
           ENDIF
         ENDDO
      ENDDO
C
      DO NF=1,NFARMS
      QMAXF(NF) =0.0                                                    !seb INITIALIZE SUMS
      QMAXFW(NF)=0.0
      NWPERF(NF)=0.0
      IF(.NOT. LFID(NF)) CYCLE                                          !seb SKIP INACTIVE FARMS
      IF(IFA(NF).NE.0) THEN
      QSUMF=0.D0
      NWPF=0         
         DO L=1,NFWELS
         FID=IDINT(FWELL(5,L))
         IF(FID.EQ.NF) THEN         
         IF(IDINT(FWELL(1,L)).GT.0) THEN
           IF(IBOUND(IDINT(FWELL(3,L)),IDINT(FWELL(2,L)),               !--> ... FOR LGR APPLICATIONS DON'T ZERO OUT FWELL(6,L) AS DONE ABOVE: STILL NEEDED FOR CHILD MODEL FARM WELLS ... SO JUST BYPASS THE QMAX
     1                                 IDINT(FWELL(1,L))).EQ.0) THEN
             IF(LSTCHK(2)) THEN
               WRITE(IOUT,992) IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
             ENDIF
           CYCLE
           ENDIF
         ENDIF         
         QSUMF=QSUMF+FWELL(6,L)
         NWPF=NWPF+1
         ENDIF
         ENDDO
      QMAXF(NF)=QSUMF 
      QMAXFW(NF)=QSUMF                                                  !Added to store original QMAXF for each Farm --rth
      NWPERF(NF)=NWPF
      ENDIF
      ENDDO
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,101) 
      ENDIF
      Do NF=1,NFARMS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,102) NF, NWPERF(NF), QMAXFW(NF)
      ENDIF
      ENDDO
C
C seb END OF MOVED BLOCK OF CODE
ccrth
      IF(IALLOTGW.NE.0)THEN
C seb added WHERE CONSTRUCT TO REPLACE DO LOOPS, MINOR SPEED IMPROVEMENT, USE WHAT IS EASIER TO READ DO OR WHERE
!       WHERE(LFID .AND. QMAXF.GT.ALLOTGW(2,:))                          !ASSIGN VALUE WHERE LFID IS TRUE
!         QMAXF=ALLOTGW(2,:)                                             !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!       END WHERE  
!       WHERE(.NOT. LFID)                                    !ASSIGN VALUE WHERE LFID IS TRUE
!         QMAXF=0d0                                          !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!       END WHERE    
!       
!       WHERE(LFID .AND. QMAXFW.GT.0D0)
!         QFRACF=ALLOTGW(2,:)/QMAXFW
!       ELSEWHERE
!         QFRACF=0D0                                                     !Set Fraction of reduction to zero if original capacity was zero
!       END WHERE
       DO NF=1,NFARMS
        IF(LFID(NF))THEN
            IF(ALLOTGW(2,NF)>QMAXFW(NF)) ALLOTGW(2,NF)=QMAXFW(NF)       !ALLOTMENTS CAN NOT EXCEDE CAPACITY, NOTE AT THIS POINT QMAXFW = QMAXF 
            IF(QMAXFW(NF)>ALLOTGW(2,NF)) QMAXF(NF)=ALLOTGW(2,NF)        !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
         IF(QMAXFW(NF).GT.0)THEN 
             QFRACF(NF)=ALLOTGW(2,NF)/QMAXFW(NF)
         ELSE
             QFRACF(NF)=0D0                                                 !Set Fraction of reduction to zero if original capacity was zero
         ENDIF
        ELSE
            QMAXF(NF)=0D0
        ENDIF
       ENDDO
       !
       !REDUCE EACH FARM WELL MAX RATE TO SCALE EQUALLY TO MEET ALLOTTED TOTAL
       DO I=1,NFWELS
           FID=IDINT(FWELL(5,I))
           FWELL(6,I)=FWELL(6,I)* QFRACF(FID)
       END DO
cc
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,103) 
      ENDIF
  103 FORMAT(1X,/1X,
     1'FARM-ID,  NUMBER OF WELLS PER FARM, ORIGINAL PUMP CAPACIFTY, ',
     2'GROUNDWATER-ALLOTMENT,  FRACTION OF PUMPING CAPACITY PER FARM')
      IF(LSTCHK(3)) THEN                                                !seb moved IF(LSTCHK(3)) to outside of DO loop to improve speed
      Do NF=1,NFARMS
       WRITE(IOUT,104) NF,NWPERF(NF),QMAXFW(NF),ALLOTGW(2,NF),QFRACF(NF)
  104 FORMAT(4X,I4,20X,I6,9X,3F20.4)
      ENDDO
      ENDIF
      ENDIF            !end of groundwater allotment block when active
Cccrth
      ENDIF            !end block for reading new FID's each Stress Period --rth
C
C-----IF MNW2 LINK ACTIVE BUILD REFERANCE INDEX FROM LINKED WELL NAMES TO FARM WELLS ONLY DO THIS ON FIRST ITERATION seb
!***GWF2MNW27RP SHOULD APPEAR IN MODFLOW MAIN BEFORE FMP3RP OR INDEX WILL FAIL (THIS WAS THE DESIGN WHEN CODE WAS WRITTEN 7/1/2013)***
      IF(IUNITMNW2.GT.0 .AND. NFWELS.GT.0) THEN
        MNW2LOC=0
        IF(LSTCHK(3)) WRITE(IOUT,'(/A)')
     +                              "FMP-MNW2 LINK INDEX IS BEING BUILT"
        DO I=1,NFWELS                                                   !SEARCH ACROSS FARM WELLS TO SEE IF ANY ARE LINKED TO MNW2, IF TRUE FIND LOCATION IN VARIABLE WELLID FROM MNW2
          FID=IDINT(FWELL(5,I))
          WID=IDINT(FWELL(4,I))
          IF(WID.GE.0 .OR. .NOT.LFID(FID)) CYCLE                        !SKIP FARM WELLS THAT ARE NOT LINKED TO MNW2 AND FARM WELLS ASSOCIATED WITH INACTIVE FARMS
          DO J=1,MNWMAX
            IF (MNW2NAM(I)==WELLID(J)) THEN                             !IF TRUE FOUND LOCATION OF MNW2 WELL THAT FWELL IS LINKED TOO
              MNW2LOC(I)=J                                              !MNW2LOC(I) TRANSLATES FARM WELL LOCATION TO MNW2 WELL LOCATION
              FIRSTNODE=IDINT( MNW2(4,J) )
              LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )
              QMNW2DES=-FWELL(6,I)
              MNW2(5,J)=0D0!QMNW2DES                                     ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
              MNWNOD(4,FIRSTNODE:LASTNODE)=0D0
              !MNWNOD(4,LASTNODE)=QMNW2DES
              PUMPCAP=IDINT(MNW2(22,J))
              IF(PUMPCAP.GT.0) THEN
               IF(FWELL(6,I).EQ.0.D0) THEN                                !IF QDES IS 0D0 FOR THIS STRESS PERIOD, DO NOT APPLY PUMP CAPACITY RESTRAINTS (Should )
                  MNW2(25,J)=0
                  MNW2(27,J)=0                                          !INITIALIZE CAPFLAG2
               ELSE
                 MNW2(25,J)=1
                 IF (CAPTABLE(J,INT(PUMPCAP)+2,2).LE.0.D0) THEN              !ONLY SET QDES AT UPPER END OF CAPTABLE IF NOT SET ALREADY
                   CAPTABLE(J,INT(PUMPCAP)+2,2)=FWELL(6,I)
                 END IF
                 CAPMULT=MNW2(24,J)
                 IF(LSTCHK(3)) THEN
                   WRITE(IOUT,'(2A)') 'FMP-MNW2 LINKED WELL: ',WELLID(J)
                   WRITE(IOUT,1114) MNW2(23,J)
 1114 FORMAT('REFERENCE HEAD FOR CALCULATING LIFT = ', 1PE12.4)
                   WRITE(IOUT,1113) CAPMULT
 1113 FORMAT('PUMP CAPACITY MULTIPLIER = ', 1PE12.4)
                   IF(MNWPRNT.GT.0) WRITE(IOUT,1112) MNW2(28,J)
 1112 FORMAT('HWTOL = ', 1PE12.4)
                   IF(MNWPRNT.GT.1) WRITE(IOUT,1115) 
                 ENDIF
 1115 FORMAT(5X,'(NOTE: SOLUTION MAY BE SENSITIVE TO VALUE OF HWTOL;',
     +           ' ADJUST VALUE IF SOLUTION FAILS TO CONVERGE.)')
C   ZERO CAPACITY USE FLAG IF CAPMULT=0
                 IF( CAPMULT.EQ.0.D0) THEN
                  MNW2(25,J)=0
                  MNW2(27,J)=0
                 END IF
                 IF(LSTCHK(3)) THEN
                   WRITE(IOUT,*) 
                   WRITE(IOUT,*) 'WELL CAPACITY TABLE'
                   WRITE(IOUT,*) ' LIFT     DISCHARGE'
                 DO IT=1,INT(PUMPCAP)+2
                     WRITE(IOUT,'(1X,1PG10.5,G10.4)')
     +                  CAPTABLE(J,IT,1), CAPTABLE(J,IT,2)
                 END DO
                 ENDIF
              END IF
             END IF

              FWELL(1,I)=DBLE( MNWNOD(1,FIRSTNODE) )                    !OVERWRITE FWELL LAYER, ROW, AND COL FROM FIRST NODE IN MNW2
              FWELL(2,I)=DBLE( MNWNOD(2,FIRSTNODE) )
              FWELL(3,I)=DBLE( MNWNOD(3,FIRSTNODE) )
              FID=IDINT(FWELL(5,I))
              EXIT                                                      !MWN2 WELL NAMNE FOUND, EXIT INTERNAL LOOP TO PREVENT SEARCHING ADDITIONAL NAMES
            END IF
            IF(J.EQ.MNWMAX)THEN                                         !WHEN TRUE MNW2NAM(J) WAS NOT FOUND IN WELLID(:)
             IF(LSTCHK(3)) WRITE(*,'(A,I5,2A,/A)')
     +         'ERROR FMP3FM: CAN NOT ESTABLISH MNW2 LINK FOR FMP WELL',
     +         IDINT(FWELL(4,I)),' WITH MNW2NAM= ',TRIM(MNW2NAM(I)),
     +         ' NAME DOES NOT EXIST IN MNW2 "WELLID" CHECK MNW2 INPUT'
             CALL USTOP('FAILED FMP LINK TO MNW2 WELL '//MNW2NAM(I))
            END IF
          END DO
      END DO
      IF(LSTCHK(3)) WRITE(IOUT,'(A/)') "FMP-MNW2 LINK INDEX COMPLETE"
      END IF
      !
      IF(IUNITMNW2.GT.0) THEN                                           !seb ADDED CHECK FOR CORRECT LINKAGE TO MNW2 WELLS THAT ARE ASSOCIATED WITH A FARM WELL
        DO I=1,NFWELS
         WID=IDINT(FWELL(4,I))
         IF(WID.GE.0) CYCLE
         FID=IDINT(FWELL(5,I))
         IF (IDINT(MNW2(1,MNW2LOC(I))).EQ.0.AND.LFID(FID)) THEN         !MNW2 WELL SHOULD TURNED ON FOR CURRENT STRESS PERIOD IF ITS LINKED TO FMP. ADD WELLID 0.0 TO ITEM 4A 
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,'(A,I5,A,I5,3A)')
     +        'FARM ID ',FID,' WITH FARM WELL ', WID, 
     +        ' IS LINKED TO MNW2 WELL THAT IS MISSING FROM MNW2 INPUT',
     +        ' FOR CURRENT SP, ADD ',TRIM(WELLID(MNW2LOC(I)))
           ENDIF
             WRITE(*,'(A,I5,A,I5,3A)')
     +        'FARM ID ',FID,' WITH FARM WELL ', WID, 
     +        ' IS LINKED TO MNW2 WELL THAT IS MISSING FROM MNW2 INPUT',
     +        ' FOR CURRENT SP, ADD ',TRIM(WELLID(MNW2LOC(I)))
          CALL USTOP('')
         ENDIF
         IF (IDINT(MNW2(1,MNW2LOC(I))).NE.0.AND..NOT.LFID(FID)) THEN    !WARN USER THAT AN FMP LINK WAS SPECIFED WITH AN FARM THAT IS NOT IN USE, SO NO LINK IS MADE AND MNW2 OPERATES NORMALLY AS IF THE LINK WAS NOT THERE
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,'(2(A,I5),3A,I5,A,/A)')
     +      'WARNING FOR SP ',KPER,',  FARM WELL ', WID,
     +      ' IS LINKED TO MNW2 WELL ',TRIM(WELLID(MNW2LOC(I))),
     +      ' BUT THE FARM ID ',FID,' IS TURNED OFF FOR CURRENT SP',
     +      ' NO LINK IS MADE AND MNW2 OPERATES NORMALLY WITHOUT LINK' 
           ENDIF
         END IF
        END DO
      END IF
C
C3===== READ LIST OF FARM-SPECIFIC EFFICIENCY, AND CREATE 2D-ARRAY OF EFFICIENCY ============================
C       (FARM-ID ALREADY READ IN FMP3RQ)
C
C3A-----READ FARM-SPECIFIC EFFICIENCY LIST & CREATE 2D-EFFICIENCY ARRAY
      IF(IEFFL.EQ.2) THEN
C        DO NF=1,NFARMS
C        DO NC=1,1+NCROPS
C           EFF(NC,NF)=0D0
C        ENDDO
C        ENDDO
       EFF=0D0
       CALL FMP3DPLSTRD(EFF,NCROPS+3,NFARMS,IN,IOUT,2,NCROPS+1,NCROPS+1)
      ENDIF
      IF(IEFFL.EQ.-1.AND.FMPDAT(1)%IEFFL.EQ.2) THEN
         EFF=FMPDAT(1)%EFF
      ENDIF      
C
C3B-----CREATE 2D-EFFICIENCY ARRAY IF IEFFL=2 OR IEBFL=2 OR IROTFL=-1
C       (IEBFL=2: cell-by-cell efficiency may vary over time step)
      IF(IEFFL.EQ.2.OR.(IEFFL.EQ.-1.AND.FMPDAT(1)%IEFFL.EQ.2).OR.
     1  (IEBFL.EQ.0.OR.IEBFL.EQ.2).OR.IROTFL.EQ.-1.or.IFRMFL.eq.2)THEN  !NOT NECESSARY FOR (IEFFL=1 AND IEBFL=1) --> ALREADY IN RQ
      DO NF=1,NFARMS                                                    !FOR IEFFL=2 AND IEBFL=0 OR 2 NECESSARY IN ORDER TO RESET EFF. FROM
      IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                              !CALC. EFF. TO SPECIFIED EFFICIENCIES AT BEGINNING OF STRESS PERIOD!  seb LFID: SKIP FARMS THAT ARE NOT IN USE
      DO NC=2,1+NCROPS                                                  !NOT FOR NCROPS+3 AND NCROPS+4 (NONZERO CALCULATED VALUES)
         DO IR=1,NROW                                                   !FOR IROTFL=-1 NECESSARY IN ORDER TO RE-DISTRIBUTE EFFICIENCIES TO
         DO IC=1,NCOL                                                   !VARYING (ROTATING) CROP 2D-ARRAY. Added check for IFRMFL=2 rth
            IF(NC.GT.2.AND.EFF(NC,NF).EQ.0D0) THEN
              IF(IFID(IC,IR).EQ.INT(EFF(1,NF))) THEN
              EF2D(IC,IR)=EFF(2,NF)
              ENDIF
            ELSE
              IF(IFID(IC,IR).EQ.INT(EFF(1,NF)).AND.
     1        ICID(IC,IR).EQ.NC-1) THEN
              EF2D(IC,IR)=EFF(NC,NF)
              ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDDO
      ENDIF
      ENDDO
      ENDIF
C
c     ZERO OUT FARM ID IN PARENT MODEL WHERE CHILD MODEL EXISTS WITH ITS OWN SFR, BUT
C     RESET FARM ID THAT WAS PREVIOUSLY ZEROED OUT DURING LGRITER>1 IN FM ROUTINE WHERE CHILD MODEL EXISTS WITHOUT ITS OWN SFR, SO THAT
C     FARM CANAL AND DRAIN REACH IDs IN THE PARENT CAN BE ASSIGNED FOR CHILD MODELS WITH AN SFR REPRESENTED SOLELY BY THE PARENT.
      IF(ILGR.NE.0.AND.IGRID.EQ.1) THEN
        DO IR=1,NROW
        DO IC=1,NCOL
           DO N=2,NGRIDS
             IF(IR.GE.LGRDAT(N)%NPRBEG.AND.IR.LE.LGRDAT(N)%NPREND.AND.
     1          IC.GE.LGRDAT(N)%NPCBEG.AND.IC.LE.LGRDAT(N)%NPCEND) THEN
                IF(LGRDAT(N)%ISFRGRID.EQ.N) THEN
                  IF(IFID(IC,IR).GT.0) LFID(IFID(IC,IR))=.FALSE.        !DISABLE POINTERS FOR FARM ID'S THE ARE COINCIDENT WITH CHILD MODELS --RTH
                  IFID(IC,IR)=0
                ELSE
                  IFID(IC,IR)=IFIDOLD(IC,IR)
                  LFID(IFID(IC,IR))=.TRUE.                              ! REASSIGN POINTER FOR FARM ID'S--RTH
                ENDIF
             ENDIF
           ENDDO
        ENDDO
        ENDDO
      ENDIF
c
C4===== READ CROP-ID 2D-ARRAY AND LISTS OF CROP-SPECIFIC ATTRIBUTES, AND CREATE 2D-ARRAYS OF ATTRIBUTES ======
C
C4A-----READ CROP-ID 2D-ARRAY (IROTFL=-1: CROP ROTATION FROM ONE STRESS PERIOD TO ANOTHER)
C       NOTE: The CROP ID array comprises the entire respective area,
C             disregarding small scale details like no-flow or constant head cells.
C             Net-Recharge will not be calculated for such cells!
      IF(IROTFL.EQ.-1) THEN                               
      IF(ILGR.NE.0.AND.ILGR.NE.0.AND.IGRID.GT.1) THEN
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'P') THEN
C4A2----PULL CHILD CID FROM PARENT MODEL        
          CALL FMP3PRTOCH(3)
        ELSE
          BACKSPACE IN !REWIND TO BEGINNING OF LINE
          CALL U2DINT(ICID,ANAME(2),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ELSE
        CALL U2DINT(ICID,ANAME(2),NROW,NCOL,0,IN,IOUT)
      ENDIF
      ENDIF
C
C4B-----READ DEPTH OF ROOTZONE FOR EACH STRESS PERIOD & CALCULATE 2D-ARRAY OF ROOTZONE-DEPTHS
      IF(IRTFL.EQ.2) CALL FMP3LSTRD(ROOT,2,NCROPS,IN,IOUT,2,2,2)
      IF(IRTFL.EQ.-1.AND.FMPDAT(1)%IRTFL.EQ.2) THEN
         ROOT=FMPDAT(1)%ROOT
      ENDIF
      IF(IRTFL.EQ.2.OR.(IRTFL.EQ.-1.AND.FMPDAT(1)%IRTFL.EQ.2).OR.
     1  (IRTFL.EQ.1.AND.IROTFL.EQ.-1)) THEN
      DO IR=1,NROW
      DO IC=1,NCOL
        DO NC=1,NCROPS      
          IF(ICA(NC).NE.0) THEN
            IF(ICID(IC,IR).EQ.INT(ROOT(1,NC))) THEN
              IF(ROOT(2,NC).LE.0.D0) THEN
                IF(LSTCHK(1)) THEN
                  WRITE(IOUT,*)
     1          "INPUT ERROR: ROOT ZONE DEPTH MUST BE A POSITIVE NUMBER"
                ENDIF
                STOP
              ENDIF
              RT2D(IC,IR)=ROOT(2,NC)
            ENDIF
          ENDIF
        ENDDO
        IF(ICID(IC,IR).EQ.-1) THEN
           RT2D(IC,IR)=1.D-3                                            !FOR ICCFL=1, TERM TRZ/RRZ SHOULD NOT BE "DEVIDE BY ZERO"
        ENDIF
      ENDDO
      ENDDO
      ENDIF
C
C4C-----READ LIST OF CONSUMPTIVE USE FLUXES OF CROP COEFFICIENTS
C       AND CREATE 2D-CU-FLOWRATE-ARRAY FOR EACH STRESS PERIOD
C       Note: the 2D-CU array does not ignore small scale 'no-flow and constant head details,'
C             and net-recharge will be calculated for such cells.
      IF(ICUFL.LT.3.AND.ICUFL.GT.-2) THEN
        CALL FMP3LSTRD(CU,3,NCROPS,IN,IOUT,2,2,3)                       !IF ETC OR KC VALUES ARE TO BE READ
      ENDIF
      IF(ICUFL.EQ.-2.AND.FMPDAT(1)%ICUFL.LT.3) THEN
        CU=FMPDAT(1)%CU
      ENDIF
      IF((ICUFL.LT.3.AND.ICUFL.GT.0).OR.(ICUFL.EQ.-2.AND.
     1    FMPDAT(1)%ICUFL.LT.3.AND.FMPDAT(1)%ICUFL.GT.0) ) THEN         !IF ETC VALUES ARE TO BE READ
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN                                             !(ICUFL=2: NO FALLOW CELLS IN ARRAY;
         DO IR=1,NROW                                                   !(ICUFL=1: FALLOW CELLS IN ARRAY, THEREFORE ETR ARRAY READ)
         DO IC=1,NCOL
            IF(ICID(IC,IR).EQ.INT(CU(1,NC))) THEN      
            CU2D(IC,IR)=CU(2,NC)*DBLE(DELR(IC)*DELC(IR))
            ENDIF
         ENDDO
         ENDDO
      ENDIF
      ENDDO
      ENDIF
C
      IF(ICUFL.EQ.1.OR.ICUFL.EQ.-1) THEN                                !IF FALLOW CELLS OR KC-BASED ETC CALCULATION REQUIRE AN ARRAY OF REFERENCE ET TO BE READ
        CALL U2DDP(ETR,ANAME(3),NROW,NCOL,0,IN,IOUT)
      ENDIF
      IF(ICUFL.EQ.-2.AND.
     1  (FMPDAT(1)%ICUFL.EQ.1.OR.FMPDAT(1)%ICUFL.EQ.-1)) THEN
        CALL FMP3PRTOCH_BILINEAR(FMPDAT(1)%ETR,ETR)
      ENDIF
      IF((ICUFL.EQ.1.OR.ICUFL.EQ.-1).OR.(ICUFL.EQ.-2.AND.
     1   (FMPDAT(1)%ICUFL.EQ.1.OR.FMPDAT(1)%ICUFL.EQ.-1))) THEN
      DO IR=1,NROW
      DO IC=1,NCOL
         IF(ICID(IC,IR).EQ.-1) THEN                                     !ASSIGN REF ET TO ANY FALLOW CELL (FOR BOTH OPTIONS ICUFL=1 AND ICUFL=-1)
         CU2D(IC,IR)=ETR(IC,IR)*DBLE(DELR(IC)*DELC(IR))
         ENDIF
         IF(ICUFL.EQ.-1.OR.(ICUFL.EQ.-2.AND.FMPDAT(1)%ICUFL.EQ.-1)) THEN
         DO NC=1,NCROPS
         IF(ICA(NC).NE.0) THEN
            IF(ICID(IC,IR).EQ.INT(CU(1,NC))) THEN
            CU2D(IC,IR)=CU(2,NC)*ETR(IC,IR)*DBLE(DELR(IC)*DELC(IR))     !HERE CU(2,NC) EQUALS KC
            ENDIF
         ENDIF
         ENDDO
         ENDIF
      ENDDO
      ENDDO
      ENDIF
C
C4D-----READ TRANSPIRATORY AND EVAPORATIVE FRACTIONS OF CROP CONSUMPTIVE USE
      IF(IFTEFL.EQ.2) CALL FMP3DPLSTRD(FTE,4,NCROPS,IN,IOUT,2,4,4)
      IF(IFTEFL.EQ.-1.AND.FMPDAT(1)%IFTEFL.EQ.2) FTE=FMPDAT(1)%FTE
      IF(IFTEFL.EQ.2.OR.(IFTEFL.EQ.-1.AND.FMPDAT(1)%IFTEFL.EQ.2)) THEN
      IF(LSTCHK(3)) THEN
        write(iout,105)
      ENDIF
  105 format(1x,/
     1'Transpiratory and Evaporative Fractions of Crop Consumptive Use',
     1/,'    % Transpiration % Evap. for Precip. % Evap. for Irrig.')
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
      IF(LSTCHK(3)) THEN
        write(IOUT,106)
     1 IDINT(FTE(1,NC)),FTE(2,NC),FTE(3,NC),FTE(4,NC)
      ENDIF
  106 format(1X,I4,3F15.3)
      ENDIF
      ENDDO
      ENDIF
C
C4E-----READ FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF FROM PRECIPITATION AND FROM IRRIGATION
      IF(IIESWFL.EQ.2) CALL FMP3DPLSTRD(FIESW,3,NCROPS,IN,IOUT,2,3,3)
      IF(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.2) FIESW=FMPDAT(1)%FIESW
      IF(IIESWFL.EQ.2.OR.(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.2))THEN
      IF(LSTCHK(3)) THEN
        write(iout,107)
      ENDIF
  107 format(1x,/
     1'FRACTIONS OF IN-EFFICIENT LOSSES TO SW-RUNOFF FROM ',/,
     1'     PRECIPITATION:  IRRIGATION:')
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
      IF(LSTCHK(3)) THEN
        write(IOUT,108) IDINT(FIESW(1,NC)),FIESW(2,NC),FIESW(3,NC)
      ENDIF
  108 format(1X,I4,2F15.3)
      ENDIF
      ENDDO
      ENDIF
C
C5===== READ PRECIPITATION CONSTANT OR 2D-ARRAY FOR EACH STRESS PERIOD & ====================================
C       CREATE 2D-ARRAY FOR PRECIPITATION FLOW RATE AND
C       FOR EVAPORATIVE AND TRANSPIRATORY PART OF PRECIPITATION PER CELL
      IF(IPFL.EQ.2) THEN
      CALL U2DDP(PFLX,ANAME(1),NROW,NCOL,0,IN,IOUT)
      ENDIF
      IF(IPFL.EQ.-1.AND.(FMPDAT(1)%IPFL.EQ.2)) THEN
         CALL FMP3PRTOCH_BILINEAR(FMPDAT(1)%PFLX,PFLX)
      ENDIF
      IF(IPFL.EQ.2.OR.(IPFL.EQ.-1.AND.(FMPDAT(1)%IPFL.EQ.2))) THEN
      DO IR=1,NROW
      DO IC=1,NCOL
         PFLR(IC,IR)=PFLX(IC,IR)*DBLE(DELR(IC)*DELC(IR))
         IF(ICID(IC,IR).GT.0) THEN
         TPPOT(IC,IR)=PFLR(IC,IR)*FTE(2,ICID(IC,IR))
         EPPOT(IC,IR)=PFLR(IC,IR)*FTE(3,ICID(IC,IR))
         ENDIF
      ENDDO
      ENDDO
      ENDIF
C
C6===== FOR ACREAGE-OPTIMIZATION, READ CROP-BENEFITS LIST AND WATERCOST COEFFICIENTS ========================
C
      IF(IDEFFL.GT.0) THEN
C6A-----FOR EACH CROP READ LIST OF CROP-BENEFITS FOR EACH STRESS PERIOD:
C       CROPBEN(1,NC): DUMMY VARIABLE FOR NUMBER OF CROP TYPE
C       CROPBEN(2,NC): SLOPE OF WATERPRODUCTION FUNCTION (ET[L],Y[MASS/AREA])
C       CROPBEN(3,NC): INTERCEPT OF WPF: SHOULD BE ZERO FOR 2 REASONS:
C                                        1. NEGLIGIBLY SMALL COMPARED TO SLOPE OF ET[L],Y[MASS/AREA]
C                                        2. FOR SMALL ET PER DAILY TIME UNIT, INTERCEPT HAS TO BE ZERO!!
C       CROPBEN(4,NC): CROP MARKET PRICE [$/MASS]
        IF(IBEN.EQ.2) CALL FMP3DPLSTRD(CROPBEN,4,NCROPS,IN,IOUT,0,0,4)
C
C6B-----READ FOR EACH FARM GROUNDWATER AND SURFACE-WATER COST COEFFICIENTS FOR EACH STRESS PERIOD:
C       WATERCOST(1,NF): DUMMY VARIABLE FOR FARM-ID
C       WATERCOST(2,NF): BASE MAINTENANCE COST FOR PUMP PER VOLUME OF GROUNDWATER [$/L3]
C       WATERCOST(3,NF): COST PER VERTICALLY PUMPED VOLUME OF GROUNDWATER PER UNIT LIFT IN WELL [$/(L3*L)]
C       WATERCOST(4,NF): COST PER VERTICALLY PUMPED VOLUME OF GROUNDWATER PER UNIT LIFT ON SURFACE    [$/(L3*L)]
C       WATERCOST(5,NF): COST PER HORIZONTALLY DELIVERED VOLUME OF GW PER UNIT DISTANCE (e.g. dep. on friction) [$/(L3*L)]
C       WATERCOST(6,NF): FIXED PRICE PER VOLUME OF ROUTED OR SEMI-ROUTED SURFACE-WATER [$/L3]
C       WATERCOST(7,NF): COST PER VERTICALLY PUMPED VOLUME OF SURFACE-W. PER UNIT LIFT ON SURFACE [$/(L3*L)]
C       WATERCOST(8,NF): COST PER HORIZONTALLY DELIVERED VOLUME OF SW PER UNIT DISTANCE (e.g. dep. on friction) [$/(L3*L)]
C       WATERCOST(9,NF): FIXED PRICE PER VOLUME OF NON-ROUTED SURFACE-WATER [$/L3]
        IF(ICOST.EQ.2)
     1  CALL FMP3DPLSTRD(WATERCOST,9,NFARMS,IN,IOUT,2,9,9)
      ENDIF
C
C7===== READ LIST OF NON-ROUTED DELIVERIES TO FARMS (NRDs) AND CREATE ARRAY OF RANKED NRDs ===================
C
      IF(INRDFL.EQ.0) THEN
      DO NF=1,NFARMS
      NRD(1,NF)=0.D0
      NRD(2,NF)=0.D0
      RNRD(1,1,NF)=0.D0
      RNRD(2,1,NF)=0.D0
      DO N=1,4
        UNRD(N,NF)=0.D0
      ENDDO
      ENDDO
      ENDIF  
C7A-----READ LIST OF YET UNRANKED NON-ROUTED DELIVERIES TO FARMS (VOLUME, RANK, USE-FLAG)
      IF(INRDFL.EQ.1) THEN
C
      CALL FMP3DPLSTRD(UNRD,MXNRDT*3+1,NFARMS,IN,IOUT,0,0,MXNRDT*3+1)
C
C seb MERGED MULTIPLE LSTCHK(3) INTO ONE GIANT IF() RATHER THAN BEING SPREAD ACROSS MULTIPLE WRITE STATEMENTS
      IF(LSTCHK(3)) THEN
        write(iout,1080) (IT, IT=1,MXNRDT)
C
 1080 format(1x,/,
     1'UNRANKED LIST OF NON-ROUTED DELIVERIES TO FARMS [L3]:',/,
     2'TYPE:  ',20I12)
C
        WRITE(IOUT,1081)
C
 1081 format(1x,/,'FARM-ID',2X,'VOLUME:') 
      DO NF=1,NFARMS      
      IF(IFA(NF).NE.0) THEN
        write(IOUT,1091)IDINT(UNRD(1,NF)),(UNRD(IT*3-1,NF), IT=1,MXNRDT)
 1091 format(1X,I6,20ES12.3)
      ENDIF
      ENDDO
C
        WRITE(IOUT,1082)
 1082 format(1x,/,'FARM-ID',2X,'PRIORITY RANK OF NRD-TYPE:')  
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0) THEN     
        write(IOUT,1092)
     1 IDINT(UNRD(1,NF)),(IDINT(UNRD(IT*3,NF)),IT=1,MXNRDT)
 1092 format(1X,I6,20I12)
      ENDIF
      ENDDO
C
        WRITE(IOUT,1083)
 1083 format(1x,/,
     1'FARM-ID',2X,'USE-FLAG:',/,
     29X,'0 = SUFFICIENT USE',/,
     39X,'1 = ABSOLUTE USE (RECHARGE SURPLUS BACK INTO CANAL)',/,
     49X,'2 = ABSOLUTE USE (INJECT SURPLUS INTO FARM-WELLS)')
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0) THEN
        write(IOUT,1093)
     1 IDINT(UNRD(1,NF)),(IDINT(UNRD(IT*3+1,NF)),IT=1,MXNRDT)
 1093 format(1X,I6,20I12)
      ENDIF
      ENDDO
      ENDIF    ! END LSTCHK(3)  seb
C
C7B-----CREATE ARRAY OF RANKED NON-ROUTED DELIVERIES TO FARMS (AND ARRAY OF THEIR NRD-USE FLAGS)
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                             ! seb ADDED LFID TO SKIP FARMS NOT IN USE  **FARMS NOT IN USE WILL NOT BE PRINTED TO IOUT
      DO IT=1,MXNRDT
      RNRD(1,IDINT(UNRD(3*IT,NF)),NF)=UNRD(3*IT-1,NF)/DBLE(PERLEN(KPER))
      RNRD(2,IDINT(UNRD(3*IT,NF)),NF)=UNRD(3*IT+1,NF)
      ENDDO
      ENDIF
      ENDDO
C seb MERGED MULTIPLE LSTCHK(3) INTO ONE GIANT IF() RATHER THAN BEING SPREAD ACROSS MULTIPLE WRITE STATEMENTS
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1084) 
 1084 format(1x,/,
     1'LIST OF NON-ROUTED DELIVERIES IN RANKED SEQUENCE [L3/T]:') 
C
        WRITE(IOUT,1085)
 1085 format(1x,/,'FARM-ID',2X,'RATE:')   
      DO NF=1,NFARMS
       IF(IFA(NF).NE.0) THEN
         WRITE(IOUT,1091) NF,(RNRD(1,IRT,NF), IRT=1,MXNRDT)
       ENDIF
      ENDDO
C
      WRITE(IOUT,1083)    
C
      DO NF=1,NFARMS
       IF(IFA(NF).NE.0) THEN
         WRITE(IOUT,1093) NF,(IDINT(RNRD(2,IRT,NF)), IRT=1,MXNRDT)
       ENDIF
      ENDDO
C
      END IF  !LISTCHK(3)
C
      ENDIF
C
C8===== READ INFORMATION IF LINKED TO STREAMFLOW ROUTING PACKAGE ============================================
      IF(IUNITSFR.GT.0) THEN
C
C8A-----READ SEMI-ROUTED DELIVERY PER FARM
      IF(ISRDFL.EQ.2) THEN
      CALL FMP3INTLSTRD(ISRD,5,NFARMS,IN,IOUT)
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,110)
      ENDIF
  110 FORMAT(/,1X,
     1'SEMI-ROUTED DELIVERIES TO FARMS (LOCATION OF DIVERSION-POINTS):',
     2/,1X,'FARM-ID    ROW    COLUMN   SEGMENT     REACH')
C seb MOVED LSTCHK(3) TO OUTSIDE OF LOOP      
      IF(LSTCHK(3)) THEN
        DO NF=1,NFARMS
          IF(IFA(NF).NE.0) THEN
            WRITE(IOUT,111) (ISRD(N,NF),N=1,5)
          ENDIF
  111       FORMAT(1X,2I7,4I10)
        ENDDO
      ENDIF
      ENDIF
C
C8B-----READ SEMI-ROUTED RUNOFF-RETURNFLOW PER FARM
      IF(ISRRFL.EQ.2) THEN
      CALL FMP3INTLSTRD(ISRR,5,NFARMS,IN,IOUT)
C seb MOVED LSTCHK(3) TO OUTSIDE OF LOOP      
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,112)
  112   FORMAT(/,1X,
     1  'SEMI-ROUTED RUNOFF-RETURNFLOW FROM FARMS ',
     2  '(LOCATION WHERE RUNOFF ENTERS A STREAM REACH):',/
     3  ,1X,'FARM-ID    ROW    COLUMN   SEGMENT     REACH')
        DO NF=1,NFARMS
          IF(IFA(NF).NE.0) THEN
            WRITE(IOUT,113) (ISRR(N,NF),N=1,5)
  113       FORMAT(1X,2I7,4I10)
          ENDIF
        ENDDO
      ENDIF  !LSTCHK(3)
      ENDIF
C
C8C-----READ FARM ALLOTMENT FOR EACH STRESS PERIOD
C
C8C1----READ EQUALLY APPROPRIATED FARM ALLOTMENT HEIGHT
      IF(IRDFL.NE.0.OR.ISRDFL.NE.0) THEN
        IF(IALLOTSW.EQ.1) THEN
          READ(IN,*) ALLOTSW
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,114) ALLOTSW
          ENDIF
  114     FORMAT(1X,/'SURFACE-WATER FARM ALLOTMENT HEIGHT [L]: ',F7.3)
C
C8C2----READ PRIOR APPROPRIATION SYSTEM WATER RIGHTS CALLS
        ELSEIF(IALLOTSW.EQ.2) THEN
          CALL FMP3DPLSTRD(WRC,2,NFARMS,IN,IOUT,0,0,2)
C seb MOVED LSTCHK(3) TO OUTSIDE OF LOOP
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,'(1X,/,"FARM-ID",2X,"WATER RIGHTS CALL [L3/T]")')
          DO NF=1,NFARMS
          IF(IFA(NF).NE.0) THEN
              write(IOUT,115)
     1      IDINT(WRC(1,NF)),WRC(2,NF)
  115       FORMAT(1X,I6,F19.4)
          ENDIF
          ENDDO
          ENDIF
        ENDIF
      ENDIF
C
C8D-----DETERMINE FULLY-ROUTED OR SEMI-ROUTED SURFACE-WATER DELIVERIES AND RETURNFLOWS:
C       (THIS ROUTINE CANNOT BE IN RQ BECAUSE OF THE USE OF IDIVAR SPECIFIED IN SFR-RP)
C       (ISTRM(4,L)=NSEG, ISTRM(2,L)=ROW, ISTRM(3,L)=COL, STRM(1,L)=RCHLEN)
C
C      IF(IRTPFL.GT.0) THEN
C      OPEN(UNIT=1009, FILE='ROUT.OUT', STATUS='UNKNOWN')                 !FMPOUT%UNIT(9) IS file number for ROUT.OUT or LST    seb
C        IOUF=1009
C      ELSEIF(IRTPFL.LT.0) THEN
C        IOUF=IOUT
C      ENDIF
      IF((IRTPFL.EQ.2.OR.IRTPFL.EQ.-2).AND.KPER.GT.1) IRTPFL=0
C
      DO L=1,NSTRM
        IF(IRDFL.NE.0.OR.ISRDFL.GT.0) SFRADD(L)=0.D0
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0) THEN  
        IF(IRDFL.NE.0.OR.ISRDFL.GT.0) IFCRID(NF,L)=0
        IFDRID(NF,L)=0
      ENDIF  
      ENDDO
      ENDDO
C
      DO NFF=1,NFARMS
      IF(IFA(NFF).EQ.0) GOTO  120
      NF=IFA(NFF)
      IF(IRTPFL.NE.0) THEN
      WRITE(FMPOUT%UNIT(9),'(//,"ROUTING INFORMATION FOR FARM: ",I8,", 
     1 PERIOD ",I4,/,A51)') NF, KPER,
     2 "---------------------------------------------------"
      ENDIF
C
C8D1----DETERMINE FULLY-ROUTED OR SEMI-ROUTED SURFACE-WATER DELIVERIES:
C       - DETERMINE CANAL SEGEMENT LENGTH WITHIN A FARM.
C         (STRM(12,L)=RUNOFF PER REACH, SEG(3,ISTRM(4,L))=RUNOFF PER SEGMENT, SEG(1,ISTRM(4,L))=SEGMENT LENGTH)
C       - DEFINE REACHES WITHIN A FARM TO BE FARM CANAL REACHES.
C       - DETERMINE PREEXISTING RUNOFF PER CANAL REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
C       - LOCK PREEXISTING RUNOFF PER CANAL REACH, SO THAT IT CAN'T BE OVERWRITTEN.
C       - LOCK DIVERSION INTO CANAL SEGMENT OF CANAL REACHES, SO THAT IT CAN'T BE OVERWRITTEN.
C       - RITE LIST OF FOUND CANAL REACHES AND FARM CANAL SEGMENT LENGTH TO LIST FILE.
      IF(IRTPFL.NE.0) THEN
      WRITE(FMPOUT%UNIT(9),
     1  '(/,1X,"DELIVERIES:",/,2X,"FULLY-ROUTED DELIVERIES:")')
      ENDIF
      FCANALSEGL=0.D0
      IF(IRDFL.NE.0.AND.(ISRDFL.EQ.0.OR.(ISRDFL.GT.0.AND.
     1  ISRD(2,NF).EQ.0.AND.ISRD(3,NF).EQ.0.AND.
     2  ISRD(4,NF).EQ.0.AND.ISRD(5,NF).EQ.0) ) ) THEN
        IF(IRTPFL.NE.0) THEN
        IF(IRDFL.EQ.1) THEN
        WRITE(FMPOUT%UNIT(9),'(3X,"ACTIVATED SEARCH FOR REACHES ",
     1  "OF DIVERSION SEGMENTS (CANALS) THAT ARE WITHIN A FARM.")')
        ELSEIF(IRDFL.EQ.-1)THEN
        WRITE(FMPOUT%UNIT(9),'(3X,"ACTIVATED SEARCH FOR REACHES ",
     1  "OF ANY STREAM SEGMENTS THAT ARE WITHIN A FARM.")')
        ENDIF
        ENDIF        
        LOLD1=0
        DO L=1,NSTRM
          DO I=1,FMLOC(NF)%Count                                        !seb removed double do loop NOTE THAT FOR INACTIVE FORMS FMLOC(NF)%Count=0 SO LOOP WILL NOT OCCUR
            IR=FMLOC(NF)%RC(1,I)
            IC=FMLOC(NF)%RC(2,I)
            IF(( (IRDFL.EQ.1.AND.IDIVAR(1,ISTRM(4,L)).GT.0).OR.
     1           (IRDFL.EQ.-1) ).AND.
     2            ISTRM(2,L).EQ.IR.AND.ISTRM(3,L).EQ.IC ) THEN
             IF(L.NE.LOLD1) THEN                                        !don't count reaches twice if they are intercalated into an "inner" corner of the farm boundary!
             FCANALSEGL=FCANALSEGL+DBLE(STRM(1,L))
             IFCRID(NF,L)=NF                                            !CREATE ID OF REACHES WITHIN A FARM EQUAL TO ID OF FARM
             STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L))) !RUNOFF PER REACH MUST BE RESET: OTHERWISE FOR ITMP<0 IN SFR (DATA REUSED FOR NEW STRESS PERIODS), THE RUNOFF PER REACH WOULD NOT BE OVERWRITTEN, AND THE LAST 'RUNOFF MINUS REACH DEL.'-VALUE WOULD FALSLY REPRESENT THE RUNOFF PER REACH VALUE FOR THE NEW STRESS PERIOD!
             SFRADD(L)=DBLE(STRM(12,L))                                 !-------> !RENAME, SO THAT FM AT SECOND AND FOLLOWING ITERATIONS DOESN'T START UP WITH STRM(12,L)=STRM(12,L)+RDR(L), BUT WITH STRM(12,L)=RUNOFF(L)+RDR(L)
             DIVADD(ISTRM(4,L))=DBLE(SEG(2,ISTRM(4,L)))                 !-------> !Similarly to runoff: lock pre-specified diversion amount per stress period, so that it won't be overwritten by reduced diversion rate!
             IF(IRTPFL.NE.0) THEN
             IF(LOLD1.EQ.0) WRITE(FMPOUT%UNIT(9),1161)                  !However note that reuse stress period (-1) in input file won't work. Each stress period's inflow must be specified separately, since if -1 is specified the last stored value (i.e. seg(2,n)) will be re-used.
     1       ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),ISTRM(5,L)
 1161        FORMAT(3X,'FULLY ROUTED DELIVERY FROM THE FIRST, MOST ',
     1       'UPSTREAM REACH OF A SEQUENCE OF REACHES',/,3X,
     2       'THAT ARE WITHIN THE FARM:',/,5X,
     3       'HEAD-GATE WITHIN FARM AT:',/,6X,
     4       'ROW',2X,'COLUMN',2X,'SEGMENT NO.',2X,'REACH NO.',/,
     5       3X,I6,I8,I12,I11,/,5X,
     6       'SEQUENCE OF REACHES WITHIN FARM:',/,
     7       6X,'ROW',2X,'COLUMN',2X,'SEGMENT NO.',2X,'REACH NO.',2X
     8       ,'REACH_LENGTH')
             WRITE(FMPOUT%UNIT(9),116)ISTRM(2,L),ISTRM(3,L),
     1       ISTRM(4,L),ISTRM(5,L),STRM(1,L) 
  116        FORMAT(3X,I6,I8,I12,I11,3X,G15.8)
             ENDIF
             ENDIF
             LOLD1=L
            ENDIF
          ENDDO
        ENDDO        
      ELSE
        IF(IRTPFL.NE.0) THEN
        IF(IRDFL.EQ.1) THEN   
        WRITE(FMPOUT%UNIT(9),'(3X,"DEACTIVATED SEARCH FOR REACHES OF ",
     1  "DIVERSION SEGMENTS (CANALS) THAT ARE WITHIN A FARM.")')
        ELSEIF(IRDFL.EQ.-1) THEN
        WRITE(FMPOUT%UNIT(9),'(3X,"DEACTIVATED SEARCH FOR REACHES OF ",
     1  "ANY STREAM SEGMENTS THAT ARE WITHIN A FARM.")')
        ELSEIF(IRDFL.EQ.0) THEN
        WRITE(FMPOUT%UNIT(9),
     1    '(3X,"ROUTED DELIVERY OPTION WAS NOT SELECTED ")')
        ENDIF
        ENDIF
      ENDIF
      IF(IRDFL.NE.0) FCSEGL(NF)=FCANALSEGL
      IF(IRTPFL.NE.0) THEN
      IF(IRDFL.NE.0)THEN
        IF(FCSEGL(NF).GT.0) THEN
        WRITE(FMPOUT%UNIT(9),'
     1 (3X,"ACTIVE FARM CANAL SEGMENT LENGTH: ",G15.8)') FCSEGL(NF)
        ELSE
        WRITE(FMPOUT%UNIT(9),'
     1  (3X,"NO ACTIVE FARM CANAL REACHES ARE WITHIN ",
     2  "THE FARM: NO FULLY-ROUTED DIVERSION POSSIBLE.")')        
        ENDIF
      ELSE
        WRITE(FMPOUT%UNIT(9),'
     1  (3X,"NO ACTIVE FARM CANAL REACHES ARE WITHIN ",
     2  "THE FARM: NO FULLY-ROUTED DIVERSION POSSIBLE.")')
      ENDIF
      ENDIF
C
C8D2---FOR SEMI-ROUTED DELIVERY:
C       - DEFINE A SPECIFIED REACH (BY COORDINATES) TO BE A "REMOTE" FARM DIVERSION REACH (= REMOTE HEAD-GATE)
C         (NOTE: UNLIKE 'CANAL REACHES' INSIDE A FARM, THIS DIVERSION REACH CAN BE ON ANY TYPE OF SEGMENT).
C       - DETERMINE RUNOFF INTO "REMOTE" FARM DIVERSION REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
C       - LOCK RUNOFF INTO "REMOTE" FARM DIVERSION REACH, SO THAT IT CAN'T BE OVERWRITTEN.
C       - LOCK DIVERSION INTO CANAL SEGMENT OF "REMOTE" FARM DIVERSION REACH, SO THAT IT CAN'T BE OVERWRITTEN.
      IF(IRTPFL.NE.0) THEN
      WRITE(FMPOUT%UNIT(9),'(/,2X,"SEMI-ROUTED DELIVERIES:")')   
      ENDIF
      IF(ISRDFL.GT.0.AND.((ISRD(2,NF).GT.0.AND.ISRD(3,NF).GT.0).OR.
     1                    (ISRD(4,NF).GT.0.AND.ISRD(5,NF).GT.0))) THEN
        DO L=1,NSTRM                                                    !  IF POINT UNIQUELY IDENTIFIED BY:
         IF((ISRD(2,NF).EQ.ISTRM(2,L).AND.ISRD(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL SEG REACH | FULL SET OF INFO (MORE INFO THAN REQUIRED)
     1       ISRD(4,NF).EQ.ISTRM(4,L).AND.ISRD(5,NF).EQ.ISTRM(5,L)).OR. !  |                  |
     2      (ISRD(2,NF).EQ.ISTRM(2,L).AND.ISRD(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL SEG 0/-   | IF >=1 SEG PASS THROUGH CELL (USER PREFERS IDENTIFICATION OF LOCATION WITH COORDINATES)
     3       ISRD(4,NF).EQ.ISTRM(4,L).AND.ISRD(5,NF).EQ.0)         .OR. !  |                  |
     4      (ISRD(2,NF).EQ.ISTRM(2,L).AND.ISRD(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL 0/- 0/-   | IF JUST 1 SEG PASSES THROUGH CELL (USER PREFERS IDENTIFICATION OF LOCATION WITH COORDINATES)
     5       ISRD(4,NF).EQ.0.AND.ISRD(5,NF).EQ.0)                  .OR. !  |                  |
     6      (ISRD(2,NF).EQ.0.AND.ISRD(3,NF).EQ.0.AND.                   !  |0   0   SEG REACH | IF >=1 SEG PASS THROUGH CELL (USER PREFERS IDENFIFICATION OF LOCATION BY REACH NO.)
     7       ISRD(4,NF).EQ.ISTRM(4,L).AND.ISRD(5,NF).EQ.ISTRM(5,L)))THEN
            IFCRID(NF,L)=NF
            STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L)))
            SFRADD(L)=DBLE(STRM(12,L))
            DIVADD(ISTRM(4,L))=DBLE(SEG(2,ISTRM(4,L)))
            IF(IRTPFL.NE.0) THEN
            WRITE(FMPOUT%UNIT(9),117) 
     1                       ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),ISTRM(5,L)
  117       FORMAT(3X,
     1      'SEMI-ROUTED DELIVERY FROM A SPECIFIED STREAM REACH AT:'
     2      ,/,6X,'ROW',2X,'COLUMN',2X,'SEGMENT NO.',2X,'REACH NO.',/,
     3       3X,I6,I8,I12,I11)
            ENDIF
          ENDIF
        ENDDO
      ELSE
      IF(IRTPFL.NE.0) THEN
      WRITE(FMPOUT%UNIT(9),'
     1  (3X,"NO POINT OF DIVERSION FOR SEMI-ROUTED DELIVERY SPECIFIED:",
     2  " NO SEMI-ROUTED DIVERSION POSSIBLE.")')
      ENDIF
      ENDIF
C
C8D3----DEFINE NUMBER OF SEGMENT FOR EACH FARM, ONCE AN FARM HEADGATE REACH WITHIN (AUTOMATIC) OR REMOTE FROM (SPECIFIED) A FARM IS FOUND
      IF(IRDFL.NE.0.OR.ISRDFL.GT.0) THEN
        IFCRIDOLD=0
        DO L = 3,NSTRM
          IF(IFCRID(NF,L).EQ.NF .AND. IFCRID(NF,L).GT.0 .AND.           !Farm Reach: * belongs to farm, * is at all a farm reach, * 1st one being different from previous one,
     1     IFCRID(NF,L).NE.IFCRID(NF,L-1).AND.IFCRID(NF,L).NE.          !Farm Reach: * 1st one being different from second previous one, meaning 1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point), but still within the same farm.
     2     IFCRID(NF,L-2).AND.IFCRID(NF,L).NE.IFCRIDOLD) THEN
             IFCRIDOLD=IFCRID(NF,L)
             IF(IALLOTSW.GE.2) NFSEG(NF)=ISTRM(4,L)
            ENDIF
        ENDDO
      ENDIF
C
      if(LFID(NF))CALL RETURNFLOW(NF,FMPOUT%UNIT(9))                    !Don't assign a return flow location for inactive farms--rth
C
  120 ENDDO                                                             !END OF FARMS DO-LOOP
      IF(IRTPFL.NE.0) WRITE(FMPOUT%UNIT(9),'(//)')
C
      ENDIF                                                             !END OF SFR IF STATEMENT
C
C9===== FOR CROP CONDITIONS:                                                             ====================
C       � CALCULATE POTENTIAL TRANSPIRATORY AND EVAPORATIVE CONSUMPTIVE USE, AS WELL AS
C       � MAXIMUM TRANSPIRATION AND MAXIMUM EVAPORATION FROM GROUNDWATER.
C       FOR NON-CROP FALLOW CONDITIONS:
C       � SET POTENTIAL TRANSPIRATION = 0, AND POTENTIAL EVAPORATION EQUAL TO
C         PRECIPTIATION OR REFERENCE ET, WHICH EVER IS LESS.
C       (if consumptive use and precipitation were read for every stress period; however
C        if calculated as time step averages --> AD ROUTINE)
C        Note: the 2D-CU array and therefore also the max.-TGW array ignores
C              small scale 'no-flow and constant head details,' because
C              net-recharge will not be calculated for such cells.
      IF((ICUFL.LE.2.OR.(ICUFL.EQ.-2.AND.FMPDAT(1)%ICUFL.LE.2)).AND.
     1  (IPFL.EQ.2.OR.(IPFL.EQ.-1.AND.(FMPDAT(1)%IPFL.EQ.2)))) THEN
      DO IR=1,NROW
      DO IC=1,NCOL
         IF(ICID(IC,IR).NE.0) THEN
         IF(ICID(IC,IR).GT.0)
     1   TGW(IC,IR)=CU2D(IC,IR)*FTE(2,ICID(IC,IR))
         IF(TGW(IC,IR).LT.0.D0.OR.IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1)
     1   TGW(IC,IR)=0.D0
         IF(ICID(IC,IR).NE.0 .AND. TPPOT(IC,IR).GT.TGW(IC,IR))          !Pe > TGW very rare on stress period averages (see AD)
     1   TPPOT(IC,IR)=TGW(IC,IR)
         IF(ICID(IC,IR).GT.0)
     1   EGW(IC,IR)=CU2D(IC,IR)*(1.D0-FTE(2,ICID(IC,IR)))
         IF(EGW(IC,IR).LT.0.D0) EGW(IC,IR)=0.D0
         IF(IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1) THEN
         EGW(IC,IR)=CU2D(IC,IR)                                         !CU HERE IS PRESPECIFIED AND NOT CALCULATED BASED ON AN Kc
         EPPOT(IC,IR)=PFLR(IC,IR)
         ENDIF
         IF(ICID(IC,IR).NE.0 .AND. EPPOT(IC,IR).GT.EGW(IC,IR))
     1   EPPOT(IC,IR)=EGW(IC,IR)
         ELSE
         TPPOT(IC,IR)=0.D0
         TGW(IC,IR)=0.D0
         EPPOT(IC,IR)=0.D0
         EGW(IC,IR)=0.D0
         ENDIF
      ENDDO
      ENDDO
      ENDIF
C     
      IF(ILGR.NE.0) THEN
      IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) IUNITSFR=0
      ENDIF
C
C11==== RETURN ================================================================
  260 RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3ADSUB(KPER,IGRID)
C-----VERSION 2.01 07/29/2010 FMP3ADSUB
C*********************************************************************
C     APPLY SUBSIDENCE DISPLACEMENT (DVZ) TO GROUND-SURFACE ELEVATION,
C     RISE-OVER-RUN SLOPE, AND THE LOWEST ELEVATION WITHIN A FARM.
C*********************************************************************
C        SPECIFICATIONS:
C     -----------------------------------------------------------------
      USE FMPMODULE,    ONLY:GSURF,NFARMS,IIESWFL,IRTPFL,SFMP3PNT,LFID,
     +                       FMPOUT
      USE FMPBLK,       ONLY:ZER
      USE GLOBAL,       ONLY:IOUT,NROW,NCOL,NLAY,IBOUND,SUBLNK  
      USE GWFSUBMODULE, ONLY:DVZ
      IMPLICIT NONE
C     -----------------------------------------------------------------
C        ARGUMENTS:
C     -----------------------------------------------------------------
      INTEGER KPER,IGRID
C     -----------------------------------------------------------------
C        LOCAL VARIABLES:
C     -----------------------------------------------------------------
      INTEGER IR,IC,IL,NF,IROUT
C     -----------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
      !
      IF(SUBLNK)THEN
      !
      IROUT=FMPOUT%UNIT(9)
C
      IF(ANY(DVZ.NE.ZER)) THEN                                          !seb changed from IF(ABS(MAXVAL(DVZ)).GT.ZER)
C1-----APPLY DISPLACEMENT TO GROUND-SURFACE ELEVATION OF UPPERMOST ACTIVE LAYER    
      DO IR=1,NROW
      DO IC=1,NCOL
      DO IL=1,NLAY
        IF(IBOUND(IC,IR,IL).EQ.0.AND.IL.NE.NLAY) GOTO 60
        GSURF(IC,IR)=GSURF(IC,IR)-DVZ(IC,IR,IL)
        GOTO 50
 60   ENDDO
 50   ENDDO
      ENDDO     
C
C2-----RECOMPUTE RISE-OVER-RUN SLOPE
      IF(IIESWFL.EQ.0) CALL RISE_RUN()
C
C3-----RECHECK FOR LOWEST ELEVATION WITHIN ANY FARM AND NEAREST REACH
      DO NF=1,NFARMS
       IF(LFID(NF))THEN                                                 !test for active farms  -- rth
         IF(IRTPFL.NE.0) THEN
           WRITE(IROUT,'(//,"RECHECK RETURNFLOW INFORMATION FOR FARM: ",
     1     I8,", PERIOD ",I4,/,A51)') NF, KPER,
     2     "---------------------------------------------------"
         ENDIF
         CALL RETURNFLOW(NF,IROUT)
       END IF
      ENDDO
C
      ENDIF
      !
      END IF !SUBLNK
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3AD(KPER,IGRID)
C-----VERSION 2 09/18/2009 FMP3AD
C*********************************************************************
C     RESET (REDUCED) DIVERSION RATE OF PREVIOUS TIME STEP,
C     COMPUTE AVERAGE EVAPOTRANSPIRATION, ROOT DEPTHS AND EFFECTIVE
C     PRECIPITATION PER TIME STEP, AND CALCULATE THE MAXIMUM ET FROM
C     GROUNDWATER.
C     RESET HEAD-DEPENDENT EFFICIENCIES TO SPECIFIED EFFICIENCY
C     (ONLY CALLED IF ICUFL, IPFL, OR IRTFL = 3, OR IEBFL = 1 OR = 3)
C*********************************************************************
C        SPECIFICATIONS:
C     -----------------------------------------------------------------
      USE FMPMODULE, ONLY:FWLAUX,NAUX,NCROPS,LENSIM,RTD,ROOT,ICID,RT2D,
     1    IRTFL,ETC,CU,CU2D,ICUFL,PFLR,TPPOT,EPPOT,FTE,EGW,CLIMATE,TGW,
     2    IPFL,IDEFFL,IRDFL,ISRDFL,DIVADD,IROTFL,IEFFL,IEBFL,IFID,
     3    NFARMS,EFF,EF2D,IFA,ICA,ISTARTFL,FWLAUXORDER,SFMP3PNT  
      USE GLOBAL,       ONLY:IOUT,NROW,NCOL,DELR,DELC
      USE GWFBASMODULE, ONLY:TOTIM,DELT
      USE GWFSFRMODULE, ONLY:SEG,NSEGDIM,IDIVAR
      IMPLICIT NONE
C     -----------------------------------------------------------------
C        ARGUMENTS:
C     -----------------------------------------------------------------
      INTEGER IGRID,KPER
C     -----------------------------------------------------------------
C        LOCAL VARIABLES:
C     -----------------------------------------------------------------
      INTEGER N,ISTOP,ISTART,NC,L,IR,IC,NF                              !FORMERLY IMPLICIT INTEGER
      DOUBLE PRECISION RTDSUM,ETCSUM,ETRSUM,ETRAVG,PSUM,PAVG            !FORMERLY IMPLICIT REAL
C     -----------------------------------------------------------------
C
      CALL SFMP3PNT(IGRID)
      IF(FWLAUXORDER(1).EQ."QMAXRESET") RETURN                          !seb USING STATIC ASSIGNMENT INSTEAD FOR CHECK
      !DO N=1,NAUX
      !  IF(FWLAUX(N).EQ."QMAXRESET") RETURN
      !ENDDO  
C
C1===== SET ISTARTFL FLAG ===============================================================
C
C1A-----SET FLAG TO INDICATE START OF TIME STEP AS START OF CHANGING CONDITIONS OF CONSUMPTIVE USE
C       (only if by beg. of stress period it was not already found, that divtmp(k-1) < divadd(k))
c       (Info: if consumptive use changes only at each stress period, then istartfl is set zero only in FMP3RP.)
      IF(ISTARTFL.NE.-1) ISTARTFL=0
c
C1B1----FOR USE OF ACREAGE-OPTIMIZATION WITH "WATER-CONSERVATION POOL" ACCOUNTING ...
      IF(IDEFFL.EQ.2.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0)) THEN
          DO N=1,NSEGDIM                                          
          IF(IDIVAR(1,N).GT.0. AND. SEG(2,N).LT.SNGL(DIVADD(N))) THEN   !(opposite to RP, within AD seg(2,n) will never be overwritten)
C         Reduced diversion rates (due to little profitability of SW-irrigation) are only reused
C         from iteration to iteration, but reset to original stress-period defined diversion rate
C         once a new time step with newly calculated demand (TFDR) starts.
C         If TFDR is variable over time steps, it is unlogic to relate a respective TFDR(t) to a div.-rate(t-1),
C         which was reduced due to little profit of SW-irrigation during the previous time step.
C         I.e. the div.rate needs to be reset to value reused from previous or specified for current stess-per.).
c
c         Therefore, at the beginning of each time step:
c         If red. div. rate from last iter. of prev. time step < stress-period pre-defined diversion rate,then
C
C         - RESET DIVERSION RATE OF CANAL SEGMENTS (SEG(2,N)) FROM LAST ITERATION OF PREVIOUS TIME STEP
C           TO ORIGINAL VALUE SPECIFIED FOR STRESS PERIOD (DIVADD(N)):
            SEG(2,N)=SNGL(DIVADD(N))
C
C         - SET ISTARTFL=-1:
            ISTARTFL=-1
c             * to cause istartfl=0 in FMP3FM, and to skip 1st FMP3FM iteration:
c                 Only allow the 1st SFR-FM iteration, in order to get a good initial value for the 2nd iteration.
c                 Otherwise old reach-inflows FLOWIN and reach-leakages FLOBOT (calc. during the last iteration
c                 of the previous time step) would be subtracted from the new diversion rate.
c             * to cause istartfl=1 during the 2nd FMP3FM iteration, in order to reset
c               reduced cell-area percentages to allow a new acreage-optimization, if needed
C
          ENDIF                                   
          ENDDO                                   
      ELSE                                        
C
C1B2----FOR ALL OTHER DEFICIENCY FLAGS
C       * skipping FMP3FM in 1st iteration is strictly not necessary if diversion rate doesn't change
c         (as caused by "water-conservation pool"), but found better by standard for faster convergence!          
c       * for acreage-optimization of farms no supplied by (semi-)routed SW:
c         even if link to sfr does not exist, istartfl=-1 causes istartfl=1 during the 2nd FMP3FM iteration,
c         in order to reset reduced cell-area percentages to allow a new acreage-optimization, if needed
        ISTARTFL=-1                               
      ENDIF
C                                                                                                                         
      ISTOP=ANINT(TOTIM)
      ISTART=ANINT(TOTIM-DELT+1)
C
C
C2===== CALCULATE TIME STEP AVERAGES OF ROOT ZONE, CONSUMPTIVE USE, REFRENCE ET, AND PRECIPITATION ===========
C       AND CREATE 2D-ARRAYS

C2A-----FOR EACH CROP: CALCULATE AVERAGES OF DAILY ROOT-ZONE DEPTH VALUES OVER EACH TIME STEP,
C       AND CREATE 2D-ARRAY OF ROOT-ZONE DEPTHS FOR ALL CELLS
      IF(IRTFL.EQ.3) THEN                                               !seb changed tripple loop to single loop with a where for effeciency
        DO NC=1,NCROPS
          IF(ICA(NC).EQ.0) CYCLE
          RTDSUM=SUM(RTD(NC,ISTART:ISTOP))/DBLE(ISTOP-ISTART+1)     
      FORALL (IC=1:NCOL,IR=1:NROW,ICID(IC,IR).EQ.NC)RT2D(IC,IR)=RTDSUM       !SCOTT seb WHERE STATEMENT IS CAUSING ISSUES WITH HIGHLIGHTING
          !WHERE(ICID.EQ.NC) RT2D=RTDSUM
        END DO
      FORALL (IC=1:NCOL,IR=1:NROW,ICID(IC,IR).EQ.-1) RT2D(IC,IR)=1.D-3 
        !WHERE(ICID.EQ.-1) RT2D=1.D-3                                     !IN CASE ET CONCEPT = 1, TERM TRZ/RRZ SHOULD NOT BE "DEVIDE BY ZERO"
      END IF
C
C2B-----FOR EACH CROP: CALCULATE AVERAGES OF DAILY CROP-ET-FLUX VALUES OVER EACH TIME STEP,
C       AND CREATE 2D-ARRAY CROP-ET-FLOWRATES (CONSUMPTIVE USE) FOR ALL CELLS
      IF(ICUFL.EQ.3) THEN
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN 
      ETCSUM=0.D0
      DO L=ISTART,ISTOP
      ETCSUM=ETCSUM+ETC(NC,L)
      ENDDO
      CU(2,NC)=ETCSUM/DBLE(ISTOP-ISTART+1)
      ENDIF
      ENDDO
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN
         DO IR=1,NROW
         DO IC=1,NCOL
            IF(ICID(IC,IR).EQ.INT(CU(1,NC))) THEN
            CU2D(IC,IR)=CU(2,NC)*DBLE(DELR(IC)*DELC(IR))
            ENDIF
         ENDDO
         ENDDO
      ENDIF
      ENDDO   
C
C2C-----CALCULATE AVERAGES OF DAILY REFERENCE ET FLUX VALUES OVER EACH TIME STEP,
C       AND CREATE 2D-ARRAY OF ETr-FLOWRATES FOR ALL CELLS    
      ETRSUM=0.D0
      DO L=ISTART,ISTOP
      ETRSUM=ETRSUM+CLIMATE(5,L)
      ENDDO
      ETRAVG=ETRSUM/DBLE(ISTOP-ISTART+1)
         DO IR=1,NROW
         DO IC=1,NCOL
            EGW(IC,IR)=ETRAVG*DBLE(DELR(IC)*DELC(IR))                   !EGW-max in general = ETr, but overwritten as E portion of ETc and only kept for Non-irrigation Season
         ENDDO
         ENDDO
      ENDIF
C
C2D-----CALCULATE AVERAGES OF DAILY PRECIPITATION-FLUX VALUES OVER EACH TIME STEP,
C       AND CREATE 2D-ARRAY FOR PRECIPITATION-FLOWRATES,
C       AND FOR EVAPORATIVE AND TRANSPIRATIVE PART OF PRECIPITATION FOR ALL CELLS.
      IF(IPFL.EQ.3) THEN
      PSUM=0.D0
      DO L=ISTART,ISTOP
      PSUM=PSUM+CLIMATE(4,L)
      ENDDO
      PAVG=PSUM/DBLE(ISTOP-ISTART+1)
      DO IR=1,NROW
      DO IC=1,NCOL
         PFLR(IC,IR)=PAVG*DBLE(DELR(IC)*DELC(IR))
         TPPOT(IC,IR)=0.D0
         EPPOT(IC,IR)=0.D0
         IF(ICID(IC,IR).GT.0) THEN
         TPPOT(IC,IR)=PFLR(IC,IR)*FTE(2,ICID(IC,IR))
         EPPOT(IC,IR)=PFLR(IC,IR)*FTE(3,ICID(IC,IR))
         ENDIF
      ENDDO
      ENDDO
      ENDIF
C
C
C3===== FOR CROP CONDITIONS:                                                              ====================
C       � CALCULATE POTENTIAL TRANSPIRATORY AND EVAPORATIVE CONSUMPTIVE USE, AS WELL AS
C       � MAXIMUM TRANSPIRATION AND MAXIMUM EVAPORATION FROM GROUNDWATER.
C       FOR NON-CROP FALLOW CONDITIONS:
C       � SET POTENTIAL TRANSPIRATION = 0, AND POTENTIAL EVAPORATION EQUAL TO
C         PRECIPTIATION OR REFERENCE ET, WHICH EVER IS LESS.
      IF(ICUFL.EQ.3 .AND. IPFL.EQ.3) THEN
      DO IR=1,NROW
      DO IC=1,NCOL
         TGW(IC,IR)=0.D0
         EGW(IC,IR)=0.D0  !SCOTT THIS WAS JUST CALCULATED IN PREVIOUS DEBUG POINTS
         IF(ICID(IC,IR).NE.0) THEN
         IF(ICID(IC,IR).GT.0)
     1   TGW(IC,IR)=CU2D(IC,IR)*FTE(2,ICID(IC,IR))
         IF(TGW(IC,IR).LT.0.D0.OR.IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1)
     1   TGW(IC,IR)=0.D0
         IF(ICID(IC,IR).NE.0 .AND. TPPOT(IC,IR).GT.TGW(IC,IR))          !Pe > TGW very rare on stress period averages (see AD)
     1   TPPOT(IC,IR)=TGW(IC,IR)
         IF(IROTFL.NE.KPER.AND.ICID(IC,IR).GT.0)
     1   EGW(IC,IR)=CU2D(IC,IR)*(1.D0-FTE(2,ICID(IC,IR)))               !EGW (before = ETr) overwritten as E portion of ETc
         IF(EGW(IC,IR).LT.0.D0) EGW(IC,IR)=0.D0
         IF(IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1)EPPOT(IC,IR)=PFLR(IC,IR)
         IF(ICID(IC,IR).NE.0 .AND. EPPOT(IC,IR).GT.EGW(IC,IR)) THEN
         EPPOT(IC,IR)=EGW(IC,IR)
         ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDIF
C
C
C4===== RESET EFFICIENCY FROM CALCULATED EFFICIENCY TO SPECIFIED EFFICIENCIES AT BEGINNING OF EACH TIME STEP ==
C       (ONLY IF TFDR CHANGES AT BEGINNING OF EACH TIME STEP, BECAUSE ICUFL=3, OR IRTFL=3, OR IPFL=3)!
      IF(IEBFL.EQ.1.OR.IEBFL.EQ.3) THEN                                 !NOT NECESSARY FOR (IEBFL=1 OR 2)
      DO NF=1,NFARMS                          
      IF(IFA(NF).NE.0) THEN
      DO NC=2,1+NCROPS                                                  !NOT FOR NCROPS+3 AND NCROPS+4 (NONZERO CALCULATED VALUES IF IEBFL=2)
         DO IR=1,NROW                         
         DO IC=1,NCOL                         
            IF(NC.GT.2.AND.EFF(NC,NF).EQ.0D0) THEN
              IF(IFID(IC,IR).EQ.INT(EFF(1,NF))) THEN
              EF2D(IC,IR)=EFF(2,NF)
              ENDIF
            ELSE
              IF(IFID(IC,IR).EQ.INT(EFF(1,NF)).AND.
     1        ICID(IC,IR).EQ.NC-1) THEN
              EF2D(IC,IR)=EFF(NC,NF)
              ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDDO
      ENDIF
      ENDDO
      ENDIF
C
C5===== RETURN ===============================================================================================      
      RETURN
      END SUBROUTINE
C=============================================================================================================
C
      SUBROUTINE FMP3FM(KITER,KPER,KSTP,IUNITSFR,IUNITMNW1,
     1   IUNITMNW2,IUNITNWT,IUNITUZF,IUNITDRT,IGRID,NGRIDS,ILGR,LGRITER)  !added IUNITNWT,IUNITMNW2 by rth
C-----VERSION 2 09/21/09 FMP3FM
C     ******************************************************************
C     COMPUTE FARM WELL DISCHARGE AND NET-RECHARGE AND ADD THE TERMS TO
C     RHS OR THE RHS AND HCOF, RESPECTIVELY.
C     Determine for each farm:
C                * Total Farm Delivery Requirement; 
C                * Actual Delivery from Non-Routed and (Semi-)Routed Surface-Water; &
C                * Groundwater Pumping Requirement.
C     If Demand > Supply:
C                * Assume Zero Scenario; or
C                * Apply Deficiency Scenarios (specified by user).
C     Update for each Scenario:
C                * Actual Delivery from Non-Routed and (Semi-)Routed Surface-Water:
C                  optionally divert it from canals - if defined by SFR);
C                * Discharge from each Farm Well:
C                  add Q of FMP-farmwells to RHS, and
C                  optionally use Q of MNW-farmwells as Q-desired in MNW package);
C                * Net Recharge and add it to RHS and HCOF;
C                * Surface-Water Runoff and route it to Drains (if defined by SFR).     
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE, ONLY:FWLAUX,NAUX,TGW,TGWO,RT2D,GSURF,SOIL2D,PSI,
     1    PFLR,TPPOT,EPPOT,CU2D,CU,FTE,EGW,EF2D,TGWA,FNRCH,NFWELS,
     2    MXFWEL,TDR,IFID,IFIDOLD,ICID,TFDR,TFDROLD,FWELL,QMAXF,QREQ,
     3    QREQOLD,QAVF,QDEF,QEXC,NFARMS,NCROPS,NFWLVL,NWPERF,NRD,UNRD,
     4    RNRD,MXNRDT,IFCRID,IFDRID,NFSEG,RDR,FLOWINMIN,QSTRMIN,FCSEGL,
     5    FDSEGL,SWRUN,SFRADD,DIVADD,DIVTMP,IDEFFL,ICCFL,INRDFL,ISRDFL,
     6    ISRRFL,IRDFL,IROTFL,HLIFT,ELHL,ELNR,DNR,DWE,IFALLOW,CROPBEN,
     7    WATERCOST,OPT,REDPCT,GWREDPCT,SWREDPCT,NRREDPCT,EFF,FIESW,
     8    RISERUN,IALLOTSW,ALLOTSW,KNTR,IIESWFL,IRDRFL,PCLOSE,IEBFL,
     9    ISDPFL,IOPFL,WRC,IPAPFL,TTOT,ETOT,DPERC,EGWA,IWELLFLD,IFA,ICA,
     *    ISTARTFL,FMPDAT,FALLOT,IALLOTGW,ALLOTGW,QMAXFW,QFRACF,
     *    PSIRAMPF,QXTF,QSAVE,SATTHK,                                   !Added Variables for reducing pumpage in Farm Wells for NWT by rth
     *    LFID,FWLAUXORDER,AUXV,MNW2NAM,MNW2LOC,FMLOC,SFMP3PNT,         !seb ADDED LFID,FWLAUXORDER,AUXV,MNW2NAM,MNW2LOC,SFMP3PNT  
     *    DRTFLOW
      USE FMPBLK
      USE GLOBAL,       ONLY:IOUT,RHS,HCOF,IBOUND,HNEW,NCOL,NROW,NLAY,
     1                       DELR,DELC,LENUNI,PERLEN,NPER,GLOBALDAT,
     2                       LBOTM,BOTM                                 !Added Variables for reducing pumpage in Farm Wells for NWT by rth
      USE GWFSFRMODULE, ONLY:STRM,ISTRM,NSTRM,SEG,DVRSFLW,NSEGDIM,
     1                       IDIVAR,SGOTFLW,GWFSFRDAT
      USE GWFMNW1MODULE, ONLY:WELL2,NWELL2
      USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD,WELLID,MNWMAX                 !seb MNW2 LINK DATA
      USE GWFUZFMODULE, ONLY:IUZFBND,FINF,EXCESPP,VKS
      USE LGRMODULE,    ONLY:IFMPGRID,LGRDAT,ISFRGRID
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell                        !Added Variables for reducing pumpage in Farm Wells for NWT by rth
      USE GWFUPWMODULE, ONLY: LAYTYPUPW                                 !Added Variables for reducing pumpage in Farm Wells for NWT by rth
      USE FMPFUNCT,     ONLY: RTFUNC                                    ! seb Converted RTFUNC to modular function to provide implicit interface
      IMPLICIT NONE
ccrth
!External function interface
      INTERFACE 
        FUNCTION SMOOTH3(H,T,B,dQ,ISS)
          DOUBLE PRECISION SMOOTH3
          DOUBLE PRECISION, INTENT(IN) :: H
          DOUBLE PRECISION, INTENT(IN) :: T
          DOUBLE PRECISION, INTENT(IN) :: B
          DOUBLE PRECISION, INTENT(OUT) :: dQ
          INTEGER, INTENT(IN) :: ISS
        END FUNCTION SMOOTH3
      END INTERFACE
!
      double precision Qp,Ttop,Bbot,dQp,Qold,Hd,SatDif
ccrth

C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER KITER,KPER,KSTP,IUNITSFR,IUNITMNW1,IUNITUZF,ILGR,IUNITNWT,
     1 IGRID,NGRIDS,LGRITER,IJ,ISS,IUNITMNW2,IUNITDRT
C     ------------------------------------------------------------------
C        FUNCTIONS AND COMMON BLOCKS:  
C      EXTERNAL RTFUNC                                                  !seb RTFUNC DECLARED THROUGH IMPLICIT INTERFACE
      COMMON ITER, ITER2      
C     ------------------------------------------------------------------ 
C        LOCAL VARIABLES:
      INTEGER NF,L,ITER2,ITER,NC,IR,IC,IOPTFL,N,NOD,IL,ISNGLWELL,M,J,I, !FORMERLY IMPLICIT INTEGER
     1 ID,NT,IDDELF,NSEGOLD,IT,ITTOT,IRT,ITU,LL,NR,IIA,IB,K,NCELL,NCON, !Changed variable IA to IIA to avoid conflict with NWT variable IA by rth
     2 NOPV,NLSER,NLGER,NLEQR,NROWA,NCOLA,ICCELL,IRCELL,NSEG,ILOLD,
     3 IFCRIDOLD,NFF,IFRM,IERR
      INTEGER EFFTEMP, NCTEMP
      LOGICAL COMPTEMP     
      INTEGER INRDR(MXNRDT),INRDU(MXNRDT)     
      DOUBLE PRECISION FSLEN,RCHLEN,COLD,COUNT,ASUM,BSUM,C,             !FORMERLY IMPLICIT REAL
     1 DWSUM,QMAX,HSUM,GSUM,HL,AREA,AREASUM,GWLIFTATSURFACE,SWLIFT,
     2 DIVSEG,DIVPER,DIVSEGOLD,DIVPEROLD,SWRED,QSUMDEF,QSUMEXC,
     3 QDEFICIENCY,QEXCESS,FDSLEN,RRUNOFF,Q,QSUMF
      DOUBLE PRECISION HH,SS,XX,YY,PCTG,TF,ND,NW,QR,SW,CCIR,DSUM,
     1  SUFDSUM,SURPLUS1,SURPLUS2,RDEL,RDELCOR,AFDELSW,QTOL,TFOLD,EF,
     2  ADSW,QMXF,NRDE,ALLOTDP,FARMALLOT,FLOWIN,GWSUM,SWSUM,NRSUM,AVI,
     3  TDROLD,CIRP,CIR,EFOLD,TMAX,TPOT,GSE,RRZ,TRZ,LXX,MXX,UXX,
     4  PSIA,MLT,DRZ,NEXP,PSI0,PSI1,PSI2,PSI3,PSIWET,PSIDRY,XWET,XDRY,
     5  THI,THD,TACT,EPOT,EHI,EHD,EACT,P,TP,EP,PET,CECT,RMIN,RMAX,RMAXP,
     6  RIRR,RPREC,RHSS,HCOFF,HCOFFOLD,DIG1,DIG2,TI,SWRUNOFF,
     7  SWRUNSUM,SWRUNSUMC,AVGSUPPLYFLR,AVGSUPPLYFLUX,QSUMDF,QSUMEX,QDF,
     8  QEX,THISAT1,THDSAT1,THISAT2,THDSAT2,TGWSAT1,TGWSAT2,ALPHA,
     9  QACT,QMAXMNW2,QMAXMNW3,FIESWI,FIESWP,TG,DRDC,                   !REMOVED RTFUNC AS DOUBLE PRECISION VARIABLE 
     +  GWCOST,SWRCOST,SWNRCOST,GWPROFIT,SWRPROFIT,SWNRPROFIT,YIELD,
     +  BENEFIT,FLUX,CTFDR,SUPPLYFLR,TDRNONFAL,SUPPLYFLROLD,SUPPLYFLRNEW
     +  ,SUPPLYFLRNEWOLD,SWPREV,SWOLDPREV,SWREDPREV,CRDEL
C     ------------------------------------------------------------------ 
      INTEGER:: FIRSTNODE,LASTNODE,WID,FID                              !INTEGER VARIABLES FOR MNW2 LINK seb
      DOUBLE PRECISION::QMNW2DES,QMNW2ACT                               !DP VARIABLES FOR MNW2 LINK seb
C     ------------------------------------------------------------------ 
ccrth Initialize constants used with NWT well smoothing added by rth
      Qp = ZERO
ccrth
      CALL SFMP3PNT(IGRID)
      CALL SGWF2LGR2PNT(IGRID)   !seb lgr
      CALL SGWF2SFR7PNT(IGRID)                                          !necessary as FMP is called before SFR in FM loop
      CALL SGWF2NWT1PNT(Igrid)   !seb lgr
      CALL SGWF2UPW1PNT(Igrid)   !seb lgr
      CALL SGWF2UZF1PNT(Igrid)   !seb lgr
      CALL SGWF2MNW2PNT(IGRID)   !seb lgr
      CALL SGWF2SFR7PNT(Igrid)   !seb lgr
      CALL SGWF2MNW1PNT(Igrid)   !seb lgr
C
      IF(ILGR.NE.0) THEN  
        IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) THEN
          CALL SGWF2SFR7PNT(Igrid)
          IUNITSFR=1
        ENDIF
      ENDIF
!      IF(IUNITMNW2>0)THEN
!      WRITE(*,'(A)')
!     +'   SP  TS  IT   QMAXZ         QRATE         QDES           QNET'
!      DO I=1,MNWMAX  
!        FIRSTNODE=IDINT(MNW2(4,I))
!        LASTNODE =IDINT( MNW2(4,I) + ABS(MNW2(2,I)) - 1D0 )
!        QMNW2ACT=SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
!        WRITE(*,'(3I4,5ES14.4)')KPER,KSTP,KITER,
!     +   FWELL(6,10+I),FWELL(NFWLVL,10+I),MNW2(5,I),MNW2(18,I)
!      END DO
!      END IF
      
C
C1===== ADVANCE ISTARTFL FLAG BY ONE =============================================================
C
c       1. ISTARTFL=-1 INDICATES NEW CONSUMPTIVE USE CONDITION IN FM:
c
c          ISTARFL=-1 is advanced to ISTARTFL=0 to skip 1st FMP3FM iteration (see comment C4 below) 
c          and to solve MF2K5 without the FMP but with SFR in order to get good initial "pre-farming"
c          streamflow values for the 2nd iteration that are not biased by previous head-dependent 
c          consumptive use conditions, which can influence the initial streamflow.
c
c       2. ISTARTFL=0 INDICATES FIRST FMP3FM ITERATION:
C 
C          If ISTARTFL=0 is advanced to ISTARTFL=1 then the 2nd MF2K5, but first FMP3FM iteration occurs.
c          ISTARTFL=0 is advanced to ISTARTFL=1 and the 2nd MF2K5, but first FMP3FM iteration.
c          Hence, ISTARTFL=0 tells when to reset some parameters at the beginning of the first FMP3FM
c          iteration (e.g., to reset reduced cell-area percentages to allow a new acreage-optimization)
c
      ISTARTFL=ISTARTFL+1
      !IF(LSTCHK(3)) THEN
      !  write(iout,*) "TIMESTEP FLAG ... =",ISTARTFL,KPER,KSTP,KITER
      !ENDIF
C
      IF(IUNITSFR.GT.0) THEN
C
C2===== FOR DRAINS ASSOCIATED WITH A FARM: RESET RUNOFF TO SPECIFIED RUNOFF =================================
c
c       (Surface-water runoff to drains is always present as long as    !drain reaches can contain cumulative runoff from several farms (inside farm / outside farm)
c        iunitsfr.gt.0; even if IRDFL.eq.0!                             !therefore: drain runoff is cumulative! However, don't carry over runoff from previous iteration.
      DO NF=1,NFARMS                                                    !therefore: reset runoff to stress preriod pre-specified runoff!!
      IF(IFA(NF).EQ.0) GOTO 113
      do l=1,nstrm                                                      !for inside farm: strm(12,l)=strm(12,l)+RRUNOFF / for outside farm: strm(12,l)=strm(12,l)+SWRUNSUM
      IF(IFDRID(NF,L).EQ.NF) THEN                                       !IFDRID CONTAINS LOCATIONS OF SFR SEGMENT/REACH THAT IS ALTERED BY FMP seb
      strm(12,l)=SNGL(sfradd(l))                                        !(info: actual deliveries out of canal reaches - to the contrary -
      endif                                                             !are not cumulative; a delivery out of a canal reach is diverted only to a unique farm!
      enddo                                                             !advantage: head-dependency better preserved; number of iterations is less and budget is better!
  113 enddo                                                             !strm(12,l)=sfradd(l)-RDEL 
C
C3===== FOR PRIOR APPROPRIATION: RESET IRDRFL(NF), RDR(L), AND SEG(2,NSEG) ==================================
C
      if(iallotsw.GE.2) then                                              !IRDRFL(NF)=0: tells to allow PRIOR for farm NF; IRDRFL(NF)=1 tells that PRIOR was solved for a farm NF.
         if((IRDFL.NE.0.OR.ISRDFL.GT.0).and.istartfl.eq.0) then         !- Set IRDRFL=0 only AT BEGINNING of new timestep to allow PRIOR for all farms within the time step!
            DO NF=1,NFARMS                                              !- Do not reset IRDRFL to zero for all farms, if ISTARTFL > 0 indicating that the process of
               IRDRFL(NF)=0                                             !  "cumulating PDIV, exiting PRIOR & FMP3FM, solving MF2K5, and restarting FMP3FM" is still ongoing
               QSTRMIN(NF)=0.D0
            ENDDO                                                       !  during the present time step! 
C                                                                       !  (After cumulating PDIV, routines PRIOR and FMP3FM are exited if there was no solution achieved. 
            do l=1,nstrm                                                !   Before solving the gwf-equation in MF2K5, ISTARTFL is set zero, and ISTARTFL=1 when FMP3FM restarts).
               rdr(l)=0.D0                                             
               strm(12,l)=SNGL(sfradd(l))
               if(idivar(1,istrm(4,l)).gt.0) SEG(2,istrm(4,l))=0.       !- Set potential diversion rate to zero
            enddo                             
         endif                                
         if(ideffl.eq.-2.or.ideffl.gt.0) then 
            if(iter2.ne.2) iter2=0        
            if(iter2.eq.2) iter2=1            
         endif
      endif                                   
      if(iallotsw.lt.2) then
      iter=0                                                            !INITIALIZE
      iter2=0                                                           !INITIALIZE
      endif
C
      ENDIF
C
C4===== SOME SETTINGS AND INITIALIZATIONS ======================================================================
C

c-----DISABLE PARENT FARMS AND ALL ASSOCIATED PROCESSES WHERE CHILD MODELS EXIST.
C     (THIS COULD SIT IN SOME FORM IN FMP3RQ ... BUT REQUIRES FMP TO BE IN NAME FILE OF CHILD MODEL)
      IF(ILGR.NE.0.AND.LGRITER.EQ.1.AND.IGRID.EQ.1) IFID=IFIDOLD
      IF(ILGR.NE.0.AND.LGRITER.GT.1.AND.IGRID.EQ.1) THEN
        DO IR=1,NROW
        DO IC=1,NCOL
           DO N=1,NGRIDS
             IF(IR.GE.LGRDAT(N)%NPRBEG.AND.IR.LE.LGRDAT(N)%NPREND.AND.
     1          IC.GE.LGRDAT(N)%NPCBEG.AND.IC.LE.LGRDAT(N)%NPCEND) THEN
                IFID(IC,IR)=0
             ENDIF
           ENDDO
        ENDDO
        ENDDO
      ENDIF


C4A-----RESET EFFICIENCY FROM CALCULATED EFFICIENCY TO SPECIFIED EFFICIENCIES AT BEGINNING OF EACH ITERATION SCOTT THIS LOOP HAS MANY REDUNDANCIES
c       (SAME AS PER TIME STEP) 
114   DO NF=1,NFARMS                          
      IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                              !seb ADDED LFID TO SKIP FARMS NOT IN USE DURING SP
      EFFTEMP = INT(EFF(1,NF))
      DO NC=2,1+NCROPS                                                  !NOT FOR NCROPS+3 AND NCROPS+4 (NONZERO CALCULATED VALUES IF IEBFL=2)
C SRP (10/23/2012) - CODE OPTIMIZATION
         NCTEMP = NC-1
         COMPTEMP = NC.GT.2.AND.EFF(NC,NF).EQ.0D0
         DO IR=1,NROW                         
         DO IC=1,NCOL                         
            IF(COMPTEMP) THEN
              IF(IFID(IC,IR).EQ.EFFTEMP) THEN
              EF2D(IC,IR)=EFF(2,NF)
              ENDIF
            ELSE
              IF(IFID(IC,IR).EQ.EFFTEMP.AND.
     1        ICID(IC,IR).EQ.NCTEMP) THEN
              EF2D(IC,IR)=EFF(NC,NF)
              ENDIF
            ENDIF
         ENDDO
         ENDDO
      ENDDO
      ENDIF
      ENDDO
C
C4B-----INITIALIZE SOME VARIABLES 
      ND=0.D0
      IOPTFL=0
      SURPLUS1=0.D0
      SURPLUS2=0.D0
      IF(
     +   ( (IUNITSFR.GT.0)                                              !seb REORGANIZED IF FOR READABILITY
     +.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0)
     +.AND.ISTARTFL.EQ.0)
     +.OR.
     +   ( 
     +    ((IUNITSFR.EQ.0)
     +.OR. (IRDFL.EQ.0.AND.ISRDFL.EQ.0))
     +.AND. ISTARTFL.EQ.1
     +   )
     +  ) THEN
        TFDR=0.D0                                                       !seb FORTRAN95 STYLE OF INITIALIZATION
        TFDROLD=0.D0
        NRD=0.D0
        QREQ=0.D0
        QREQOLD=0.D0        
!        DO NF=1,NFARMS      
!        TFDR(NF)=0.D0
!        TFDROLD(NF)=0.D0
!        NRD(1,NF)=0.D0
!        NRD(2,NF)=0.D0
!        QREQ(NF)=0.D0
!        QREQOLD(NF)=0.D0
!        ENDDO
      ENDIF      
      WHERE (ICID.EQ.0)                                                 !seb SWITCHED DO LOOPS FOR WHERE CONSTRUCT
        TTOT=0D0
        TGWA=0D0
        ETOT=0D0
        EGWA=0D0   
      END WHERE
!      DO IR=1,NROW
!      DO IC=1,NCOL
!      IF(ICID(IC,IR).EQ.0) THEN
!      TTOT(IC,IR)=0.D0
!      TGWA(IC,IR)=0.D0
!      ETOT(IC,IR)=0.D0
!      EGWA(IC,IR)=0.D0      
!      ENDIF
!      ENDDO
!      ENDDO
C
C-----MNW2 LINK SET QDES AND QACT FOR KITER=1
      IF(IUNITMNW2.GT.0 .AND. KITER.EQ.1 .AND. NFWELS.GT.0) THEN        !SET QACT and QDES for MNW2 on first interation
        DO L=1,NFWELS
          FID=IDINT(FWELL(5,L))
          WID=IDINT(FWELL(4,L))
          IF(WID.GE.0 .OR. .NOT.LFID(FID)) CYCLE                        !SKIP FARM WELLS THAT ARE NOT LINKED TO MNW2 AND FARM WELLS ASSOCIATED WITH INACTIVE FARMS
          J=MNW2LOC(L)
          FIRSTNODE=IDINT(MNW2(4,J))
          LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )           !J=MNW2LOC(L) TRANSLATES FARM WELL LOCATION TO MNW2 WELL LOCATION
          MNW2(5,J)=0D0
          MNWNOD(4,FIRSTNODE:LASTNODE)=0D0
        ENDDO
      END IF
C4C-----SKIP 1ST FMP3FM-ITERATION
      if(istartfl.le.0) GO TO 300
C
C4D-----INITIALIZE SOME LOCAL VARIABLES
      FARMALLOT=0.D0
C
C5===== UPDATE QMAXF WHEN QMAXRESET FLAG OCCURS ======================================= THIS USED TO BE FOR FOR MNW IF QMAX OF MNW-WELLS ITERATIVELY CHANGES NOW CAN APPLY TO ALL WHENEVER QMAXRESET IS IN USE FOR NWT SMOOTHED PUMPING
C       (ONLY IN FM WHEN QMAXRESET FLAG IS SET TO TRUE. -- QMAX NORMALLY DONE IN RP)
       IF((ISTARTFL.EQ.1.OR.KITER.EQ.1).AND.                            !IF TRUE THEN "GO TO" WILL SKIP REMAINING MNW SECTIONS
     +     FWLAUXORDER(1).EQ."QMAXRESET") THEN
        QMAXF=0D0                                                       !RESET QMAXF (MAX RATE FOR FARM)
        DO L=1,NFWELS
          FID=IDINT(FWELL(5,L)) 
          IF(LFID(FID)) THEN
            IF(AUXV(1,L).EQ.1) THEN
                FWELL(6,L)=FWELL(7,L)                                   !WELL CONATAINS FLAG TO RESET QMAX WHICH IS STORED IN FWELL(7,L)
                IF(IALLOTGW.NE.0) FWELL(6,L)=FWELL(6,L)* QFRACF(FID)    !REDUCE EACH FARM WELL MAX RATE TO SCALE EQUALLY TO MEET ALLOTTED TOTAL
            END IF    
C           FOR NON-MNW WELLS USED TOGETHER WITH MNW-WELLS TO RESET QMAXF, MAKE SURE QMAX=0 FOR IBOUND=0               
            IR=IDINT(FWELL(2,L))
            IC=IDINT(FWELL(3,L))
            IL=IDINT(FWELL(1,L)) 
            IF(IL.GT.0) THEN
              IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
            ENDIF
            QMAXF(FID)=QMAXF(FID)+FWELL(6,L)                            !REBUILD QMAXF WITH NEW VALUES
          END IF
        ENDDO                                ! L=1,NFWELS 
cc
ccrth
!!!         IF(IALLOTGW.GT.0)THEN
!!! C seb QMAXF HAS BEEN UPDATED, IF THERE ARE ALLOTMENTS OVERWRITE QMAX WITH ALLOTTED GW VALUE
!!! !         WHERE(LFID .AND. QMAXF.GT.ALLOTGW(2,:))                        !ASSIGN VALUE WHERE LFID IS TRUE
!!! !           QMAXF=ALLOTGW(2,:)                                           !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!! !         END WHERE
!!! !         WHERE(.NOT. LFID)                                    !ASSIGN VALUE WHERE LFID IS TRUE
!!! !           QMAXF=0d0                                          !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!! !         END WHERE    
!!! !        END IF                                                           ! End of block for IALLOTGW
!!!         DO NF=1,NFARMS
!!!          IF(LFID(NF))THEN
!!!              IF(QMAXF(NF)>ALLOTGW(2,NF)) QMAXF(NF)=ALLOTGW(2,NF)         !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!!          ELSE
!!!              QMAXF(NF)=0D0
!!!          ENDIF
!!!         ENDDO
!!!        END IF
cc        
        GO TO 1000                                                      !SKIP THE MNW ROUTINES BECAUSE QMAXF WAS RESET
       END IF
C-----MNW1 LINK
      IF(IUNITMNW1.GT.0 .AND. KITER.GT.2 ) THEN                         !WELL2(3,N)=qact is not calculated in K=1. I.e., comparison between Well2(2 and Well2(3 is not yet possible in next iteration K=2 (FMP comes first). So first comparison is possible for K>2
C-----FIND LAYER NUMBER FOR SINGLE-NODE MNW WELL AND OVERWRITE A USER-SPECIFIED ZERO LAYER NUMBER (WHICH NORMALLY INDICATES MULIT-NODE MNW WELL)      
      DO NF=1,NFARMS
        IF(.NOT.LFID(NF)) CYCLE                                         !seb ADDED LFID TO SKIP FARMS THAT ARE NOT IN USE FOR SP
        QSUMF=0.D0
        DO L=1,NFWELS
         IF(IDINT(FWELL(5,L)).EQ.NF) THEN
          IR=IDINT(FWELL(2,L))
          IC=IDINT(FWELL(3,L))
          IL=IDINT(FWELL(1,L))          
          IF(IDINT(FWELL(4,L)).LT.0) THEN
            NOD=0
            IF(IDINT(FWELL(4,L)).LT.0) THEN
              ISNGLWELL=0
              DO N=1,NWELL2
                if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then
                  if(n.eq.1)then
                    m=IDINT(well2(1,n))
                    j = int(mod((m-1),ncol*nrow)/ncol) + 1
                    i = mod((m-1),ncol) + 1
                    if(ir.eq.j.and.ic.eq.i) then
                    ISNGLWELL=ISNGLWELL+1
                    IL=int((m-1)/(ncol*nrow))+1
                    endif           
                  endif
                  if(n.gt.1)then
                    if(well2(8,n-1).lt.1D30)then
                      m=IDINT(well2(1,n))
                      j = int(mod((m-1),ncol*nrow)/ncol) + 1
                      i = mod((m-1),ncol) + 1
                      if(ir.eq.j.and.ic.eq.i) then
                      ISNGLWELL=ISNGLWELL+1
                      IL=int((m-1)/(ncol*nrow))+1
                      endif
                    endif 
                  endif
                endif
              ENDDO
              IF(ISNGLWELL.GT.1) THEN
                IF(LSTCHK(1)) THEN
                  WRITE(IOUT,112) ISNGLWELL,IR,IC,IDINT(FWELL(4,L))
                ENDIF
112             FORMAT(/,1X,'THERE ARE 'I3,' MNW-NODES ASSOCIATED ',
     1          'WITH THE FOLLOWING ROW AND COLUM OF A SINGLE-LAYER'
     2          ,' FMP-WELL LOCATION:',/,1X,
     3          'ROW: ',I6,', COL.: ',I6,', FARM-WELL ID: ',I6,/,1X,
     4         'YOU HAVE TWO OPTIONS TO CORRECT THE ERROR:',/,2X,
     5          'A.) INSTEAD OF A ZERO LAYER NUMBER, ENTER THE LAYER ',
     6          'NUMBER OF THE MNW-NODE LINKED TO THE FMP-WELL INTO ',
     7          'THE FMP WELL-LIST.',/,2X,
     8          'B.) DELETE ALL BUT ONE MNW-NODE ENTRIES WITH ',
     9          'THE SAME ROW AND COLUM.') 
                STOP
              ELSEIF(ISNGLWELL.EQ.1) THEN
                FWELL(1,L)=IL
              ENDIF           
C         
              NOD=(IR-1)*NCOL + IC
              QMAXMNW2=0.D0
              QMAXMNW3=0.D0           
              DO N=1,NWELL2
              DO M=1,NLAY
                IF(IDINT(WELL2(1,N))-(M-1)*NROW*NCOL.EQ.NOD) THEN
                  QMAXMNW2=QMAXMNW2+WELL2(2,N)  !Qdes
                  QMAXMNW3=QMAXMNW3+WELL2(3,N)  !Qact
                ENDIF
              ENDDO
              ENDDO
C             RESET MAXIMUM CAPACITY FOR MULTI-NODE WELL IF CUMULATIVE PUMPAGE OF CURRENT ITERATION 
C             IS LESS THAN THE ONE OF PREVIOUS ITERATION BUT NOT LESS THAN ZERO (Q-OUT: NEGATIVE SIGNS)
C             (NET INFLOW BY INTRA-BORE FLOW NOT ALLOWED TO FALSELY RESET QMAX)
              IF(FWELL(NFWLVL,L).LT.0.D0.AND.QMAXMNW3.LT.-FPS.AND.
     1           QMAXMNW3.GT.QMAXMNW2) THEN
              FWELL(6,L)=-QMAXMNW3
              ENDIF          
            ENDIF
C         
111         IF(IL.GT.0) THEN
              NOD=(IL-1)*NROW*NCOL + (IR-1)*NCOL + IC
              DO N=1,NWELL2
C               RESET MAXIMUM CAPACITY FOR SINGLE-NODE WELL IF PUMPAGE OF CURRENT ITERATION IS LESS
C               THAN THE ONE OF PREVIOUS ITERATION BUT NOT LESS THAN ZERO (Q-OUT: NOTICE NEGATIVE SIGNS)
C               (NET INFLOW BY INTRA-BORE FLOW NOT ALLOWED TO FALSELY RESET QMAX)            
                IF(IDINT(WELL2(1,N)).EQ.NOD.AND.
     1            FWELL(NFWLVL,L).LT.0.D0.AND.WELL2(3,N).LT.-FPS.AND.
     2            WELL2(3,N).GT.WELL2(2,N))THEN
                  FWELL(6,L)=-WELL2(3,N)
                ENDIF
              ENDDO
            ENDIF
C         
          ENDIF
C                
C         FOR NON-MNW WELLS USED TOGETHER WITH MNW-WELLS TO UPDATE QMAXF, MAKE SURE QMAX=0 FOR IBOUND=0          
          IF(IL.GT.0) THEN
            IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
          ENDIF
C         
          QSUMF=QSUMF+FWELL(6,L)
         ENDIF
        ENDDO
        QMAXF(NF)=QSUMF
      ENDDO
      ENDIF
C
C
C-----MNW2 LINK CHECK IF MNW2 CAN PROVIDE DESIRED PUMPING FOR FARM WELLS FOR KITER>=3
      IF(IUNITMNW2.GT.0 .AND. KITER.GE.3  .AND. NFWELS.GT.0) THEN       !MNWNOD(4,FIRSTNODE:LASTNODE)=qact is not calculated in K=1 or 2. I.e., comparison between MNWNOD(4,FIRSTNODE:LASTNODE) and MNW2(5,J) is not yet possible in next iteration K=2 (FMP comes first). So first comparison is possible for K>2
        DO L=1,NFWELS
         FID=IDINT(FWELL(5,L))
         WID=IDINT(FWELL(4,L))
         IF( LFID(FID) .AND. WID.LT.0 ) THEN                            !ONLY PROCESS MNW2 LINKED WELLS THAT ARE ASSOCIATED WITH ACTIVE FARMS.
           J=MNW2LOC(L)
           FIRSTNODE=IDINT(MNW2(4,J))
           LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )          !J=MNW2LOC(L) TRANSLATES FARM WELL LOCATION TO MNW2 WELL LOCATION
           QMNW2DES=MNW2(5,J)
           QMNW2ACT=SUM(MNWNOD(4,FIRSTNODE:LASTNODE))                   !SUM ALL RATES FROM EACH NODE TO GET ACTUAL PUMPING RATE
C          RESET MAXIMUM CAPACITY FOR MULTI-NODE WELL IF CUMULATIVE PUMPAGE OF CURRENT ITERATION 
C          IS LESS THAN THE ONE OF PREVIOUS ITERATION BUT NOT LESS THAN ZERO (Q-OUT: NEGATIVE SIGNS)
C          (NET INFLOW BY INTRA-BORE FLOW NOT ALLOWED TO FALSELY RESET QMAX)
           IF(FWELL(NFWLVL,L).lT.0.D0.AND.QMNW2ACT.LT.-FPS.AND.
     +        QMNW2ACT.GT.QMNW2DES) THEN
            FWELL(6,L)=-QMNW2ACT
           ENDIF
         ELSEIF(.NOT.LFID(FID) .AND. WID.LT.0)THEN
           FWELL(6,L)=0D0
         END IF
        ENDDO
       
        QMAXF=0.D0                                                      !RECALCUATE FARM MAX PUMPING RATES --LOOP CAN BE COMBINED WITH PREVIOUS LOOP TO CONDENSE CODE, BUT MAY NOT OPTIMIZE AS WELL
        DO L=1,NFWELS
         FID=IDINT(FWELL(5,L))
         IF(LFID(FID)) QMAXF(FID)=QMAXF(FID)+FWELL(6,L)                 !ONLY PROCESS WELLS ASSOCIATED WITH ACTIVE FARMS
        END DO
      ENDIF
C
C
C6===== DETERMINE ACTUAL TRANSPIRATION FROM GROUNDWATER FOR EACH FARM CELL, AND               ===============
C       DETERMINE CROP IRRIGATION REQUIREMENT & TOTAL DELIVERY REQUIREMENT FOR EACH FARM CELL
C       =====================================================================================
C
C6A-----TRANSPIRATION FROM HIGHEST INTERNAL CELL
C
1000  DO IR=1,NROW
      DO IC=1,NCOL
      DO IL=1,NLAY
      DRDC=DBLE(DELR(IC)*DELC(IR))
C
C6A1----FOR ALL CELLS IN A FARM
      IF(IFID(IC,IR).GT.0) THEN
C
C6A2----FOR ALL ACTIVE CROP CELLS
      IF(ICID(IC,IR).GT.0) THEN
C
C6A3----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION (DELETED IN FMP3).
C       (not excluded anymore: ET is now allowed to depend on a constant head and 
C        a constant head cell is considered aquifer that can receive recharge:
C        FNRCH is calculated and Deep Percolation and ETgw are used for Farm Budget
C        but FNRCH source/sink term is not applied to RHS and HCOFF for constant head)
C       IF(IBOUND(IC,IR,IL).LT.0) GO TO 50
C
C6A4----IF CELL IS INACTIVE MOVE DOWN A LAYER.
C       (excluded only for no-flow cells of any non-bottom layer, but not excluded anymore 
C        for no-flow cells of the bottom layer: If any layer is IBOUND>0 below another
C        layer with IBOUND=0 then that layer is the uppermost active layer receiving FNRCH.
C        If all layers down to the bottom layer are IBOUND=0, we assume a bedrock formation
C        and Deep percolation is switched to zero and all inefficient losses will all be sent
C        to RUNOFF to avoid losing mass).
      IF(IBOUND(IC,IR,IL).EQ.0) THEN
        IF(IL.NE.NLAY) THEN !.OR.ILGR.GT.0
          TDR(IC,IR)=0.D0
          GO TO 60
        ENDIF
      ENDIF
C 
C6B-----TRANSPIRATION, CROP IRRIGATION REQUIREMENT, & TOTAL DELIVERY REQUIREMENT PER CELL
C
      HH=HNEW(IC,IR,IL)
      TMAX=DNINT(TGW(IC,IR)*AC)/AC                                      !here: TMAX = potential crop transpiration Tc-pot (TGW used for Tc-pot).
      GSE= DNINT(GSURF(IC,IR)*AC)/AC
      TRZ= DNINT(RT2D(IC,IR)*AC)/AC
      SS=GSE-TRZ                          
      PSIA=SOIL2D(1,IC,IR)
      LXX=SS-PSIA
      TP=DNINT(TPPOT(IC,IR)*AC)/AC
      IF(FTE(2,ICID(IC,IR)).LE.0.D0)THEN
       CECT=0.D0                                                        !if transpiration from irrigation is 0, then no T+E components of CIR are needed!
      ELSE
       CECT=(FTE(4,ICID(IC,IR))/FTE(2,ICID(IC,IR)))
      ENDIF
      CIR=0.D0
C
C6B1----DETERMINE TRANSPIRATION AND CROP IRRIGATION REQUIREMENT
C       WITH ROOT WATER UPTAKE STRESS-RESPONSE FUNCTION & SOIL-TYPE PARAMETERS     
      IF(ICCFL.EQ.1.OR.ICCFL.EQ.3) THEN
C
C6B1A---INITIALIZE PARAMETERS WHICH ENTER ANALYTICAL FUNCTION
      TPOT=0.D0
      DRZ=0.D0
      NEXP=0.D0
      PSIDRY=0.D0
      PSIWET=0.D0
      XWET=0.D0
      XDRY=0.D0
      UXX=0.D0
      MXX=0.D0
C
C6B1B---STORE ORIGINAL MAXIMUM-TRANSPIRATION &
C       DETERMINE REDUCTION-PERCENTAGE FROM PREVIOUS ITERATION (FOR ACREAGE-OPTIMIZATION)
      IF(ISTARTFL.EQ.1)       TGWO(IC,IR)=TMAX                          !SET TGWO IN 1ST ITER. & RE-USE IN FOLLOWING ITERATIONS;
      IF(TGWO(IC,IR).GT.0D0)  PCTG=TMAX/TGWO(IC,IR)                     !DETERMINE % REDUCTION WITHOUT STORING A NEW VARIABLE (TMAX here = TGW-reduced).
      IF(TGWO(IC,IR).EQ.0D0)  PCTG=1D0
C
C6B1C---CONVERT FLOWRATE OF ORIGINAL MAXIMUM TRANSPIRATION BACK TO FLUX
C       (REASON: UXX AND MXX BELOW ARE FUNCTIONS OF T-FLUX NOT T-FLOWRATE; WITH:
C        UXX =    Head Elevation of Upper Extinction of Transpiration due to Anoxia
C        MXX =    Head Elevation of Elimination of crop-unproductive Wilting Zone)
      TMAX=TGWO(IC,IR)/DRDC
      TPOT=TMAX                                                         !here:_  TMAX = potential crop transpiration Tc-pot
      RRZ=GSE-HH                                                        !      |_(caution: TMAX is also reused below for Tgw-act-max)
C
C6B1D---DEFINE STRESS RESPONSE FUNCTION FOR UPTAKE FROM UNSATURATED ROOT ZONE:
C       PRESSURE HEADS AT WHICH UPTAKE IS ZERO [PSI0,PSI3] OR OPTIMAL [PSI1,PSI2]
C       (UPTAKE FROM SATURATED ROOT ZONE IN C9B1D --> IRRELEVANT FOR CALCULATION OF IRRIGATION REQUIREMENT)
      IF(PSI(2,ICID(IC,IR)).LT.0.D0) THEN
         PSI0=DABS(PSI(2,ICID(IC,IR)))
      ELSE
         PSI0=0.D0
      ENDIF
      IF(PSI(3,ICID(IC,IR)).LT.0.D0) THEN
         PSI1=DABS(PSI(3,ICID(IC,IR)))
      ELSE
         PSI1=0.D0
      ENDIF
      IF(PSI(4,ICID(IC,IR)).LT.0.D0) THEN                               !seb ADDED 0 INITIALIZATION
         PSI2=DABS(PSI(4,ICID(IC,IR)))
      ELSE
         PSI2=0D0
      END IF
      IF(PSI(5,ICID(IC,IR)).LT.0.D0) THEN
         PSI3=DABS(PSI(5,ICID(IC,IR)))
      ELSE
         PSI3=0D0
      END IF
C
C6B1E---APPLY INSTRINSIC ANALYTICAL SOLUTIONS FOR SPECIFIED SOIL TYPES, OR
C       FORMULATE ANALTYICAL SOLUTIONS USING USER-SPECIFED COEFFICIENTS FOR:
C       DRZ - DEPLETED ROOT ZONE  ( DRZ = FUNCTION(A,B,C,Tc-pot,TRZ) )        
C       N   - SINUOSITY COEFFICIENT ( N = FUNCTION(D,E,DRZ)   )
C
C       (DRZ ORIGINALLY DERIVED IN CM: ADJUSTED TO RESPECTIVE UNITS BY MULTIPLIER)
      IF(LENUNI.EQ.3) MLT=1D0                                           !CENTIMETER
      IF(LENUNI.EQ.2) MLT=100D0                                         !METER
      IF(LENUNI.EQ.1) MLT=30.48D0                                       !FOOT
      IF(TPOT.LE.0.D0) GOTO 51          
      IF(IDINT(SOIL2D(2,IC,IR)).EQ.-999) THEN                           !DEFAULT ANALYTICAL SOLUTION FOR SILT
      DRZ=(DEXP(0.320D0*DLOG(TRZ*MLT)+                        
     1(-0.329D0)*DLOG(TPOT*MLT)+2.852D0))                               !0.320149668    -0.328586793    2.85192125
      NEXP=1.303D0*DLOG(DRZ)-2.042D0                                    !y = 1.3027Ln(x) - 2.0416
      ELSEIF(IDINT(SOIL2D(2,IC,IR)).EQ.-998) THEN                       !DEFAULT ANALYTICAL SOLUTION FOR SANDYLOAM
      DRZ=(DEXP(0.201D0*DLOG(TRZ*MLT)+
     1(-0.195D0)*DLOG(TPOT*MLT)+3.083D0))                               !0.200738267    -0.195485538    3.083110101
      NEXP=3.201D0*DLOG(DRZ)-3.903D0                                    !y = 3.2012Ln(x) - 3.9025
      ELSEIF(IDINT(SOIL2D(2,IC,IR)).EQ.-997) THEN                       !DEFAULT ANALYTICAL SOLUTION FOR SILTYCLAY
      DRZ=(DEXP(0.348D0*DLOG(TRZ*MLT)+
     1(-0.327D0)*DLOG(TPOT*MLT)+1.731D0))                               !0.348098866    -0.327445062    1.730759566
      NEXP=0.530D0*DLOG(DRZ)-0.377D0                                    !y = 0.5298Ln(x) - 0.3767
      ELSE                                                              !READ IN FUNCTION FOR ANALYTICAL SOLUTION
      DRZ=(DEXP(SOIL2D(2,IC,IR)*DLOG(TRZ*MLT)+
     1SOIL2D(3,IC,IR)*DLOG(TPOT*MLT)+SOIL2D(4,IC,IR)))            
      NEXP=SOIL2D(5,IC,IR)*DLOG(DRZ)+SOIL2D(6,IC,IR)
      ENDIF
      IF(DRZ.GT.PSI3*MLT) DRZ=PSI3*MLT
      IF(NEXP.LT.1D-30) NEXP=1D-30
C
C6B1F---EVALUATE PRESSURE HEADS BETWEEN WHICH UPTAKE IS CONSIDERED OPTIMAL
      PSIDRY=(PSI2+PSI3)/2.D0
      PSIWET=(PSI0+PSI1)/2.D0
      IF(PSIWET.EQ.0D0) THEN
      XWET=0.D0
      ELSE
C     
C6B1G---SOLVE ANALYTICAL FUNCTION FOR DEPTHS BETWEEN WHICH UPTAKE IS OPTIMAL:
C       ANALYTICAL FUNCTION: PSI(DEPTH) = FUNCTION (DRZ,NEXP,PSI3,DEPTH)
C                            PSI(DEPTH)   IS SOLVED ITERATIVELY BY BISECTION-METHOD
C                                         FOR DEPTH(PSIWET) AND FOR DEPTH(PSIDRY)
      XWET=RTFUNC(DRZ,PSI3*MLT,NEXP,PSIWET*MLT,EPS,ic,ir)/MLT           ! seb REMOVED VARIABLE j
      ENDIF
      XDRY=RTFUNC(DRZ,PSI3*MLT,NEXP,PSIDRY*MLT,EPS,ic,ir)/MLT
      IF(XWET.LT.0D0) XWET=0D0     
   51 IF(TPOT.LE.0.D0) THEN
      XWET=PSIWET
      XDRY=PSIDRY
      ENDIF
C
C6B1H---DETERMINE: UXX =  Head Elevation of Upper Extinction of Transpiration due to Anoxia
C                  MXX =  Head Elevation of Elimination of crop-unproductive Wilting Zone
      UXX=GSE-XWET
      if(xwet.gt.psiwet) uxx=gse-psiwet
      if(xwet.lt.0d0)    uxx=gse
      if(xwet.gt.trz)    uxx=ss
      MXX=GSE-XDRY      
      IF(xdry.gt.trz)    mxx=ss   
C
C6B1I---DETERMINE MAXIMUM POSSIBLE TRANSPIRATION FROM GROUNDWATER
      TMAX=(TPOT/TRZ)*(UXX-MXX)                                         !here: TMAX = Tgw-act-max = maximum possible transpiration from GW (variable TMAX reused, see above!)
      TMAX=TMAX*DRDC*PCTG
C
C6B1J---EVALUATE TRANSPIRATON FROM GROUNDWATER FOR VARIOUS RANGES OF WATER-LEVEL ELEVATIONS
      IF(HH.GE.UXX) THEN
      TACT=0.D0                                                         !TACT = Tgw-act = 0
      CIR=0.D0
      ENDIF
      IF(HH.GT.MXX .AND. HH.LT.UXX) THEN
      THI=(TPOT/TRZ)*UXX
      THD=TPOT/TRZ
      THI=THI*DRDC*PCTG
      THD=THD*DRDC*PCTG
      TACT=THI-THD*HH                                                   !TACT = Tgw-act < Tgw-act-max < Tc-pot (Tgw-act varies linearly)
      CIR=0.D0                                                          !       Tgw-act = Tc-act
      ENDIF
      IF(HH.GE.SS .AND. HH.LE.MXX) THEN
      TACT=TMAX                                                         !TACT = Tgw-act = Tgw-act-max < Tc-pot
      CIR=(TPOT/TRZ)*(UXX-HH)*DRDC*PCTG-TACT                            !       Tgw-act < Tc-act (Tc-act varies linearly)
      ENDIF
      IF(HH.LT.SS. AND. HH.GT.LXX) THEN
      THI=TMAX*(1-SS/PSIA)
      THD=TMAX/PSIA                                                                                                                             !IF NONLINEAR: THI & THD SEPARATION COULD BE SKIPPED:
      TACT=THI+THD*HH                                                   !TACT = Tgw-act < Tgw-act-max < Tc-pot (Tgw-act varies linearly)        !TACT=TMAX*(1-SS/PSIA+HH/PSIA)**2
      CIR=(TPOT/TRZ)*(UXX-SS)*DRDC*PCTG-TACT                            !       Tgw-act < Tc-act = Tc-act-max = (TPOT/TRZ)*(UXX-SS) < Tc-pot
      ENDIF
      IF(HH.LE.LXX) THEN
      TACT=0.D0                                                         !TACT = Tgw-act = 0
      CIR=(TPOT/TRZ)*(UXX-SS)*DRDC*PCTG                                 !       Tgw-act < Tc-act = Tc-act-max = (TPOT/TRZ)*(UXX-SS) < Tc-pot
      ENDIF
      TGWA(IC,IR)=TACT
C
      ENDIF                                                             !END OF CONDITION: IF(ICCFL.EQ.1.OR.ICCFL.EQ.3)
C
C6B2----DETERMINE TRANSPIRATION AND CROP IRRIGATION REQUIREMENT
C       WITHOUT SOIL AND PLANT PARAMETERS (BUT WITH CAPILLARY FRINGE & ROOTZONE)
      IF(ICCFL.EQ.2.OR.ICCFL.EQ.4) THEN
C
C6B2A---EVALUATE TRANSPIRATON FROM GROUNDWATER FOR VARIOUS RANGES OF WATER-LEVEL ELEVATIONS                        
      IF(HH.GT.GSE) THEN
      TACT=0.D0                                                          !TACT = Tgw-act = 0
      CIR=0.D0
      ELSE IF(HH.GT.SS) THEN                      
      THI=TMAX*(GSE/TRZ)
      THD=-TMAX/TRZ
      TACT=THI+THD*HH                                                   !TACT = Tgw-act < Tgw-act-max = Tc-pot (Tgw-act varies linearly)
      CIR=0.D0                                                          !       Tgw-act = Tc-act
      ELSE IF(HH.LT.LXX) THEN
      TACT=0.0D0                                                        !TACT = Tgw-act = 0
      CIR=TMAX                                                          !       Tgw-act < Tc-act = Tc-act-max = Tc-pot
      ELSE
      THI=TMAX*(1-SS/PSIA)
      THD=TMAX/PSIA
      TACT=THI+THD*HH                                                   !TACT = Tgw-act < Tgw-act-max = Tc-pot (Tgw-act varies linearly)
      CIR=TMAX-TACT                                                     !       Tgw-act < Tc-act = Tc-act-max = Tc-pot
      ENDIF
      TGWA(IC,IR)=TACT
C
      ENDIF
C
C6C-----ADJUST CROP IRRIGATION REQUIREMENT:
C       (1) FOR NON-IRRIGATION CROPS: no irrigation requirement exists,
C       but notice: P, T and E are still active and recharge relevant!
C       (2) USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND
C       ADJUST PRELIMINARY CIR FOR T + E COMPONENTS
      IF(IDINT(CU(3,ICID(IC,IR))).EQ.1.OR.TP.GE.CIR) THEN
C     TPPOT(IC,IR)=CIR                                                 !prescribed Tp should stay, so that it can be compared to CIR, when calculating FNRCH
      CIR=0.D0
      ELSE
      CIR=(CIR-TP)*(1.D0+CECT)
      ENDIF
C
C6D-----UPDATE TOTAL IRRIGATION DELIVERY REQUIREMENT AND HEAD-DEPENDENT EFFICIENCY
C
C6D1----UPDATE TOTAL IRRIGATION DELIVERY REQUIREMENT PER CELL             
c       (IF EFFICIENCY IS KEPT CONSTANT / IF INITAL EFFICIENCY IS RESET TO SPECIFIED EFFICIENCY)
      IF(IEBFL.LE.1.OR.(IEBFL.GT.1.AND.ISTARTFL.EQ.1)) THEN 
      TDR(IC,IR)=CIR/EF2D(IC,IR)
      TDR(IC,IR)=dnint(tdr(ic,ir)*ac)/ac
      ENDIF
C6D2----UPDATE HEAD-DEPENDENT EFFICIENCY PER CELL (IF TDR IS KEPT CONSTANT)
      IF(IEBFL.GT.1.AND.ISTARTFL.GT.1) THEN
      IF(CIR.GE.TDR(IC,IR))  TDR(IC,IR)=CIR                             !TDR VARIES WITH CIR IF IT EXCEEDS CIR
      IF(CIR.GT.0.D0.AND.TDR(IC,IR).GT.0.D0) EF2D(IC,IR)=CIR/TDR(IC,IR) !TDR KEPT CONSTANT
      ENDIF
C
      ENDIF                                                             !ICID IF CONDITION
C
C6E-----FOR FALLOW SEASONS (NO CROP ROTATION) & FOR FALLOWED CELLS (CROP ROTATION):
C       all transpiratory components of CU are zero, and no irrigation requirement exists,
C       but notice: P and E are still active and recharge relevant!
C       (different from non-irrigation crops, where P, T and E are still active)
      IF(IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1) THEN
      CU2D(IC,IR)=0.D0
      TACT=0.D0
      TGWA(IC,IR)=0.D0                 
      TPPOT(IC,IR)=0.D0
      TTOT(IC,IR)=0.D0
      CIR=0.D0
      TDR(IC,IR)=0.0D0
      ENDIF
C
      ENDIF                                                             !IFID IF CONDITION
C
      GO TO 50
   60 ENDDO
   50 ENDDO
      ENDDO
C
C6F-----SET QMAX TO ZERO FOR WELLS WHOSE CELLS HAVE A ZERO CROP IRRIGATION REQUIREMENT
C       AND RESET QMAX BACK TO DEFAULT VALUE AT FIRST ITERATION OF EACH TIME STEP
C
      IF(FWLAUXORDER(2).EQ."NOCIRNOQ") THEN                             ! seb USE STATIC ASSIGNMENT RATHER THEN LOOP THROUGH
       QMAXF=0.D0                                                       !RECALCUATE FARM MAX PUMPING RATES
       DO L=1,NFWELS
        FID=IDINT(FWELL(5,L))
        IF( LFID(FID) ) THEN                                            !PROCESS WELLS ASSOCIATED WITH ACTIVE FARMS
          IR=IDINT(FWELL(2,L))
          IC=IDINT(FWELL(3,L))
          IL=IDINT(FWELL(1,L)) 
          !
          IF(AUXV(2,L).EQ.1 .AND. (ISTARTFL.EQ.1.OR.KITER.EQ.1)) THEN 
              FWELL(6,L)=FWELL(8,L)                                     !WELL CONATAINS FLAG FOR NOCIRNOQ TO RESET QMAX AT EACH TIME STEP WHICH IS STORED IN FWELL(8,L)
              IF(IALLOTGW.NE.0) FWELL(6,L)=FWELL(6,L)* QFRACF(FID)      !REDUCE EACH FARM WELL MAX RATE TO SCALE EQUALLY TO MEET ALLOTTED TOTAL
          END IF
              
          IF(AUXV(2,L).EQ.1 .AND. TDR(IC,IR).LT.FPS) 
     +                                           FWELL(6,L)=0.D0        !SET QMAX TO ZERO FOR WELLS WHOSE CELLS HAVE A ZERO CROP IRRIGATION REQUIREMENT
C         WHEN RESETTING QMAX TO DEFAULT VALUE AND THEN RECOMPUTING QMAXF, MAKE SURE QMAX=0 FOR IBOUND=0               
          IF(IL.GT.0) THEN
            IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
          ENDIF
          QMAXF(FID)=QMAXF(FID)+FWELL(6,L)
        ENDIF
       ENDDO
      END IF
!      IF(FWLAUXORDER(2).EQ."NOCIRNOQ") THEN                            ! seb USE STATIC ASSIGNMENT RATHER THEN LOOP THROUGH
!       DO NF=1,NFARMS                                                  !LOOP CAN BE MERGED WITH PREVIOUS ONE SCOTT
!        IF(.NOT.LFID(NF)) CYCLE
!        QSUMF=0.D0
!        DO L=1,NFWELS
!         ID=IDINT(FWELL(5,L))
!         IF(ID.EQ.NF)THEN
!           IR=IDINT(FWELL(2,L))
!           IC=IDINT(FWELL(3,L))
!           IL=IDINT(FWELL(1,L))
!           IF(AUXV(2,L).EQ.1 .AND. TDR(IC,IR).LT.FPS) FWELL(6,L)=0.D0   
!C          FOR WELLS IN CELLS WITH TDR>0 USED TOGETHER WITH WELLS IN CELLS WITH TDR=0 TO RECOMPUTE QMAXF, MAKE SURE QMAX=0 FOR IBOUND=0               
!           IF(IL.GT.0) THEN
!             IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
!           ENDIF
!           QSUMF=QSUMF+FWELL(6,L)
!         ENDIF
!        ENDDO
!        QMAXF(NF)=QSUMF
!       ENDDO
!      END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C seb ADDED TO FIX POTENTIAL ISSUE WITH ALLOTMENT THIS COVERS POTENTIAL OVERWRITE BY MNW1, MNW2, and "NOCIRNOQ" AUX VARIABLE
!!!!        IF(IALLOTGW.NE.0)THEN
!!!!!         WHERE(LFID .AND. QMAXF.GT.ALLOTGW(2,:))                        !ASSIGN VALUE WHERE LFID IS TRUE
!!!!!           QMAXF=ALLOTGW(2,:)                                           !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!!!!         END WHERE
!!!!!         WHERE(.NOT. LFID)                                    !ASSIGN VALUE WHERE LFID IS TRUE
!!!!!           QMAXF=0d0                                          !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!!!!         END WHERE    
!!!!!        END IF                                                           ! End of block for IALLOTGW
!!!!        DO NF=1,NFARMS
!!!!         IF(LFID(NF))THEN
!!!!             IF(QMAXF(NF)>ALLOTGW(2,NF)) QMAXF(NF)=ALLOTGW(2,NF)         !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!!!!         ELSE
!!!!             QMAXF(NF)=0D0
!!!!         ENDIF
!!!!        ENDDO
!!!!       END IF
C
C6G-----WELL-FIELD OPTION - ALLOWS A SERIES OF IRRIGATED FARMS TO RECEIVE THEIR CUMULATIVE IRRIGATION 
C       DEMAND AS SIMULATED NON-ROUTED DELIVERIES FROM WELL FIELDS SIMULATED AS VIRTUAL FARMS.
C
      IF(IWELLFLD.EQ.1) THEN
      IF(KITER.GT.1)THEN
C
C6G1----CALCULATE TOTAL FARM DELIVERY REQUIREMENT FOR EACH FARM
      DO NF=1,NFARMS
       TFDR(NF)=0.0D0
       DO I=1,FMLOC(NF)%Count                                        !seb removed double do loop  COUNT=0 FOR INACTIVE FARM SO LOOP IS SKIPPED
         IR=FMLOC(NF)%RC(1,I)
         IC=FMLOC(NF)%RC(2,I)
         IF(ICID(IC,IR).GT.0) TFDR(NF)=TFDR(NF)+TDR(IC,IR)
       ENDDO
      ENDDO         
C
C6G2-----FOR EACH WELL FIELD SIMULATED AS NON-ROUTED DELIVERY TYPE
      DO NT=1,MXNRDT      
      CTFDR=0.D0
C
C6G2A----CALCULATE CUMULATIVE DELIVERY REQUIREMENT OF FARMS SUPPLIED BY A PARTICULAR WELL FIELD
C        AND EVALUATE AVAILABLE SIMULATED "NON-ROUTED DELIVERY" FROM THAT WELL FIELD
      DO NF=1,NFARMS
      IF(.NOT.LFID(NF)) CYCLE                                           ! seb SKIP INACTIVE FARMS FOR SP
      IF(TFDROLD(NF).EQ.0D0) TF=TFDR(NF)
      IF(TFDROLD(NF).GT.0D0) TF=TFDROLD(NF)           
         IF(IDINT(UNRD(NT*3+1,NF)).LT.0) THEN
           IF(NT.EQ.1) THEN
             CTFDR=CTFDR+TF
           ELSE
             IF(RNRD(1,NT-1,NF).LT.TF) THEN
             CTFDR=CTFDR+(TF-RNRD(1,NT-1,NF))
             ENDIF
           ENDIF
           IDDELF=IDINT(DABS(UNRD(NT*3+1,NF)))
           RNRD(1,NT,IDDELF)=-MIN(CTFDR,QMAXF(IDDELF))
         ENDIF
      ENDDO
C
C6G2B----RE-DISTRIBUTE NON-ROUTED DELIVERY FROM WELL FIELD OVER TO RECCEIVING FARMS WEIGHTED BY EACH FARM'S 
C        TOTAL DELIVERY REQUIREMENT (OR RESIDUAL DELIVERY REQUIRMENT FOR LOWER PRIORITY WELL FIELDS)
      DO NF=1,NFARMS
      IF(.NOT.LFID(NF)) CYCLE                                           ! seb SKIP INACTIVE FARMS FOR SP
      IF(TFDROLD(NF).EQ.0D0) TF=TFDR(NF)
      IF(TFDROLD(NF).GT.0D0) TF=TFDROLD(NF)         
         IF(CTFDR.GT.0) THEN
           IF(IDINT(UNRD(NT*3+1,NF)).LT.0)THEN
             IF(NT.EQ.1)THEN
             RNRD(1,NT,NF)=-RNRD(1,NT,IDDELF)*TF/CTFDR   
             ELSE
             RNRD(1,NT,NF)=-RNRD(1,NT,IDDELF)*(TF-RNRD(1,NT-1,NF))/CTFDR
             ENDIF
             RNRD(2,NT,NF)=0.D0
           ENDIF
         ENDIF
      ENDDO
      ENDDO
      ENDIF
      ENDIF
C7B1----SET ACREAGE OF ALL CELLS IN DOMAIN TO -100% (FAKE PERCENTAGE, THAT IS ONLY USED  seb moved outside of MAIN FARM LOOP
C       TO DISTINGUISH NON-FARM CELLS FROM FARM CELLS (WHICH CAN BE BETWEEN 100% AND 0%)
C
      IF(IDEFFL.GE.1) WHERE(IFID.EQ.0) REDPCT=-1.D0              !USE WHERE CONSTRUCT TO SET CELLS WITHOUT A FARM TO -100%
C
C7===== START OF FARMS LOOP =================================================================================
400   NSEGOLD=0
401   DO NFF=1,NFARMS
      IF(.NOT.LFID(NFF)) GOTO  500                                       ! seb SKIP INACTIVE FARMS FOR SP  **NOTE: GOTO 500 IS SAME AS CYCLE
      IF(IFA(NFF).EQ.0) GOTO  500
      NF=IFA(NFF)
C7A-----ONLY FOR PRIOR APPR. AND WATERSTACKING OR ACREAGE-OPT. AND DEFICIENCY RE-RUNS:
C       Unlike in regular cases, where a farm budget is evaluated indivually, farm after farm,
c       for prior appropriation, all farms are evaluated together. That is, the farms loop,
c       needs to be done first, then the entire budget of all farms is corrected
c       for minor inaccuracies,and then an update of farm budget terms is restarted,
c       while running deficiency scenarios (if so specified, and if necessary).
c       HOWEVER, the reiteration of budget terms is limited by the following logic:
c       1.) Budget terms for farms without deficiencies are not updated,
c       2.) The corrected pre-deficency budget terms are reused for farms, which do show deficiencies 
c           (only the deficiency budget is reevaluated after the scenario).
c       3.) Some farms showed deficiencies, but do no longer show deficiencies after they benefit 
c           from increased streamflow due to post-scenario reduction of the demand of the
c           upstream senior farm to less than the available supply (pior appropriation no longer holds, 
c           and senior farms' policy makes water available downstream):
c           The budget of such farms will be reevaluated (they are "exempted from reusing previous terms")         
c
C       After pre-deficiency run of all farms (iter2=0),
C       pre-deficiency budget was corrected and saved (and iter2 was set 1)
      IF(IALLOTSW.GE.2.and.(ideffl.eq.-2.or.ideffl.gt.0)
     1  .and.iter2.eq.1)then
C
C7A1----Re-run PRIOR with all farms for deficiency,
C       but don't re-run PRIOR with farms that do not have any deficiency
C       (uncomment GOTO 80 to improve GW-budget; comment out to improve farm-budget)  
C
        IF((ideffl.eq.-2.or.ideffl.gt.0).and.
     1    qreq(nf)-qmaxf(nf).le.0.D0) then
          goto 80
        endif
C
        if(ideffl.gt.0) then
C
C7A2----Check if previous farm experienced a reduction of SW to less than what was available:
C       if so, then the current farm will benefit from increased streamflow downstream from the previous farm.
          swredprev=0.D0
          if(nf.gt.1)then
            if(nfseg(nf).ge.nfseg(nf-1)) then       
              swprev=tfdr(nf-1)-nrd(1,nf-1)-qreq(nf-1)
              swoldprev=tfdrold(nf-1)-nrd(2,nf-1)-qreqold(nf-1)
              swredprev=swoldprev-swprev
            endif
          endif
C
          if(swredprev.lt.PCLOSE.and.iter.eq.0) then
C
C7A3----Save corrected TFDROLD, NRD(2, QREQOLD and indirectly corrected value of AFDELSW
C       for farms that are to be optimized
C       (latter 3 are the corrected supply constraints that go into optimization).
            tf=tfdrold(nf)
            nd=nrd(2,nf)                                              
            qr=qreqold(nf)
C
C7A3----Skip update of TFDR, NRD, AFDELSW, and QREQ if, after a solution of a previous farm (iter=0),
C       a farm does not expect any increase in streamflow after the acreage-reduction (swredprev > PCLOSE)
C       (as a result of SW reduction of previous upstream farm (see C7A2)); 
C       But do not skip update of TFDR, NRD, AFDELSW, and QREQ if, after a solution of a previous farm (iter=0),
C       a farm expects an increase in streamflow after the acreage-reduction.
C
          goto 403
          endif
        endif
      endif
C     
C7B-----UPDATE TOTAL FARM DELIVERY REQUIREMENT      
C       (As long as the TDR is not split into a SW%, GW%, and NW% as a result of acreage-optimization
C        the components from GW, SW, and NW are set to zero and "surface-water domination rule" applies:
C        TF is evaluated and subsequently satisfied by non-routed delivery ND, available SW from a canal,
C        and groundwater pumping (decreasing priority), which are to be determined in C7E)
      TF=0.0D0
      QR=0.0D0
      SW=0.0D0
      NW=0.0D0
      CCIR=0.0D0
      IOPTFL=0
      DO I=1,FMLOC(NF)%Count                                        !seb removed double do loop
        IR=FMLOC(NF)%RC(1,I)
        IC=FMLOC(NF)%RC(2,I)
C
C7B1----SET ACREAGE OF ALL CELLS IN DOMAIN TO -100% (FAKE PERCENTAGE, THAT IS ONLY USED  COMMENTED OUT AND MOVED OUTSIDE OF MAIN FARM LOOP
C       TO DISTINGUISH NON-FARM CELLS FROM FARM CELLS (WHICH CAN BE BETWEEN 100% AND 0%)
C
!           IF(IDEFFL.GE.1.AND.IFID(IC,IR).EQ.0) REDPCT(IC,IR)=-1.D0
C
C7B2----CUMULATE TOTAL FARM DELIVERY REQUIREMENT OVER ALL CELLS IN A FARM
        IF(ICID(IC,IR).GT.0) THEN
          TF=TF+TDR(IC,IR)
C
C7B3----FOR ACREAGE-OPTIMIZATION: UPDATE TOTAL FARM DELVERY REQUIREMENT AND
C       PARTIAL REQUIREMENTS FROM GW, SW, AND NW
C       After an optimization, the rule of "SW-domination" needs to be dropped, and the "exact" amount
C       of NW,SW,GW has be delivered by ND, diverted from the canal as SW, or pumped from GW.
C       Since the optimization occurred during the last iteration, the demand components NW,SW,GW
C       are head-dependent and related to the previous iteration.
C       With slightly different head in the current iteration, the TDR per cell may change slightly. 
C       The percentage of NW,SW,GW among the reduced irrigation fluxes per cell however is carried over
C       from the last iteration and is used to calculate new values of NW,SW,QR demand.
          IF(IDEFFL.GE.1) THEN
C       
C7B3A---If prior appr. solution was not achieved in previos iteration,
C       then --> exit --> new iteration is not a new beginning
            if(iallotsw.GE.2.and.iter2.eq.1.and.iter.eq.1) goto 402
C
C7B3B---Dependent on whether the timestep-AD routine is activated, the percentages of GW,SW,or NW          
C       among the reduced irrigation fluxes per cell is reset to zero at the beginning of each         
C       timestep or stress period, i.e. new TDR flowrates per cell are calculated and, if needed, 
C       newly optimized! If no FMP-AD is activated (ISTARTFL > 1), then the GW%,SW%, & NW% from 
C       the previous iteration coming from the previous timestep are reused to calculate
C       new head-dependent demand component values of GW, SW, NW.
            IF(ISTARTFL.EQ.1) THEN
C
C              INITIALIZE ACREAGE OF FARM CELL AS 100 % --> needed to output 2D arrays of REDPCT.  
C              (needs to be here and not within acreage-optimization section. reason:
C              for iallotsw>=2, all farms are run prior to acreage optimization)
               REDPCT(IC,IR)=1.D0
C
C              GW,SW,NW %ages will be reused as long as                 |
C              the resulting QR,SW,NW are equal or less than            |-> in most cases they ARE about equal 
C              QMAXF or the new SW or ND availability.                  |   in absence of transp. from gw (TGW)!!
C              For the rare case that - due to higher TDRs              |
C              per cell and due to lower streamflow - QR,SW,NW          |
C              demands are higher than the available QMAXF,SW,ND        |-> higher, if TDR in previous iteration  
C              a new optimization will be carried out.                  |   benefits from +TGW (-TDR), and due   
               GWREDPCT(IC,IR)=0D0                                      !   to head drop in next itertation 
               SWREDPCT(IC,IR)=0D0                                      !   TGW goes down (TDR up again!)
               NRREDPCT(IC,IR)=0D0
               ENDIF                                                    
C
C              After an acreage-optimization, new optimized demand GW,SW,NW from groundwater, 
C              surface-water or non-routed deliveries were calculated either equal to the
C              available resource constraints QMAXF,AFDELSW,ND or respectively smaller,
C              due to a low profitability of GW or SW use for each respective cell.
C              But even if the SW demand component was optimized to be equal to AFDELSW
C              during the previous iteration, it still could be slightly less than head-
C              head-dependent SW supply during the next iteration. 
402            IF(GWREDPCT(IC,IR).NE.0D0.OR.SWREDPCT(IC,IR).NE.0D0
     1                                  .OR.NRREDPCT(IC,IR).NE.0D0) THEN
               IOPTFL=1
               QR=QR+TDR(IC,IR)*GWREDPCT(IC,IR)
               QR=DNINT(QR*AC)/AC
               SW=SW+TDR(IC,IR)*SWREDPCT(IC,IR)
               SW=DNINT(SW*AC)/AC
               NW=NW+TDR(IC,IR)*NRREDPCT(IC,IR)
               NW=DNINT(NW*AC)/AC
               TF=QR+SW+NW
               ENDIF
            ENDIF
C
C7B4----ACCUMULATE CIR OVER A FARM TO CALCULATE A HEAD-DEPENDENT EFFICIENCY (CCIR / TFDR) IN C7C
             CCIR=CCIR+EF2D(IC,IR)*TDR(IC,IR)
C
           ENDIF
        ENDDO
      TF=DNINT(TF*AC)/AC
      QR=DNINT(QR*AC)/AC
      SW=DNINT(SW*AC)/AC
      NW=DNINT(NW*AC)/AC
C
C7C-----CALCULATE HEAD-DEPENDENT EFFICIENCY (IF TDR WAS LOCKED OVER A TIME STEP) 
C       save head-dep. efficiency from last iteration --> for closure criterion in FMP3WELBD
        EFF(NCROPS+3,NF)=EFF(NCROPS+2,NF)
        IF(TF.LT.FPS) THEN
        EFF(NCROPS+2,NF)=-999
        ELSEIF(TF.GT.0.d0) THEN                                         !ISTARTFL.GT.1.AND. ?
C       head-dependent efficiency per farm
        EFF(NCROPS+2,NF)=CCIR/TF
        ENDIF
C
C7D-----COMPUTE EQUAL APPROPRIATION ALLOTMENT FLOW RATE PER FARM:
C      (ALLOMENT-DEPTH x FARM AREA)/LENGTH OF STRESS PERIOD
C       AND PULL ALLOTMENT DATA FROM PARENT MODEL IF DESIRED
      IF((IALLOTSW.EQ.1.OR.IALLOTSW.EQ.-1).AND.IUNITSFR.GT.0) THEN  
      AREASUM=0.D0
      DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
        IR=FMLOC(NF)%RC(1,I)
        IC=FMLOC(NF)%RC(2,I)
C       NOT FOR FALLOWED CELLS ICID=-1 (IF FARMER FALLOWS SOME AREA, HE'S GOT MORE FOR THE REST!)
        IF(ICID(IC,IR).GT.0) AREASUM=AREASUM+DBLE(DELR(IC)*DELC(IR))
      ENDDO
      IF(IALLOTSW.EQ.1) THEN
        ALLOTDP=DNINT(ALLOTSW*AC)/AC
      ELSEIF(IALLOTSW.EQ.-1) THEN
        ALLOTDP=DNINT((FMPDAT(1)%ALLOTSW)*AC)/AC
      ENDIF
      FARMALLOT=(ALLOTDP*AREASUM)/DBLE(PERLEN(KPER))
      FARMALLOT=DNINT(FARMALLOT*AC)/AC
C
C------FOR LGR, SAVE FARM ALLOTMENT RATE AS GLOBAL VARIABLE AND ADD THIS VARIABLE
C      FOR CHILD MODEL FARMS OF FARM ID TO PARENT MODEL FARM ALLOTMENT RATE
      IF(ILGR.NE.0)THEN
        FALLOT(NF)=FARMALLOT                                             !NEED GLOBAL VARIABLE FOR FMP-LGR      
        IF(LGRITER.GT.1.AND.IGRID.EQ.1.AND.NGRIDS.GT.1)THEN
          DO N=2,NGRIDS
            IF(N.EQ.LGRDAT(N)%IFMPGRID) THEN
              FARMALLOT=FARMALLOT+FMPDAT(N)%FALLOT(NF)
            ENDIF
          ENDDO
        ENDIF
      ENDIF   
C
      ENDIF
C
C7E-----UPDATE REQUIRED FARM WELL DISCHARGE PER FARM,                  
C       (ONLY GROUNDWATER SUPPLY:           QREQ = TFDR - NRD,
C        SURFACAE WATER SUPPLY PREDOMINANT: QREQ = TFDR - NRD - ACTUAL FARM DELIVERY BY SURFACE WATER)
C     
C7E1----UPDATE NON-ROUTED DELIVERY
C7E1A---UPDATE NON-ROUDED DELIVERY FOR NON-OPTIMIZATION CASES
      SURPLUS1=0.0D0
      SURPLUS2=0.0D0
      IF(INRDFL.EQ.0) ND=0.D0
      IF(INRDFL.NE.0) THEN
C
C     FOR LGR, SCALE RANKED NON-ROUTED DELIVERIES TO PARENT OR CHILD MODEL FARM
C     BY RATIO BETWEEN PARENT OR CHILD FARM DEMAND AND JOINT PARENT+CHILD DEMAND
      IF(ILGR.NE.0.AND.LGRITER.EQ.2.AND.KITER.EQ.1) THEN
C
C     SCALE RANKED NON-ROUTED DELIVERIES TO PARENT MODEL FARM BY RATIO BETWEEN
C     PARENT FARM DEMAND AND JOINT PARENT+CHILD DEMAND
        IF(IGRID.EQ.1) THEN
          do n=2,ngrids
            if(N.EQ.LGRDAT(N)%IFMPGRID) then
             IF(FMPDAT(N)%INRDFL.EQ.-1) THEN
              DO IT=1,MXNRDT   
                IF(TF+FMPDAT(N)%TFDR(NF).GT.0) THEN
                IF(KSTP.EQ.1) RNRD(1,MXNRDT+IT,NF)=RNRD(1,IT,NF)
                  RNRD(1,IT,NF)=
     1            RNRD(1,MXNRDT+IT,NF)*TF/(TF+FMPDAT(N)%TFDR(NF))
                ENDIF
              ENDDO
             ENDIF
            ENDIF
          ENDDO
        ELSE
C     SCALE RANKED NON-ROUTED DELIVERIES TO CHILD MODEL FARM BY RATIO BETWEEN
C     CHILD FARM DEMAND AND JOINT PARENT+CHILD DEMAND AND
C     ADOPT NRD-USE FLAGS FROM PARENT MODEL NRD DATA INPUT.
         IF(INRDFL.EQ.-1) THEN
           DO IT=1,MXNRDT   
             IF(FMPDAT(1)%TFDR(NF)+TF.GT.0) THEN
               IF(KSTP.EQ.1) THEN
                 RNRD(1,MXNRDT+IT,NF)=FMPDAT(1)%RNRD(1,MXNRDT+IT,NF)
               ENDIF
               RNRD(1,IT,NF)=
     1         RNRD(1,MXNRDT+IT,NF)*TF/(TF+FMPDAT(1)%TFDR(NF))
               RNRD(2,IT,NF)=FMPDAT(1)%RNRD(2,IT,NF)
               UNRD(3*IT,NF)=FMPDAT(1)%UNRD(3*IT,NF)
             ENDIF
           ENDDO
         ENDIF        
        ENDIF
      ENDIF      
C
         DO IT=1,MXNRDT
            INRDR(IT)=IDINT(UNRD(3*IT,NF))                              !define nrd-ranks as array to find maximum rank for current farm
            INRDU(IT)=IDINT(RNRD(2,IT,NF))                              !define nrd-use flags for a ranked array of nrd-types for current farm
         ENDDO
         ITTOT=MAXVAL(INRDR)                                            !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
c
         DSUM=0.0d0
         DO IRT=1,ITTOT
            DSUM=DSUM+RNRD(1,IRT,NF)
            ITU=ITTOT-IRT                                               !check for delivery types unnecessary to meet TFDR (ITU) in sequence of ranking 
            IF(TF-DSUM.LE.0.0D0) GOTO 70
         ENDDO
70       IF(TF.LT.FPS) ITU=ITTOT
c
         ND=0.0D0
         SUFDSUM=0.0D0
         SURPLUS1=0.0D0
         SURPLUS2=0.0D0
         DO IRT=1,ITTOT
            IF(TF.GT.DSUM) THEN
            ND=DSUM                                                     !non-routed delivery can't be greater than the sum of deliveries
            ELSE
               IF(IRT.LT.ITTOT-ITU) THEN
               SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                           !sum of deliveries that are used in their entirety to satisfy TF
               ENDIF
               IF(IRT.EQ.ITTOT-ITU) THEN
                  IF(INRDU(IRT).EQ.0) THEN
                  ND=TF                                                 !as long as the current Ranked Type IRT exceeds TF, only part of IRT will be used & the total ND is equal to TF
                  ELSEIF(INRDU(IRT).GT.0) THEN
                  ND=SUFDSUM+RNRD(1,IRT,NF)                             !if not only the sufficient but the abolute amount of IRT is to be used ...
                    IF(INRDU(IRT).EQ.1) SURPLUS1=ND-TF                  !gather excess surplus component desired to be recharged into the canal
                    IF(INRDU(IRT).EQ.2) SURPLUS2=ND-TF                  !gather excess surplus component desired to be injected into farm wells
                  ENDIF
               ENDIF
               IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
               ND=ND+RNRD(1,IRT,NF)                                     !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
                 IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be recharged into the canal
                 IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be injected into farm wells
               ENDIF
            ENDIF
         ENDDO
C
C7E1B--UPDATE NON-ROUTED DELIVERY FOR ACREAGE-OPTIMIZATION 
c      (only for farms that had a reduction in nw during previous iteration)
c       * to include ".AND.ISTARTFL.GE.2" into the if statement will only allow 
c         an ND-update for cases where a prior appropriation solution was already  
c         achieved right after optimization (for ISTARTFL=1 advanced to =2).
c       * to exclude ".AND.ISTARTFL.GE.2" allows an nd-update also if
c         prior appropriation wasn't solved yet right after optimization during 
c         previous iteration (for ISTARTFL set to =0 and advanced to =1).
c      (--> the combination of acreage-optimization and prior appropriation 
c           is still in testing phase!)
       IF(IDEFFL.GT.0.AND.IOPTFL.EQ.1) THEN    !.AND.ISTARTFL.GE.2       !for Acreage-Optimization carried out in previous iteration
c     Don't adjust ND if ND = "new demand from non-routed deliveries,"
c     or adjust ND to "new demand from non-routed deliveries" if all NRD-use flags are zero (surplus not used)
         IF(NW.EQ.ND) GOTO 791                                          !if optimized NW was not reduced from original ND --> skip!
           DO IT=1,MXNRDT
              INRDR(IT)=IDINT(UNRD(3*IT,NF))                            !define nrd-ranks as array to find maximum rank for current farm
              INRDU(IT)=IDINT(RNRD(2,IT,NF))                            !define nrd-use flags for a ranked array of nrd-types for current farm
           ENDDO
           ITTOT=MAXVAL(INRDR)                                          !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
           IF(MAXVAL(INRDU).EQ.0.AND.NW.LE.ND) THEN                     !use NW as long as it is smaller than the available ND
             ND=NW
             GOTO 791
           ENDIF
c
c     Determine new Surplus, in case ND > "new demand from non-routed deliveries"
c     Definition of nrd-ranks & nrd-use flags, and of maximum rank for current farm (already done above - outside def. scenarios)
           DSUM=0.0d0
           DO IRT=1,ITTOT
              DSUM=DSUM+RNRD(1,IRT,NF)
              ITU=ITTOT-IRT                                             !check for delivery types unnecessary to meet TFDR (ITU) in sequence of ranking 
              IF(NW-DSUM.LE.0.0D0) GOTO 73
           ENDDO
73         IF(NW.LT.FPS) ITU=ITTOT
           ND=0.0D0
           SUFDSUM=0.0D0
           SURPLUS1=0.0D0
           SURPLUS2=0.0D0
           DO IRT=1,ITTOT
             IF(NW.GT.DSUM) THEN
               ND=DSUM                                                  !non-routed delivery can't be greater than the sum of deliveries
             ELSE
               IF(IRT.LT.ITTOT-ITU) THEN
               SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                           !sum of deliveries that are used in their entirety to satisfy TF
               ENDIF
               IF(IRT.EQ.ITTOT-ITU) THEN
                 IF(INRDU(IRT).EQ.0) THEN
                 ND=NW                                                  !as long as the current Ranked Type IRT exceeds TF, only part of IRT will be used & the total ND is equal to TF
                 ELSEIF(INRDU(IRT).GT.0) THEN
                   ND=SUFDSUM+RNRD(1,IRT,NF)                            !if not only the sufficient but the absolute amount of IRT is to be used ...
                   IF(INRDU(IRT).EQ.1) SURPLUS1=ND-NW                   !gather excess surplus component desired to be recharged into the canal
                   IF(INRDU(IRT).EQ.2) SURPLUS2=ND-NW                   !gather excess surplus component desired to be injected into farm wells
                 ENDIF
               ENDIF
               IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
                 ND=ND+RNRD(1,IRT,NF)                                   !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
                 IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be recharged into the canal
                 IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be injected into farm wells
               ENDIF
             ENDIF
           ENDDO
c
791      CONTINUE
       ENDIF
C
      ENDIF   ! INRDFL.NE.0
C
C7E2----UPDATE SW DELIVERY FROM HEAD-GATE REACH (ROUTED OR SEMI-ROUTED), AND FARM WELL DISCHARGE
      IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.NE.0)) THEN
         FSLEN=FCSEGL(NF)
         AFDELSW=0.D0
         RDEL=0.D0
         IFCRIDOLD=0
         IF(ISRDFL.EQ.-1) GOTO 665
         DO L=3,NSTRM
            LL=L-1                              
            FLOWIN=DBLE(ANINT(STRM(9,LL)*AR))/AC                        !STRM(10,L) ?   seb response STRM(9,L-1) = STRM(10,L)  **note that LL=L-1
            RCHLEN=DBLE(STRM(1,L))
C
C7E2A---DETERMINE SW DELIVERY FROM HEAD-GATE REACH

C7E2A1--DETERMINE LOCATION OF HEAD-GATE REACH
C       Farm Reach: * belongs to farm, * is at all a farm reach,
C                   * 1st one being different from previous one,
C                   * 1st one being different from second previous one, meaning
C                     1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point),
C                     but still within the same farm.
           IF(IFCRID(NF,L).EQ.NF .AND. IFCRID(NF,L).GT.0 .AND.
     1      IFCRID(NF,L).NE.IFCRID(NF,L-1).AND.IFCRID(NF,L).NE.
     2      IFCRID(NF,L-2).AND.IFCRID(NF,L).NE.IFCRIDOLD) THEN
C
C7E2A2--DETERMINE REACH DELIVERY REQUIREMENT AND ACTUAL DELIVERY FROM HEAD-GATE REACH INTO FARM
              IFCRIDOLD=IFCRID(NF,L)
              IF(IOPTFL.EQ.0.AND.ND.LE.TF) RDEL=TF-ND-QR                !QR is not zero for acreage optimization, otherwise zero here
              IF(IOPTFL.EQ.1.AND.ND.LE.TF) RDEL=SW-SURPLUS1
              IF(ND.GT.TF) RDEL=-SURPLUS1                               !recharge cumulative surplus of nrd (if desired to be sent to SW) into canal
C
C             GENERAL CASE & EQUAL APPROPRIATION
              IF((IALLOTSW.EQ.1.OR.IALLOTSW.EQ.-1).AND.
     1          FARMALLOT.LT.FLOWIN) FLOWIN=FARMALLOT
              IF(IALLOTSW.LT.2.AND.RDEL.GT.FLOWIN) RDEL=FLOWIN
              IF(IGRID.EQ.1.AND.IALLOTSW.EQ.1) FALLOT(NF)=FLOWIN        !OVERWRITE PARENT ALLOTMENT WITH JOINT (PARENT + CHILD) ALLOTMENT OR UNRESTRICTED STREAMFLOW
C
C             PRIOR APPROPRIATION
              IF(IALLOTSW.GE.2) THEN
C
C               check if delivery is constrained by water rights calls
                IF(IALLOTSW.EQ.2) RDEL=MIN(RDEL,WRC(2,NF))
C
C               tell to carry out PRIOR by setting IRDRFL=0 for the following case:
C               a solution was found in previous iteration (ADIV=PDIV; RDEL=FLOWIN),
C               but in current iteration ADIV happens to be < PDIV and therefore also FLOWIN<RDEL)
                if((DABS(rdel-flowin).gt.PCLOSE.and.
     1            abs(SEG(2,ISTRM(4,L))-DVRSFLW(ISTRM(4,L))).GT.
     2            SNGL(PCLOSE)))
     3            irdrfl(ifcrid(nf,l))=0
C
C               don't iterate PRIOR anymore after a solution for a farm head gate reach was found (ifcrid(nf,l)=1),
C               and after a solution with all farms together was found (istarftfl=2)
                if(irdrfl(ifcrid(nf,L)).eq.1.or.istartfl.ge.2) goto 500
C
C               carry out prior appropriation subroutine
                IF(IRDRFL(ifcrid(nf,L)).EQ.0) THEN
                  CALL PRIOR(RDEL,FLOWIN,L,NF,KPER,KSTP)
                  IF(ITER.EQ.1) THEN
                     IF(ideffl.eq.-2.or.ideffl.gt.0) THEN
                        IF(ITER2.EQ.1) ITER2=2
                        IF(IOPTFL.EQ.1) GOTO 41
                        IF(IOPTFL.EQ.0) GOTO 300
                     ELSE
c                       note: after incrementing PDIV and no PRIOR solution,
c                       SEG and STRM are adjusted before PRIOR exits: previously "goto 300" 
c                       but adjusting CIR,TDR,FNRCH, and SWRUN after "41" makes sense before exiting.
                        GOTO 41
                     ENDIF
                  ENDIF
                ENDIF
C
c               correction 1: correction of minor inaccuracy after having found a convergence solution
c               correction 1.a (if tf-nd > 0):
                IF(ND.LE.TF.AND.TF-ND-RDEL.LT.PCLOSE) THEN
                  SEG(2,ISTRM(4,L))=SEG(2,ISTRM(4,L))+SNGL((TF-ND-RDEL))
                  RDEL=TF-ND
                ENDIF
c               correction 1.b (if tf-nd < 0 & surplus < flowinmin):
                IF(ND.GT.TF.AND.DABS(-SURPLUS1-RDEL).LT.PCLOSE) THEN
                  SEG(2,ISTRM(4,L))=SEG(2,ISTRM(4,L))-
     1            SNGL(SURPLUS1+RDEL)
                  RDEL=-SURPLUS1
                ENDIF
C
c               assumption (if suplus > flowinmin):
c                  - surplus at upstream junior farm compensates for flowinmin, and
c                  - additional excess surplus is recharged into canal at upstream junior farm.
                IF(ND.GT.TF.AND.SURPLUS1.GT.FLOWINMIN(L)) THEN
                  SEG(2,ISTRM(4,L))=0.0
                  RDEL=-SURPLUS1
                ENDIF
C               
              ENDIF
              
C           save actual delivery from surface water to farm and pull it from canal 
C           (by adding a 'negative recharge' " - RDEL " to 'reach-by-reach overland recharge' of SFR package)
C
C-----------FOR A PARENT MODEL FARM WITH A HEAD-GATE REACH, FOR A CHILD MODEL WHERE FMP IS ACTIVE, AND WHERE THE CURRENT FARM STRADDLES BOTH MODELS,
C           CHECK IF THE SUM OF RESIDUAL DEMANDS OF PARENT AND CHILD MODEL FARMS EXCEED THE AVAILABLE STREAMFLOW OR NOT:
C           --> for the case that the sum of both residual demands (parent and child) exceed the available streamflow:
C               scale the RDEL of parent farm back by flowin/(parent_farm_demand + child_farm_demand) AND divert all streamflow.
C           --> for the case that the sum of both residual demands (parent and child) does not exceed the available streamflow:
C               divert child farm demand in addition to parent farm demand from the available streamflow.  
666         IF(ILGR.NE.0.AND.LGRITER.GT.1.AND.IGRID.EQ.1.AND.
     1         NGRIDS.GT.1) THEN 
               CRDEL=0
               DO N=2,NGRIDS
               IF(N.EQ.LGRDAT(N)%IFMPGRID) THEN
                IF(FMPDAT(N)%ISRDFL.EQ.-1)THEN
                  CRDEL=CRDEL+FMPDAT(N)%TFDR(NF)-FMPDAT(N)%NRD(1,NF)
                  IF(TF-ND-QR+CRDEL.GT.FLOWIN) THEN
                    STRM(12,L)=SNGL(SFRADD(L)-FLOWIN)
                    RDEL=(TF-ND-QR)*FLOWIN/(TF-ND-QR+CRDEL)
                  ELSE
                    STRM(12,L)=SNGL(SFRADD(L)-RDEL-CRDEL)
                  ENDIF
                ENDIF
               ENDIF
               ENDDO
            ELSE
               STRM(12,L)=SNGL(SFRADD(L)-RDEL)
            ENDIF
C     
            AFDELSW=RDEL
            RDR(L)=RDEL
            ENDIF
         ENDDO
C
C        FOR CHILD MODEL FARM WITHOUT A HEAD-GATE REACH, THE CHILD FARM DOES NOT HAVE ANY ACTUAL FARM DELIVERY 
C        FROM SURFACE WATER FROM AN OWN SOURCE, BUT MAY RECEIVE DELIVERIES FROM THE PARENT FARM HEAD GATE
C        --> for the case that the sum of both residual demands (parent and child) exceed the available streamflow:
C            surface-water delivery to child farm is calculated as available streamflow minus the scaled back delivery requirement of parent farm.
C        --> for the case that the sum of both residual demands (parent and child) does not exceed the available streamflow:
C            surface-water delivery to child farm equals child farm delivery requirement minus child farm non-routed deliveries.
 665     IF(ILGR.NE.0.AND.LGRITER.GT.1.AND.
     1   IGRID.GE.2.AND.ISRDFL.EQ.-1) THEN
         CRDEL=0
         DO N=2,NGRIDS
         IF(N.EQ.LGRDAT(N)%IFMPGRID) THEN
           IF(FMPDAT(N)%ISRDFL.EQ.-1)THEN
             CRDEL=CRDEL+FMPDAT(N)%TFDR(NF)-FMPDAT(N)%NRD(1,NF)
           ENDIF
         ENDIF
         ENDDO
         
         DO L=1,GWFSFRDAT(1)%NSTRM
           IF(NF.EQ.FMPDAT(1)%IFCRID(NF,L)) THEN
              FLOWIN=GWFSFRDAT(1)%STRM(9,L-1)           
              IF(IALLOTSW.EQ.0.AND.
     1           FLOWIN.LT.FMPDAT(1)%RDR(L)+CRDEL)THEN
                 AFDELSW=(TF-ND)*FLOWIN/
     1           (FMPDAT(1)%TFDR(NF)-FMPDAT(1)%NRD(1,NF)+CRDEL)
              ELSEIF((IALLOTSW.EQ.1.OR.IALLOTSW.EQ.-1).AND.
     1           FMPDAT(1)%FALLOT(NF).LT.FMPDAT(1)%RDR(L)+CRDEL) THEN
                 AFDELSW=(TF-ND)*FMPDAT(1)%FALLOT(NF)/
     1           (FMPDAT(1)%TFDR(NF)-FMPDAT(1)%NRD(1,NF)+CRDEL)
              ELSE
                 AFDELSW=TF-ND
              ENDIF
           ENDIF
         ENDDO
C         ENDIF
C         ENDIF
C         ENDDO
         ENDIF
C
C7E2B---DETERMINE PUMPING REQUIREMENT FOR "SW-DOMINATED" CASE
C       PUMPING REQUIREMENT (QR) =
C       TOTAL FARM DELIVERY REQUIREMENT MINUS ACUTAL FARM DELIVERY FROM SURFACE WATER!
         IF(ND.LE.TF) QR=TF-ND-AFDELSW
         IF(ND.GT.TF) QR=-SURPLUS2                                      !inject cumulative surplus of nrd (desired to be sent to GW) into farm wells
         QR=dnint(QR*ac)/ac  
      ELSE
C
C7E3----DETERMINE PUMPING REQUIMENT FOR "GW-ONLY" CASE IN THE ABSENCE OF SURFACE-WATER ROUTING SYSTEM
      IF(ND.LE.TF) QR=TF-ND
      IF(ND.GT.TF) QR=-SURPLUS2                                         !inject cumulative surplus of nrd (desired to be sent to GW) into farm wells
      ENDIF
C
C7E4----PRINT SUPPLY AND DEMAND BUDGET INFORMATION PER ITERATION
C
C7E4A---PRINT HEADER INFORMING ABOUT A BUDGET CORRECTION
C       (ONLY FOR PRIOR APPROPRIATION IN CONJUNCTION WITH DEFICIENCY SCENARIOS)
c       If a solution for a PDIV reduction (as a result) of reduced demand was not yet 
c       found within the water-stacking routine during the previous iteration, then the 
c       reduced demand is now, during the current iteration, taken to be a new non-reduced 
c       demand and the deficit scenario below will not be activated anymore (solved as usual):
c       This is the reason, why we need at this point the headers for a corrected budget, which
c       usually are only printed from within the deficiency scenario routine below (cmpr. format 11 and 12).
      IF(iallotsw.GE.2.and.(ideffl.eq.-2.or.ideffl.gt.0).and.
     1   iter2.eq.1.and.iter.eq.0) then
        supplyflrold=tfdrold(nf)-qreqold(nf)+qmaxf(nf)    
        supplyflrnew=tf-qr+qmaxf(nf)      
C
C7E4A1--PRINT HEADER INFORMING ABOUT A BUDGET CORRECTION
C       (ONLY FOR PRIOR APPROPRIATION IN CONJUNCTION WITH WATER STACKING) 
      if(ideffl.eq.-2) then
c
c       comments informing about budget corrections as a result of
c       prior appropriation in conjuction with water-stacking
        if(supplyflrold.gt.tf) then                                     !tf = tdrnonfal (demand of priority crops)
        IF(LSTCHK(3)) THEN
          write(iout,11)'CORRECTION FOR TFDR, NR-SWD, R-SWD, QREQ IF  
     1TOTAL AVAILABLE SUPPLY > DEMAND OF NON-FALLOWABLE CROPS:'
        ENDIF
       elseif(supplyflrold.lt.tf.and.tf.lt.tfdrold(nf).and.
     1         qr.gt.qmaxf(nf)) then
       IF(LSTCHK(3)) THEN
         write(iout,12)'CORRECTION FOR TFDR AND QREQ, IF SUPPLY <  
     1DEMAND FOR NON-FALLOWABLE CROPS:'
       ENDIF
c
c       in contrast to the above comments, which represent budget corrections as a result of water-stacking, 
c       this comment represents a case, where a deficiency from the previous iteration is rendered 
c       to be no more a deficiency during the current iteration, since a downstream farm now benefits 
c       from a partially fallowed upstream farm.
c       Therefore, this comment does not show up as well in the deficiency scenario routine below.
c       (if after delivering more SW to the downstream farm still a deficiency persists, then
c        deficit irrigation will be applied to the priority crops).
        elseif(supplyflrnew.gt.supplyflrold) then
       IF(LSTCHK(3)) THEN
         write(iout,121)'CORRECTION FOR R-SWD AND QREQ, IF INCREASED
     1SUPPLY DUE TO FALLOWED UPSTREAM FARM > DEMAND OF ORIGINAL CROPS:'
       ENDIF
  121   format(1x,/,A108)                                             
        endif                                                         
      endif                                                           
C
C7E4A2--PRINT HEADER INFORMING ABOUT A BUDGET CORRECTION
C       (ONLY FOR PRIOR APPROPRIATION IN CONJUNCTION WITH ACREAGE OPTIMIZATION)
      if(ideffl.gt.0) then
      supplyflrnewold=tfdr(nf)-qreq(nf)+qmaxf(nf)
c
c       comments informing about budget corrections as a result of
c       prior appropriation in conjuction with acreage optimization
        if(supplyflrold-tf.gt.FPS) then                                 !tf = nw+sw+qr (reduced demand); if new tf < avail. supply ==> non-profitable old supply 
        IF(LSTCHK(3)) THEN
          write(iout,13)'CORRECTION FOR TFDR, NR-SWD, R-SWD, & QREQ IF  
     1ORIG. AVAILABLE SUPPLY > DEMAND OF PROFITABLE ACREAGE:'
        ENDIF
        elseif(DABS(supplyflrold-tf).lt.FPS) then
        IF(LSTCHK(3)) THEN
          write(iout,14)'CORRECTION FOR TFDR,NR-SWD,R-SWD,QREQ IF DEMAND
     1OF REDUCED ACREAGE WAS FORCED TO MATCH SUPPLY:'
        ENDIF
c
c       in contrast to the above comments, which represent budget corrections as a result of acreage-optimization, 
c       this comment represents a case, where a deficiency from the previous iteration may be rendered 
c       to be no more a deficiency during the current iteration, since a downstream farm now benefits
c       from a partially fallowed upstream farm. 
c       ==> farm satisfied:
        elseif(supplyflrnew.gt.supplyflrold.and.qr.lt.qmaxf(nf).and.
     1  DABS(supplyflrnewold-supplyflrold).lt.FPS) then
        IF(LSTCHK(3)) THEN
          write(iout,141)
     1  'CORRECTION FOR R-SWD AND QREQ, IF INCREASED DOWNSTREAM SUPPLY'
     2 ,'(DUE TO NON-PROFITABLILTY OF SW-SUPPLY OF UPSTREAM FARM) > ',
     3  'DEMAND OF ORIGINAL CROPS:'                                   
        ENDIF
  141   format(1x,/,A61,/,A59,A25)
c       ==> a secondary deficiency could still be the case and optimization be carried out,
c           even though more SW was delivered:
        elseif(supplyflrnew.gt.supplyflrold.and.swredprev.gt.FPS.and.
     1  qr.gt.qmaxf(nf)) then
        IF(LSTCHK(3)) THEN
          write(iout,142)
     1  'CORRECTION FOR R-SWD AND QREQ, IF DOWNSTREAM SUPPLY INCREASED'
     2 ,'DUE TO NON-PROFITABLILTY OF SW-SUPPLY OF UPSTREAM FARM, BUT ',
     3  'QREQ IS STILL > QMAXF:'                                      
        ENDIF
  142   format(1x,/,A61,/,A60,A22)
c       ==> after secondary deficiency the new increased supply might still be un-profitable
c           notice: the demand pulls a final supply that is less than the new increased supply, but       
c           this final supply could still be either smaller or greater than the original available supply.
        elseif(supplyflrnewold-tf.gt.FPS) then
        IF(LSTCHK(3)) THEN
          write(iout,143)
     1  'CORRECTION FOR TFDR, NR-SWD, R-SWD & QREQ, IF INCREASED AVAILA'
     2 ,'BLE SUPPLY > DEMAND OF PROFITABLE ACREAGE:'                  
        ENDIF
  143   format(1x,/,A62,A42)
        endif
      endif
      endif
C
C7E4B---PRINT SUPPLY AND DEMAND BUDGET PER ITERATION
C
  403 IF(ISDPFL.EQ.-3) THEN
      IF(LSTCHK(3)) THEN                                                ! seb MERGED TWO LSTCHK(3)   
        write(IOUT,10)
   10 format(1X,/,1X,'FARM-ID',
     18X,'OFE',20X,'TFDR',18X,'NR-SWD',19X,'R-SWD',20X,'QREQ')
        WRITE(IOUT,20) NF, EFF(NCROPS+2,NF), TF, ND, TF-ND-QR, QR
      ENDIF
   20 FORMAT(I8,1F11.4,4F24.4)
      ENDIF
C
C7E5----SAVE BUDGET TERMS
C
C7E5A---SAVE BUDGET TERMS IN GENERAL IN ODER TO BE AVAILABLE IN FMP3WELBD
C       In the regular case, budget terms are saved at each iteration. In case of deficiency,
C       "pre-deficiency" terms are saved during the deficiency scenario execution (C7E6B,C7E7G)  
C       under a different name in order to be still available in FMP3WELBD along with the final terms.
      TFDR(NF)=TF
      NRD(1,NF)=ND
      QREQ(NF)=QR
C
C7E5B---SAVE BUDGET TERMS AS "PRE-DEFICIENCY" SCENARIO TERMS:
C       For prior appropriation, the "to be optimized" "pre-deficiency" terms need to be corrected 
C       for small numerical inaccuracies after the PIOR routine was carried out for all farms and
C       after the farms loop is completed. That is, these terms need to be gathered at the end of 
C       the farms loop while deficieny scenarios are not yet executed.
      if(IALLOTSW.GE.2.and.iter2.eq.0) then
      TFDROLD(NF)=TF
      NRD(2,NF)=ND
      QREQOLD(NF)=QR
      go to 80
      ENDIF
C
C7E5C---SAVE UNSATISFIED TOLERANCE OF PUMPING REQUIREMENT
      QTOL=QREQ(NF)-QMAXF(NF)
C
501   IF(IDEFFL.EQ.0) GO TO 80
      IF(IDEFFL.GT.0) GO TO 30
      IF(IDEFFL.LT.0) THEN
        IF(ISTARTFL.EQ.1) KNTR(NF)=1
        IF(QTOL.GT.FPS) THEN
        AFDELSW=TF-QR-ND
        AREASUM=0.D0
C
C7E6----DEFICIT IRRIGATION OPTION: ADJUST IRRIGATION FLUX PER CELL
C
C7E6A---Calculate Average Supply Flux for each cell
          IF(IDEFFL.EQ.-1) THEN                               
           DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
            IR=FMLOC(NF)%RC(1,I)
            IC=FMLOC(NF)%RC(2,I)
            IF(ICID(IC,IR).GT.0) AREASUM=AREASUM+DBLE(DELR(IC)*DELC(IR))
           ENDDO
            AVGSUPPLYFLUX=(QMAXF(NF)+AFDELSW+ND)/AREASUM
C
C7E6B---Prorate Excess from crops, where TDR < Average Supply, to other crops
            QSUMEX=0.D0
            QSUMDF=0.D0
            DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
              IR=FMLOC(NF)%RC(1,I)
              IC=FMLOC(NF)%RC(2,I)
               IF(ICID(IC,IR).GT.0) THEN
                 AVGSUPPLYFLR=AVGSUPPLYFLUX*DBLE(DELR(IC)*DELC(IR))
                 IF(AVGSUPPLYFLR.LT.TDR(IC,IR)) THEN
                   QDF=TDR(IC,IR)-AVGSUPPLYFLR
C                  sum of all deficiencies between average available supply per cell and demand per cell
                   QSUMDF=QSUMDF+QDF
                 ELSE
                   QEX=AVGSUPPLYFLR-TDR(IC,IR)
C                  sum of all excesses between demand per cell and average available supply per cell
                   QSUMEX=QSUMEX+QEX
                 ENDIF
               ENDIF
            ENDDO
C         
            TF=0.D0
            CCIR=0.D0
            DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
              IR=FMLOC(NF)%RC(1,I)
              IC=FMLOC(NF)%RC(2,I)
               IF(ICID(IC,IR).GT.0) THEN
C                SAVE EFFICIENCY BEFORE OVERWRITING IT
                 CIR=EF2D(IC,IR)*TDR(IC,IR)
C
                 AVGSUPPLYFLR=AVGSUPPLYFLUX*DBLE(DELR(IC)*DELC(IR))
                 IF(AVGSUPPLYFLR.LT.TDR(IC,IR)) THEN                    !IF SUPPLY HOWEVER IS MORE, THEN TDR STAYS ORIGINAL!
C                OVERWRITE OLD DEMAND PER CELL WITH
C                ACTUAL AV.SUPPLY + PRORATED SHARE FROM CELLS WITH EXCESS SUPPLY
                 TDR(IC,IR)=
     1           AVGSUPPLYFLR+(QSUMEX/QSUMDF)*(TDR(IC,IR)-AVGSUPPLYFLR)
                 ENDIF
C
c                IMPROVE EFFICIENCY OF EACH CELL IN DROUGHT SITUATIONS
                 IF(TDR(IC,IR).GT.CIR) EF2D(IC,IR)=CIR/TDR(IC,IR)
                 IF(TDR(IC,IR).LE.CIR) THEN
                   EF2D(IC,IR)=1.D0
                   CIR=TDR(IC,IR)
                 ENDIF
                 CCIR=CCIR+CIR
                 TF=TF+TDR(IC,IR)
C                        
               ENDIF
            ENDDO
C
C           EFF(NCROPS+3,NF)=EFF(NCROPS+2,NF)
            IF(TF.LT.FPS) THEN
              EFF(NCROPS+2,NF)=-999
            ELSEIF(TF.GT.0.d0) THEN                                     !ISTARTFL.GT.1.AND. ?
C           IMPROVE 'COMPOSITE' EFFICIENCY OF FARM AS A RESPONSE TO REDUCED DELIVERY
              EFF(NCROPS+2,NF)=CCIR/TF
C           REDEFINE QR WITH NEW TF
            ENDIF                                                       !**
              IF(IRDFL.NE.0.OR.ISRDFL.GT.0) THEN
                QR=TF-ND-AFDELSW
              ELSE
                QR=TF-ND
              ENDIF
C           store original pumping requirement before updates after acreage-optimization are done
            IF(IWELLFLD.EQ.1.AND.(KITER.LE.1.OR.ISTARTFL.LE.1)) GOTO 502
              IF(QREQOLD(NF).EQ.0.0D0) QREQOLD(NF)=QREQ(NF)
              IF(NRD(2,NF).EQ.0.0D0) NRD(2,NF)=NRD(1,NF)
              IF(TFDROLD(NF).EQ.0.0D0) TFDROLD(NF)=TFDR(NF)
502           TFDR(NF)=TF
              QREQ(NF)=QR     
c           ENDIF                                                       !moved to ** to also allow updates of a zero TF (reduction of non-zero TFDR(NF) to a zero TF)
c
          ENDIF
C
C7E7----DEFICIT IRRIGATION WITH WATER STACKING FOR NON-FALLOWABLE CROPS
C
C7E7A---Calculate Average Supply Flux, if available supply is "water-stacked" to non-fallow crops
          IF(IDEFFL.EQ.-2) THEN
C          
          DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
            IR=FMLOC(NF)%RC(1,I)
            IC=FMLOC(NF)%RC(2,I)
              DO NC=1,NCROPS
              IF(ICA(NC).NE.0) THEN   
                IF(ICID(IC,IR).EQ.IFALLOW(1,NC).
     1            AND.IFALLOW(2,NC).EQ.0)THEN                        !for all non-fallowable crops
                  AREASUM=AREASUM+DBLE(DELR(IC)*DELC(IR))
                ENDIF
              ENDIF
              ENDDO
            ENDDO
            AVGSUPPLYFLUX=(QMAXF(NF)+AFDELSW+ND)/AREASUM
C
C7E7B---Among non-fallow crops: Prorate Excess from crops, where TDR < Average Supply, to other crops
            QSUMEX=0.D0
            QSUMDF=0.D0
            DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
              IR=FMLOC(NF)%RC(1,I)
              IC=FMLOC(NF)%RC(2,I)
                 DO NC=1,NCROPS
                 IF(ICA(NC).NE.0) THEN 
                   IF(ICID(IC,IR).EQ.IFALLOW(1,NC).
     1               AND.IFALLOW(2,NC).EQ.0)THEN                        !for all non-fallowable crops
                     AVGSUPPLYFLR=AVGSUPPLYFLUX*DBLE(DELR(IC)*DELC(IR))
                     IF(AVGSUPPLYFLR.LT.TDR(IC,IR)) THEN      
                       QDF=TDR(IC,IR)-AVGSUPPLYFLR
C                      sum of all deficiencies between average available supply per cell and demand per cell
                       QSUMDF=QSUMDF+QDF
                     ELSE
                       QEX=AVGSUPPLYFLR-TDR(IC,IR)
C                      sum of all excesses between demand per cell and average available supply per cell
                       QSUMEX=QSUMEX+QEX
                     ENDIF
                   ENDIF
                 ENDIF
                 ENDDO
            ENDDO
C         
            TDRNONFAL=0.D0
            DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
              IR=FMLOC(NF)%RC(1,I)
              IC=FMLOC(NF)%RC(2,I)
                  DO NC=1,NCROPS
                  IF(ICA(NC).NE.0) THEN   
                    IF(ICID(IC,IR).EQ.IFALLOW(1,NC).
     1                AND.IFALLOW(2,NC).EQ.0)THEN                       !for all non-fallowable crops
c                     gather original TDR for non-fallowable crops: 
c                     (in case there are no under-supplied non-fallowable crops, 
C                      new TFDR (sum of over-supplied non-fallow crops) is needed,
C                      which will be supplied from canals and wells)
                      TDRNONFAL=TDRNONFAL+TDR(IC,IR)
                      AVGSUPPLYFLR=AVGSUPPLYFLUX*DBLE(DELR(IC)*DELC(IR))
                      IF(AVGSUPPLYFLR.LT.TDR(IC,IR)) THEN               !IF SUPPLY HOWEVER IS MORE, THEN TDR STAYS ORIGINAL!
C                     OVERWRITE OLD DEMAND PER CELL WITH
C                     ACTUAL AV.SUPPLY + PRORATED SHARE FROM CELLS WITH EXCESS SUPPLY
                      TDR(IC,IR)=AVGSUPPLYFLR+
     1                (QSUMEX/QSUMDF)*(TDR(IC,IR)-AVGSUPPLYFLR)
                      ENDIF
C                   set supply zero for all fallowable crops
                    ELSEIF(ICID(IC,IR).EQ.IFALLOW(1,NC).
     1                AND.IFALLOW(2,NC).NE.0)THEN
C                     set components of TDR for future iterations zero for all fallowable crops
                      TDR(IC,IR)=0.D0
                      CU2D(IC,IR)=0.D0                             
                      TPPOT(IC,IR)=0.D0
                      TGW(IC,IR)=0.D0
C                     set tgwa zero to make cir = 0, before cir & tdrold get compared to avi
C                     but here: no crop condition anymore since those crops go fallow, so...
C                     we neither have supply (available irrigation avi), nor do we have CU (no cir,no TPPOT,no tgwa)
                      TGWA(IC,IR)=0.D0
                    ENDIF
                  ENDIF
                  ENDDO
            ENDDO                                   
C           
C7E7C---UPDATE TFDR FOR NON-FALLOWABLE CROPS ONLY AND UPDATE AFDELSW & QREQ, 
C       IF NEW TFDR IS LESS THEN PREVIOUSLY INSUFFICIENT (FOR ALL CROPS) SUPPLY
c       (less supply necessary for only non-fallow crops)
c
            SUPPLYFLR=AFDELSW+ND+QMAXF(NF)
            TFOLD=TFDR(NF)
            TF=TDRNONFAL                                                !update total farm delivery requirement
            IF(SUPPLYFLR.GT.TDRNONFAL) THEN
C
              IF(KNTR(NF).EQ.1) THEN
c
C7E7C1--UPDATE NON-ROUTED DELIVERY IF TF CHANGES AS A RESULT OF WATER-STACKING
              IF(INRDFL.NE.0) THEN
                 if(iallotsw.LT.2) NRD(2,NF)=ND
c
c             Don't adjust ND if ND <= new TF or adjust ND to new reduced TF if all NRD-use flags are zero (surplus not used)
c             (not really needed but saves unneeded computations)!
                 IF(ND.LE.TF) GOTO 789            
                 DO IT=1,MXNRDT
                    INRDR(IT)=IDINT(UNRD(3*IT,NF))                      !define nrd-ranks as array to find maximum rank for current farm
                    INRDU(IT)=IDINT(RNRD(2,IT,NF))                      !define nrd-use flags for a ranked array of nrd-types for current farm
                 ENDDO       
                 ITTOT=MAXVAL(INRDR)                                    !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
                 IF(MAXVAL(INRDU).EQ.0.AND.ND.GT.TF) THEN               !store original non-routed delivery before water-stacking is done
                 ND=TF                                                  !check by priority if new TF is now less than Non-Routed Delivery
                 GOTO 789
                 ENDIF
c
c             Determine new Surplus, in case ND > new TF
c             Definition of nrd-ranks & nrd-use flags, and of maximum rank for current farm (already done above - outside def. scenarios)
                 DSUM=0.0d0
                 DO IRT=1,ITTOT
                    DSUM=DSUM+RNRD(1,IRT,NF)
                    ITU=ITTOT-IRT                                       !check for delivery types unnecessary to meet TFDR (ITU) in sequence of ranking
                    IF(TF-DSUM.LE.0.0D0) GOTO 71
                 ENDDO
71               IF(TF.LT.FPS) ITU=ITTOT
                 ND=0.0D0
                 SUFDSUM=0.0D0
                 SURPLUS1=0.0D0
                 SURPLUS2=0.0D0
                 DO IRT=1,ITTOT
                  IF(TF.GT.DSUM) THEN
                  ND=DSUM                                               !non-routed delivery can't be greater than the sum of deliveries
                  ELSE
                   IF(IRT.LT.ITTOT-ITU) THEN
                   SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                       !sum of deliveries that are used in their entirety to satisfy TF
                   ENDIF
                   IF(IRT.EQ.ITTOT-ITU) THEN
                      IF(INRDU(IRT).EQ.0) THEN
                      ND=TF                                             !as long as the current Ranked Type IRT exceeds TF, only part of IRT will be used & the total ND is equal to TF
                      ELSEIF(INRDU(IRT).GT.0) THEN
                      ND=SUFDSUM+RNRD(1,IRT,NF)                         !if not only the sufficient but the abolute amount of IRT is to be used ...
                        IF(INRDU(IRT).EQ.1) SURPLUS1=ND-TF              !gather excess surplus component desired to be recharged into the canal
                        IF(INRDU(IRT).EQ.2) SURPLUS2=ND-TF              !gather excess surplus component desired to be injected into farm wells
                      ENDIF
                   ENDIF
                   IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
                   ND=ND+RNRD(1,IRT,NF)                                 !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
                    IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)!gather unneeded surplus components desired to be recharged into the canal
                    IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)!gather unneeded surplus components desired to be injected into farm wells
                   ENDIF
                  ENDIF
                 ENDDO
c
789            CONTINUE
              ENDIF
C             
C7E7C2--SAVE BUDGET TERMS AS "PREDEFICIENCY TERMS" IN GENERAL (IN THE ABSENCE OF PRIOR APPROPRITON)
              if (iallotsw.LT.2) then
C               store original pumping requirement before updates after water-stacking are done
                QREQOLD(NF)=QREQ(NF)
                TFDROLD(NF)=TFOLD
              endif
              ENDIF              
C
C7E7C3--UPDATE SW DELIVERY FROM HEAD-GATE REACH (ROUTED OR SEMI-ROUTED), AND FARM WELL DISCHARGE AS A RESULT OF WATER-STACKING
              KNTR(NF)=KNTR(NF)+1         
              IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0)) THEN
C                FSLEN=FCSEGL(NF) !SEEMS REDUNDANT
                AFDELSW=0.0D0                                           !reset AFDELSW to compute new AFDELSW in case supply > demand for non-fallow crops (for water stacking)
                IFCRIDOLD=0
                DO L=3,NSTRM
                   LL=L-1
                   FLOWIN=DBLE(ANINT(STRM(9,LL)*AR))/AC                 !STRM(10,L) ?  seb response STRM(9,L-1) = STRM(10,L)  **note that LL=L-1
                   RCHLEN=DBLE(STRM(1,L))
C
C           DETERMINE LOCATION OF HEAD-GATE REACH
C           Farm Reach: * belongs to farm, * is at all a farm reach,
C                       * 1st one being different from previous one,
C                       * 1st one being different from second previous one, meaning
C                         1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point),
C                         but still within the same farm.
                   IF(IFCRID(NF,L).EQ.NF .AND. IFCRID(NF,L).GT.0 .AND.
     1             IFCRID(NF,L).NE.IFCRID(NF,L-1).AND.IFCRID(NF,L).NE.
     2             IFCRID(NF,L-2).AND.IFCRID(NF,L).NE.IFCRIDOLD) THEN
C
C           DETERMINE REACH DELIVERY REQUIREMENT AND ACTUAL DELIVERY FROM HEAD-GATE REACH INTO FARM
                   IFCRIDOLD=IFCRID(NF,L)
                   RDELCOR=TF-ND
C
C                  GENERAL CASE & EQUAL APPROPRIATION
                   IF(IALLOTSW.EQ.1 .AND. FARMALLOT.LT.FLOWIN)
     1             FLOWIN=FARMALLOT                                     !Available Flow = Allotment
                   IF(IALLOTSW.LT.2.AND.RDELCOR.GT.FLOWIN)RDELCOR=FLOWIN
C
C                  PRIOR APROPRIATION
                   IF(IALLOTSW.GE.2) THEN
C
C                    check if delivery is constrained by water rights calls
                     IF(IALLOTSW.EQ.2) RDELCOR=MIN(RDELCOR,WRC(2,NF))
C
C                    tell to carry out PRIOR by setting IRDRFL=0 for the following case:
C                    a solution was found in previous iteration (ADIV=PDIV; RDELCOR=FLOWIN),
C                    but in current iteration ADIV happens to be < PDIV and therefore also FLOWIN<RDELCOR)
                     if((DABS(rdelcor-flowin).gt.PCLOSE.and.
     1                 abs(SEG(2,ISTRM(4,L))-DVRSFLW(ISTRM(4,L))).GT.   !NEW HERE (as apposed to first call of PRIOR above):
     2                 SNGL(PCLOSE)).or.QTOL.GT.FPS)
     3                 irdrfl(ifcrid(nf,l))=0                           !Call of PRIOR is allowed if QREQ > QMAXF (QTOL.GT.FPS) !!
c                                                                              
C                    don't iterate PRIOR anymore after a solution for a farm head gate reach was found (ifcrid(nf,l)=1),
C                    and after a solution with all farms together was found (istarftfl=2)
                     if(irdrfl(ifcrid(nf,L)).eq.1.or.istartfl.ge.2)
     1               goto 500
C
C                    carry out prior appropriation subroutine
                     IF(IRDRFL(ifcrid(nf,L)).EQ.0) THEN
                     CALL PRIOR(RDELCOR,FLOWIN,L,NF,KPER,KSTP)
                       IF(ITER.EQ.1) then
                         iter2=2
                         GO TO 300
                       endif
c
c                      correction 1: correction of minor inaccuracy after having found a convergence solution
c                      correction 1.a (if tf-nd > 0):
                       IF(ND.LE.TF.AND.TF-ND-RDELCOR.LT.PCLOSE) THEN
                         SEG(2,ISTRM(4,L))=
     1                   SEG(2,ISTRM(4,L))+SNGL(TF-ND-RDELCOR)
                         RDELCOR=TF-ND
                       ENDIF
c                      correction 1.b (if tf-nd < 0 & surplus < flowinmin):
                       IF(ND.GT.TF.AND.
     1                   DABS(-SURPLUS1-RDELCOR).LT.PCLOSE) THEN
                         SEG(2,ISTRM(4,L))=
     1                   SEG(2,ISTRM(4,L))-SNGL(SURPLUS1+RDELCOR)
                         RDELCOR=-SURPLUS1
                       ENDIF
c
c                      assumption (if suplus > flowinmin):
c                         - surplus at upstream junior farm compensates for flowinmin, and
c                         - additional excess surplus is recharged into canal at upstream junior farm.
                       IF(ND.GT.TF.AND.SURPLUS1.GT.FLOWINMIN(L)) THEN
                         SEG(2,ISTRM(4,L))=0.0
                         RDELCOR=-SURPLUS1
                       ENDIF
                     ENDIF
                   ENDIF
C
C                  save actual delivery from surface water to farm and pull it from canal 
C                  (by adding a 'negative recharge' " - RDELCOR " to 'reach-by-reach overland recharge' of SFR package)
                   AFDELSW=RDELCOR
                   STRM(12,L)=SNGL(SFRADD(L)-RDELCOR)                   !OVERWRITE "STRM(12,L)=SFRADD(L)-RDEL"
                   RDR(L)=RDELCOR
                   ENDIF
                ENDDO
C
C7E7C4----UPDATE PUMPING REQUIREMENT FOR "SW-DOMINATED" CASE
C           PUMPING REQUIREMENT (QR) =
C           TOTAL FARM DELIVERY REQUIREMENT MINUS ACUTAL FARM DELIVERY FROM SURFACE WATER!
C               redefine AFDELSW and QR with new TF
                QR=TF-ND-AFDELSW      
              ELSE
C           DETERMINE PUMPING REQUIMENT FOR "GW-ONLY" CASE IN THE ABSENCE OF SURFACE-WATER ROUTING SYSTEM
                QR=TF-ND
              ENDIF
C
C7E7C5----PRINT SUPPLY AND DEMAND BUDGET INFORMATION PER ITERATION
              IF(ISDPFL.EQ.-3) THEN
              IF(LSTCHK(3)) THEN
              WRITE(IOUT,11)'CORRECTION FOR TFDR, NR-SWD, R-SWD, QREQ IF! seb SHIFTED TWO SPACED TO THE LEFT SO "IF" IS NOT TRUNCATED
     1TOTAL AVAILABLE SUPPLY > DEMAND OF NON-FALLOWABLE CROPS:'
              ENDIF
   11         FORMAT(1X,/,A101)
              ENDIF
C
C7E7D---UPDATE AND SAVE QREQ, IF NEW TFDR IS GREATER THEN PREVIOUSLY INSUFFICIENT (FOR ALL CROPS) SUPPLY
C       (but original supply doesn't change since here even insufficient for just non-fallow crops)
            ELSEIF(SUPPLYFLR.LT.TDRNONFAL .AND. TF.LT.TFOLD) THEN
C
              IF(KNTR(NF).EQ.1) THEN
                 IF(INRDFL.NE.0) THEN
                 NRD(2,NF)=ND                                           !store original non-routed delivery before water-stacking is done
                 ENDIF
              if(iallotsw.LT.2) then 
                 QREQOLD(NF)=QR                                         !store original pumping requirement before water-stacking is done
                 TFDROLD(NF)=TFOLD
              endif
              ENDIF              
              KNTR(NF)=KNTR(NF)+1
              IF(IRDFL.NE.0.OR.ISRDFL.GT.0) THEN                        !redefine QR with new TF 
                QR=TF-ND-AFDELSW
              ELSE
                QR=TF-ND
              ENDIF
              IF(ISDPFL.EQ.-3) THEN
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,12)'CORRECTION FOR TFDR AND QREQ, IF SUPPLY <  
     1DEMAND FOR NON-FALLOWABLE CROPS:'
             ENDIF
   12         FORMAT(1X,/,A75)
              ENDIF
            ENDIF
           IF(SUPPLYFLR.GT.TDRNONFAL .OR.                               !RE-WRITE FOR BOTH CORRECTION CASES
     1     (SUPPLYFLR.LT.TDRNONFAL .AND. TF.LT.TFOLD)) THEN
              IF(ISDPFL.EQ.-3) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,10)
              ENDIF
              IF(LSTCHK(3)) THEN
               WRITE(IOUT,20) NF, EFF(NCROPS+2,NF), TF, ND, TF-ND-QR, QR! seb SHIFTED ONE SPACE TO LEFT SO "QR" IS NOT TRUNCATED TO Q
              ENDIF
              ENDIF
              NRD(1,NF)=ND
              TFDR(NF)=TF
              QREQ(NF)=QR
            ENDIF
C
          ENDIF       
C
        ENDIF
      ENDIF
      GO TO 80   
C-------END OF DEFICIT-IRRIGATION W/WO WATER-STACKING ----------------------------------------------- 
C
C7E8----ACREAGE - OPTIMIZATION:
C
C7E8A---DETEMINE DISTANCE TO NEAREST REACH IN CASE SW-PRICE DEPENDS ON THAT (==> IF POSSIBLE LATER IN SUBROUTINE!!!)
C       (ONLY NECESSARY IF SW-IRRIG. EXISTS & QMAXF < QREQ & NOT YET PREVIOUSLY CALCULATED)
C       [NOTE: BECAUSE OF QMAXF < QREQ IN FM, OTHERWISE COULD BE IN RP, BUT THEN UNNECESSARILY FOR ALL FARMS!]
C       AND DETERMINE ELEVATION OF STREAMHEAD IN NEAREST REACH [NOTE: THIS INDEED NEEDS TO BE HERE IN FM, BECAUSE IT'S HEAD-DEPENDENT]
C
   30 IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0).AND.
     1 (QTOL.GT.FPS.OR.(IDEFFL.GE.3.AND.TF.GT.0D0)) )THEN
      COLD=0.D0
      DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
        IR=FMLOC(NF)%RC(1,I)
        IC=FMLOC(NF)%RC(2,I)                                      !DON'T CALCULATE ANYMORE IF DNR IS ALREADY CALCULATED (DNR > 0)!
        COUNT=0.D0                                                      !.AND. DNR(IC,IR).EQ.0.
        DO L=1,NSTRM
          IF(IFCRID(NF,L).EQ.NF) THEN
          COUNT=COUNT+1
          ASUM=DBLE(DELC(ISTRM(2,L))/2.+DELC(IR)/2.)
          BSUM=DBLE(DELR(ISTRM(3,L))/2.+DELR(IC)/2.)
          NR=IABS(ISTRM(2,L)-IR)                                        !NUMBER OF ROWS BETWEEN STREAM REACH AND CELL
          NC=IABS(ISTRM(3,L)-IC)                                        !NUMBER OF COLUMNS BETWEEN STREAM REACH AND CELL
            IF(NR.EQ.0) ASUM=0.D0
            IF(NR.GT.1) THEN
            DO N=2,NR
              IF(ISTRM(2,L).LT.IR) IIA=ISTRM(2,L)+(N-1)                  !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH! 
              IF(ISTRM(2,L).GT.IR) IIA=IR+(N-1)                          !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
              ASUM=ASUM+DBLE(DELC(IIA))
            ENDDO
            ENDIF
            IF(NC.EQ.0) BSUM=0.D0
            IF(NC.GT.1) THEN
            DO N=2,NC  
              IF(ISTRM(3,L).LT.IC) IB=ISTRM(3,L)+(N-1)                  !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH! 
              IF(ISTRM(3,L).GT.IC) IB=IC+(N-1)                          !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
              BSUM=BSUM+DBLE(DELR(IB))     
            ENDDO
            ENDIF
          C=DSQRT(ASUM**2+BSUM**2)
            IF(COUNT.EQ.1 .OR. (COUNT.GT.1 .AND. C.LT.COLD)) THEN
            COLD=C
            ELNR(IC,IR)=DBLE(STRM(15,L))                                !STORE HEAD ELEVATION OF NEAREST REACH IN CASE SW-LIFT IS RELATED TO COSTS!
            ENDIF
          ENDIF
        ENDDO
        DNR(IC,IR)=COLD
      ENDDO 
      ENDIF
C
C7E8B---DETEMINE EFFECTIVE DISTANCE TO WELLS IN CASE GW-PRICE DEPENDS ON THAT:
C       = AVERAGE DISTANCE BETWEEN CELL AND ALL FARM WELLS WEIGHTED BY THEIR QMAX.
C       (ONLY NECESSARY IF QMAXF < QREQ & NOT YET PREVIOUSLY CALCULATED)
C       [NOTE: BECAUSE OF QMAXF < QREQ IN FM, OTHERWISE COULD BE IN RP, BUT THEN UNNECESSARILY FOR ALL FARMS!]
C
      IF(QTOL.GT.FPS.OR.(IDEFFL.GE.3.AND.TF.GT.0D0)) THEN
       DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
        IR=FMLOC(NF)%RC(1,I)
        IC=FMLOC(NF)%RC(2,I)
        IF(DWE(IC,IR).EQ.0.D0) THEN
        DWSUM=0.D0
        DO L=1,NFWELS
        QMAX=FWELL(6,L)
          IF(IDINT(FWELL(5,L)).EQ.NF) THEN
          ASUM=DBLE(DELC(IDINT(FWELL(2,L)))/2+DELC(IR)/2.)
          BSUM=DBLE(DELR(IDINT(FWELL(3,L)))/2+DELR(IC)/2.)
          NR=IABS(IDINT(FWELL(2,L))-IR)                                 !NUMBER OF ROWS BETWEEN WELL AND CELL
          NC=IABS(IDINT(FWELL(3,L))-IC)                                 !NUMBER OF COLUMNS BETWEEN WELL AND CELL
            IF(NR.EQ.0) ASUM=0.D0
            IF(NR.GT.1) THEN
            DO N=2,NR
              IF(IDINT(FWELL(2,L)).LT.IR) IIA=IDINT(FWELL(2,L))+(N-1)    !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM WELL!
              IF(IDINT(FWELL(2,L)).GT.IR) IIA=IR+(N-1)                   !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
              ASUM=ASUM+DBLE(DELC(IIA))
            ENDDO
            ENDIF
            IF(NC.EQ.0) BSUM=0.D0
            IF(NC.GT.1) THEN
            DO N=2,NC  
              IF(IDINT(FWELL(3,L)).LT.IC) IB=IDINT(FWELL(3,L))+(N-1)    !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM WELL!
              IF(IDINT(FWELL(3,L)).GT.IC) IB=IC+(N-1)                   !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
              BSUM=BSUM+DBLE(DELR(IB))
            ENDDO
            ENDIF
          C=DSQRT(ASUM**2+BSUM**2)
          DWSUM=DWSUM+QMAX*C
          ENDIF
        ENDDO
        DWE(IC,IR)=DWSUM/QMAXF(NF)
        ENDIF 
      ENDDO
      ENDIF
C           
C7E8C---CALCULATE EFFECTIVE HEAD LIFT PER FARM AND RELATE IT TO AN EFFECTIVE ELEVATION (IF ACREAGE NEEDS TO BE REDUCED)
C     --> CALCULATE HEAD LIFT OF FARM WELLS:          
C     --> CALCULATE EFFECTIVE HEAD LIFT AND EFFECTIVE GROUND-ELEVATION
C         (AV. LIFT AND AV. TOP-ELEVATION OF ALL WELLS IN A FARM WEIGHTED BY THEIR QMAX):
      IF(QTOL.GT.FPS.OR.(IDEFFL.GE.3.AND.TF.GT.0D0)) THEN
      HSUM=0.D0
      GSUM=0.D0
      DO M=1,NFWELS
      IR=IDINT(FWELL(2,M))
      IC=IDINT(FWELL(3,M))
      IL=IDINT(FWELL(1,M))
      FID=IDINT(FWELL(5,M))
      WID=IDINT(FWELL(4,M))
      QMAX=FWELL(6,M)
      IF(FID.EQ.NF) THEN
       IF(IUNITMNW1.GT.0.AND.WID.LT.0) THEN                             ! SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth
         DO N=1,NWELL2                                                  ! SCOTT THE FIRST PART OF     if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then DOES NOTHING EXCEPT FIND IL WHICH IS NEVER USED 
           if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then
             if(n.eq.1)then
               k=IDINT(well2(1,n))
               j = int(mod((k-1),ncol*nrow)/ncol) + 1
               i = mod((k-1),ncol) + 1
               if(ir.eq.j.and.ic.eq.i) then
                 IL=int((k-1)/(ncol*nrow))+1
               endif           
             elseif(n.gt.1)then
               if(well2(8,n-1).lt.1D30)then
                 k = IDINT(well2(1,n))
                 j = int(mod((k-1),ncol*nrow)/ncol) + 1
                 i = mod((k-1),ncol) + 1
                 if(ir.eq.j.and.ic.eq.i) then
                   IL=int((k-1)/(ncol*nrow))+1
                 endif
               endif 
             endif
           elseif(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
             k = IDINT(well2(1,n))
             j = int(mod((k-1),ncol*nrow)/ncol) + 1
             i = mod((k-1),ncol) + 1
             if(ir.eq.j.and.ic.eq.i) then
               HL=GSURF(IC,IR)-WELL2(10,n)
             endif
           endif
         ENDDO
       ENDIF
C GET WATER LEVEL FROM MNW2 WELL seb
       IF(IUNITMNW2.GT.0.AND.WID.LT.0) THEN
         HL=GSURF(IC,IR)-MNW2(17,MNW2LOC(M))
       ENDIF
       IF(WID.GE.0) HL=GSURF(IC,IR)-HNEW(IC,IR,IL)                       ! seb CHANGED FROM IL.GT.0
       HSUM=HSUM+QMAX*HL
       GSUM=GSUM+GSURF(IC,IR)*QMAX
      ENDIF
      ENDDO      
      HLIFT(NF)=HSUM/QMAXF(NF)
      ELHL(NF)=GSUM/QMAXF(NF)                                           !STORE ELEVATION RELATED TO EFFECTIVE HEAD-LIFT, IN CASE THIS IS CAUSING ANY COSTS!
      ENDIF
C
C7E8D---DETERMINE ECONOMIC VALUE FOR GROUNDWATER & SURFACE-WATER IRRIGATED AREAS
C       (note: still within Farms loop!)
C
C     NO MORE USE OF EFFICIENCY IN OBJECTIVE FUNCTION IN FMP3:
C     FOR OPTIMIZATION, EF (FOR RESOURCE CONTRAINTS FOR ENTIRE FARM) AND 
C       EF2D (FOR EACH CELLS CIR) ARE CANCELLED OUT NOW.
C       REASON, FOR THE USE OF EFFICIENY MATRICES, THE COMPOSIT EFFICIENCY OF A FARM
C       MAY BE DIFFERENT THAN THE EFFICIENCY OF PARTICULAR CELLS THAT REMAIN AFTER 
C       THE ACREAGE REDUCTION. THAT IS:
C     THE COST OF DELIVERY IS ASSOCIATED WITH TOTAL DELIVERY (FMP3) AND
C       NOT JUST THE EFFICIENTLY USED CIR (FMP1), AND
C     RESOURCE CONSTRAINTS ARE RELATED TO THE TOTAL AVAILABLE RESOURCE (FMP3) AND
C       NOT JUST THE RESOURCE AVAILABLE FOR EFFICIENT USE (FMP1).
      EF=1D0 !DNINT(EFF(NCROPS+2,NF)*ac)/ac                             !Has to be declared always, since EF is also used in Conservation Pool, which runs always!! 
C
      IF(QTOL.GT.FPS.OR.(IDEFFL.GE.3.AND.TF.GT.0D0)) THEN
      NCELL=0
      DO NC=1,NCROPS
      IF(ICA(NC).NE.0) THEN     
        DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
          IR=FMLOC(NF)%RC(1,I)
          IC=FMLOC(NF)%RC(2,I)
            IF(ICID(IC,IR).EQ.INT(CROPBEN(1,NC))) THEN
               AREA=DBLE(DELR(IC)*DELC(IR))
C              COMPUTE YIELD FROM WATER-PRODUCTION-FUNCTION:
C              - FOR ENGLISH LENGTH UNITS (LENUNI = 1 [FT]),
C                THEN YIELD WILL GENERALLY BE IN [LB/FT2], IF USER USES [LB].
C              - FOR INTERNATIONAL LENGTH UNITS (LENUNI = 2 [CM], LENUNI = 3 [M]),
C                THEN YIELD WILL BE IN [KG/CM2], OR IN [KG/M2], IF USER USES [KG].
C              (THE CHOICE OF THE WEIGHT UNIT DEPENDS ON THE USERS PREFERENCE.
C               IT IS THE USER'S RESPONSIBIITY TO KEEP THE SAME WEIGHT UNIT IN THE
C               OUTPUT-YIELD OF THE WATER-RPODUCTION-FUNCTION
C               AND THE CROP-MARKET-PRICE; e.g. [KG/M2] <--> [$/KG])
               YIELD=CROPBEN(2,NC)*CU2D(IC,IR)/AREA+CROPBEN(3,NC)       !CU2D INTSTEAD OF CU(2,NC) USED BECAUSE OF TIME SERIES AVERAGES OF CU2D IN AD! WITHOUT AVERAGE CU PER TIMESTEP, WE COULD HAVE USED CU(2,NC) FROM RP OF CU(2,NC) TIME SERIES FROM AD
               BENEFIT=(YIELD*CROPBEN(4,NC))
               IF(GSURF(IC,IR).GE.ELHL(NF)) THEN
                  GWLIFTATSURFACE=GSURF(IC,IR)-ELHL(NF)
               ELSE
                  GWLIFTATSURFACE=0.D0
               ENDIF
               IF(GSURF(IC,IR).GE.ELNR(IC,IR)) THEN
                  SWLIFT=GSURF(IC,IR)-ELNR(IC,IR)
               ELSE
                  SWLIFT=0.D0
               ENDIF
               GWCOST=WATERCOST(2,NF) +WATERCOST(3,NF)*HLIFT(NF)
     1                                +WATERCOST(4,NF)*GWLIFTATSURFACE
     2                                +WATERCOST(5,NF)*DWE(IC,IR)
               SWRCOST=WATERCOST(6,NF)+WATERCOST(7,NF)*SWLIFT
     1                                +WATERCOST(8,NF)*DNR(IC,IR)
               SWNRCOST=WATERCOST(9,NF)
               IF(IDINT(CU(3,NC)).EQ.0) FLUX=
     1                               (TDR(IC,IR))/AREA    !*EF2D(IC,IR) !BACK OUT CIR FOR IRRIGATED CROPS
               IF(IDINT(CU(3,NC)).EQ.1) FLUX=0.D0                       !NO IRRIGATION FOR NON-IRRIG. CROPS
               GWPROFIT=BENEFIT-GWCOST*FLUX
               SWRPROFIT=BENEFIT-SWRCOST*FLUX
               SWNRPROFIT=BENEFIT-SWNRCOST*FLUX
               NCELL=NCELL+1
               OPT(1,NCELL)=GWPROFIT
               OPT(2,NCELL)=SWRPROFIT
               OPT(3,NCELL)=SWNRPROFIT
               OPT(4,NCELL)=FLUX
               OPT(5,NCELL)=AREA
               OPT(6,NCELL)=DBLE(IC)
               OPT(7,NCELL)=DBLE(IR)
            ENDIF
         ENDDO
      ENDIF
      ENDDO
      QMXF=QMAXF(NF)*EF
      IF(QR.LT.FPS) QR=0.D0
      ADSW=(TF-ND-QR)*EF
      IF(ADSW.LT.FPS) ADSW=0.D0
      NRDE=ND*EF
C
C7E8E---CALL ACREAGE-OPTIMIZATION SUBROUTINE AND CARRY OUT OPTIMIZATION
C
C-----THE FOLLOWING LINES COULD BE DECLARED IN SUBROUTINE ACREAGE AS PARAMETERS,
C     IF ncon & nopv WERE KNOWN AS CONSTANTS. HOWEVER HERE ncon & nopv DEPEND ON THE NUMBER
C     OF CELLS IN EACH FARM. THEREFORE ncon & nopv AND DEPENDENT PARAMETERS ARE CALCULATED
C     AHEAD OF THE SUBROUTINE:
c     enter ncon=number of constraints = 3+#cells
c          (1. equation:      resource function for groundwater-irrig.-areas only:  q*Xg <= QMAX*EF,
c           2. equation:      resource function for surfacewater-irrig.-areas only: q*Xs <= ADSW*EF,
c           3. equation:      resource function for non-routed-sw-irrig.-areas only: q*Xnr <= NRD*EF,
c           #cells equations: Xg + Xs + Xnr >= Area-max for each cell)
c     enter nopv=number of optimization-variables =3*#cells (#of gw-irrig.cells + #of sw-irrig.cells + #of nrsw-irrig.cells)
c     enter nlser=number of LHS<=RHS, nlger=number of LHS>=RHS, nleqr=number of LHS=RHS    
      ncon=3+ncell
      nopv=3*ncell
      nlser=ncon
      nlger=0                                                           !keep it in here for future constraints
      nleqr=0                                                           !keep it in here for future constraints
      nrowa=ncon+2                                                      !ncon+3
      ncola=nopv+1                                                      !nopv+nlser+nlger (slack variable not part of the a matrix)
C
      CALL ACREAGE(NCELL,QMXF,ADSW,NRDE,NCON,NOPV,NLSER,NLGER,NLEQR,
     1NROWA,NCOLA,GWSUM,SWSUM,NRSUM,EF,NF,KPER,KSTP)
C
C7E8F---EVALUATE CELL-BY-CELL REDUCTION FACTOR AND DETERMINE 
C       PERCENTAGES OF NON-ROUTED SW, (SEMI-)ROUTED SW,AND GW AMONG THE REDUCED FLOWRATE
      DO J=1,NCELL
      ICCELL=IDINT(OPT(6,J))
      IRCELL=IDINT(OPT(7,J))
C     calculate overall reduction factor since beginning of timestep
      REDPCT(ICCELL,IRCELL)=REDPCT(ICCELL,IRCELL)*OPT(8,j)
C     apply total reduction factor of current iteration to calculate reduced TDR for each cell
      CU2D(ICCELL,IRCELL)= CU2D(ICCELL,IRCELL)* OPT(8,J)
      TPPOT(ICCELL,IRCELL)=TPPOT(ICCELL,IRCELL)*OPT(8,J)
      TGW(ICCELL,IRCELL)=  TGW(ICCELL,IRCELL)*  OPT(8,J)
c     GW and SW percentages of reduced flowrates of each cell allow during next iteration the calculation
c     of the exact GWSUM & SWSUM, which may be slightly different from current values (head-dependent)
      IF(OPT(9,J).NE.0.D0.OR.OPT(10,J).NE.0.D0.OR.OPT(11,J).NE.0.D0)THEN
      GWREDPCT(ICCELL,IRCELL)=OPT(9,j)
      SWREDPCT(ICCELL,IRCELL)=OPT(10,J)
      NRREDPCT(ICCELL,IRCELL)=OPT(11,J)
      ENDIF
      ENDDO    
C
C7E8G---UPDATE UPDATE AFDELSW & QREQ IF DEMANDED DELIVERY IS EVEN LESS THAN AVAILABLE SUPPLY
C       DUE TO INSUFFICIENT PROFITABILITY (NRSUM < NRDE; OR SWSUM < ADSW; OR GWSUM < QMXF)
C
C7E8G1--UPDATE NON-ROUTED DELIVERY IF TF CHANGES AS A RESULT OF ACREAGE-OPTIMIZATION
      IF(INRDFL.NE.0) THEN
         if(iallotsw.LT.2) NRD(2,NF)=ND
c
c     Don't adjust ND if ND = "new demand from non-routed deliveries,"
c     or adjust ND to "new demand from non-routed deliveries" if all NRD-use flags are zero (surplus not used)
c     (not really needed but saves unneeded computations)!
         IF(NRSUM.EQ.NRDE) GOTO 790           
         DO IT=1,MXNRDT
            INRDR(IT)=IDINT(UNRD(3*IT,NF))                              !define nrd-ranks as array to find maximum rank for current farm
            INRDU(IT)=IDINT(RNRD(2,IT,NF))                              !define nrd-use flags for a ranked array of nrd-types for current farm
         ENDDO       
         ITTOT=MAXVAL(INRDR)                                            !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
         IF(MAXVAL(INRDU).EQ.0.AND.NRDE.GT.NRSUM) THEN
         ND=NRSUM/EF
         GOTO 790
         ENDIF                                                     
c
c     Determine new Surplus, in case ND > "new demand from non-routed deliveries"
c     Definition of nrd-ranks & nrd-use flags, and of maximum rank for current farm (already done above - outside def. scenarios)
         DSUM=0.0d0
         DO IRT=1,ITTOT 
            DSUM=DSUM+RNRD(1,IRT,NF) 
            ITU=ITTOT-IRT                                               !check for delivery types unnecessary to meet "new demand from non-routed deliveries" (ITU) in sequence of ranking
            IF(NRSUM/EF-DSUM.LE.0.0D0) GOTO 72
         ENDDO
72       IF(NRSUM.LT.FPS) ITU=ITTOT
         ND=0.0D0
         SUFDSUM=0.0D0
         SURPLUS1=0.0D0
         SURPLUS2=0.0D0
         DO IRT=1,ITTOT
           IF(NRSUM/EF.GT.DSUM) THEN
             ND=DSUM                                                    !non-routed delivery can't be greater than the sum of deliveries
           ELSE
             IF(IRT.LT.ITTOT-ITU) THEN
               SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                           !sum of deliveries that are used in their entirety to satisfy "new demand from non-routed deliveries"
             ENDIF
             IF(IRT.EQ.ITTOT-ITU) THEN
               IF(INRDU(IRT).EQ.0) THEN
                 ND=NRSUM/EF                                            !as long as the current Ranked Type IRT exceeds "new demand from non-routed deliveries," only part of IRT will be used & the total ND is equal to "new demand from non-routed deliveries"
               ELSEIF(INRDU(IRT).GT.0) THEN
                 ND=SUFDSUM+RNRD(1,IRT,NF)                              !if not only the sufficient but the abolute amount of IRT is to be used ...
                 IF(INRDU(IRT).EQ.1) SURPLUS1=ND-NRSUM/EF               !gather excess surplus component desired to be recharged into the canal
                 IF(INRDU(IRT).EQ.2) SURPLUS2=ND-NRSUM/EF               !gather excess surplus component desired to be injected into farm wells
               ENDIF
             ENDIF
             IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
               ND=ND+RNRD(1,IRT,NF)                                     !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
               IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)     !gather unneeded surplus components desired to be recharged into the canal
               IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)     !gather unneeded surplus components desired to be injected into farm wells
             ENDIF
           ENDIF
         ENDDO
c
790     CONTINUE
       ENDIF
C
C7E8G2--SAVE BUDGET TERMS AS "PREDEFICIENCY TERMS" IN GENERAL (IN THE ABSENCE OF PRIOR APPROPRITON)
      if (IALLOTSW.LT.2) then
C       store original pumping requirement before updates after acreage-optimization are done
        IF(QREQOLD(NF).EQ.0.0D0) QREQOLD(NF)=QREQ(NF)
        IF(TFDROLD(NF).EQ.0.0D0) TFDROLD(NF)=TFDR(NF)
      ENDIF              
C
C7E8G3--UPDATE SW DELIVERY FROM HEAD-GATE REACH (ROUTED OR SEMI-ROUTED),
C       AND FARM WELL DISCHARGE AS A RESULT OF ACREAGE-OPTIMIZATION (NON-PROFITABILITY OF AVAILABLE SUPPLY!)
      QR=GWSUM/EF-SURPLUS2
      SW=SWSUM/EF-SURPLUS1
      TF=QR+SW+ND
      IF(ADSW-SWSUM.GT.FPS .OR. QMXF-GWSUM.GT.FPS
     1                     .OR. NRDE-NRSUM.GT.FPS) THEN
      IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0)) THEN
C        FSLEN=FCSEGL(NF) !SEEMS REDUNDANT
        AFDELSW=0.0D0                                                   !reset AFDELSW to compute new AFDELSW in case supply > demand for non-fallow crops (for water stacking)
        IFCRIDOLD=0
        DO L=3,NSTRM
        LL=L-1                                                     
           FLOWIN=DBLE(ANINT(STRM(9,LL)*AR))/AC                         !STRM(10,L) ?   seb response STRM(9,L-1) = STRM(10,L)  **note that LL=L-1
           RCHLEN=DBLE(STRM(1,L))
C
C           DETERMINE LOCATION OF HEAD-GATE REACH
C           Farm Reach: * belongs to farm, * is at all a farm reach,
C                       * 1st one being different from previous one,
C                       * 1st one being different from second previous one, meaning
C                         1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point),
C                         but still within the same farm.
         IF(IFCRID(NF,L).EQ.NF .AND. IFCRID(NF,L).GT.0 .AND.
     1     IFCRID(NF,L).NE.IFCRID(NF,L-1).AND.IFCRID(NF,L).NE.
     2     IFCRID(NF,L-2).AND.IFCRID(NF,L).NE.IFCRIDOLD) THEN
C
C          DETERMINE REACH DELIVERY REQUIREMENT AND ACTUAL DELIVERY FROM HEAD-GATE REACH INTO FARM

           IFCRIDOLD=IFCRID(NF,L)
           RDELCOR=SW                                                   !SW is always < flowin or farmallot, so no check for the lesser one needed!!!
C
C                  GENERAL CASE & EQUAL APPROPRIATION
                   IF(IALLOTSW.EQ.1 .AND. FARMALLOT.LT.FLOWIN)
     1             FLOWIN=FARMALLOT                                     !Available Flow = Allotment
                   IF(IALLOTSW.LT.2.AND.RDELCOR.GT.FLOWIN)RDELCOR=FLOWIN
C
C                  PRIOR APPOPRIATION
                   IF(IALLOTSW.GE.2) THEN
C
C                    check if delivery is constrained by water rights calls
                     IF(IALLOTSW.EQ.2) RDELCOR=MIN(RDELCOR,WRC(2,NF))
C
C                    tell to carry out PRIOR by setting IRDRFL=0 for the following case:
C                    a solution was found in previous iteration (ADIV=PDIV; RDELCOR=FLOWIN),
C                    but in current iteration ADIV happens to be < PDIV and therefore also FLOWIN<RDELCOR)
                     if((DABS(rdelcor-flowin).gt.PCLOSE.and.
     1                 abs(SEG(2,ISTRM(4,L))-DVRSFLW(ISTRM(4,L))).GT.   !NEW HERE (as apposed to first call of PRIOR above):
     2                 SNGL(PCLOSE)).or.QTOL.GT.FPS)
     3                 irdrfl(ifcrid(nf,l))=0                           !Call of PRIOR is allowed if QREQ > QMAXF (QTOL.GT.FPS) !! 
c
                     if(irdrfl(ifcrid(nf,L)).eq.1) goto 500
                     IF(IRDRFL(ifcrid(nf,L)).EQ.0) THEN
                      CALL PRIOR(RDELCOR,FLOWIN,L,NF,KPER,KSTP)
                       IF(ITER.EQ.1) then
                         if(iter2.eq.1) iter2=2
                         GOTO 41 !668 !300 
                       ENDIF
                     ENDIF                                              !ENDIF FROM BELOW
C
c                      correction 1: correction of minor inaccuracy after having found a convergence solution
c                      correction 1.a (if tf-nd > 0):
667                    IF(ND.LE.NRSUM/EF.AND.SW.GT.PCLOSE.AND.
     1                 SW-RDELCOR.LT.PCLOSE) THEN
                         SEG(2,ISTRM(4,L))=
     1                   SEG(2,ISTRM(4,L))+SNGL(SW-RDELCOR)
                         RDELCOR=SW
                       ENDIF
c                      correction 1.b (if tf-nd < 0 & surplus < flowinmin):
                       IF(ND.GT.NRSUM/EF.AND.
     1                   DABS(-SURPLUS1-RDELCOR).LT.PCLOSE) THEN
                         SEG(2,ISTRM(4,L))=
     1                   SEG(2,ISTRM(4,L))-SNGL(SURPLUS1+RDELCOR)
                         RDELCOR=-SURPLUS1
                       ENDIF
C
c                      assumption (if suplus > flowinmin):
c                         - surplus at upstream junior farm compensates for flowinmin, and
c                         - additional excess surplus is recharged into canal at upstream junior farm.
                       IF(ND.GT.NRSUM/EF.AND.
     1                 SURPLUS1.GT.FLOWINMIN(L)) THEN
                         SEG(2,ISTRM(4,L))=0.0
                         RDELCOR=-SURPLUS1
                       ENDIF
c                    ENDIF                                             !SHIFT TO UP
                   ENDIF
C
C          save actual delivery from surface water to farm and pull it from canal 
C          (by adding a 'negative recharge' " - RDELCOR " to 'reach-by-reach overland recharge' of SFR package)
668        AFDELSW=RDELCOR
           STRM(12,L)=SNGL(SFRADD(L)-RDELCOR)
           RDR(L)=RDELCOR
           ENDIF
        ENDDO
C
C7E8G4--UPDATE QREQ
C
C       TOTAL FARM DELIVERY REQUIREMENT MINUS ACUTAL FARM DELIVERY FROM SURFACE WATER!
C       redefine QR with new TF and new AFDELSW
C       (only necessary for the case, if indeed some of the reaches' FLOWIN was < prorated SW (RDELCOR))
        QR=TF-ND-AFDELSW
      ELSE
C
C       DETERMINE PUMPING REQUIMENT FOR "GW-ONLY" CASE IN THE ABSENCE OF SURFACE-WATER ROUTING SYSTEM
        QR=TF-ND
      ENDIF                                                             ! END OF CONDITION:  IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0.OR.ISRDFL.GT.0))
C
C7E8H---PRINT AND SAVE SUPPLY AND DEMAND BUDGET INFORMATION PER ITERATION
4031    IF(ISDPFL.EQ.-3) THEN
      IF(LSTCHK(3)) THEN
      WRITE(IOUT,13)'CORRECTION FOR TFDR, NR-SWD, R-SWD, & QREQ IF TOTAL! seb SHIFTED TWO SPACED TO LEFT SO THAT "TOTAL" IS NOT TRUCATED TO "TOT"
     1 AVAILABLE SUPPLY > DEMAND OF PROFITABLE ACREAGE:'
      ENDIF
   13 FORMAT(1X,/,A101)
        ENDIF
C
      ELSE   
C
        IF(ISDPFL.EQ.-3) THEN
      IF(LSTCHK(3)) THEN
      WRITE(IOUT,14)'CORRECTION FOR TFDR,NR-SWD,R-SWD,QREQ IF DEMAND OF ! seb SHIFTED TO LEFT TO FIT ENTIRE LINE
     1REDUCED ACREAGE WAS FORCED TO MATCH SUPPLY:'
      ENDIF
   14 FORMAT(1X,/,A94)
        ENDIF
C 
      ENDIF                                                             ! END OF CONDITION: IF(ADSW-SWSUM.GT.FPS .OR. QMXF-GWSUM.GT.FPS .OR. NRDE-NRSUM.GT.FPS)
C
        IF(ISDPFL.EQ.-3) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,10)
          ENDIF
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,20) NF, EFF(NCROPS+2,NF), TF, ND, TF-ND-QR, QR
          ENDIF
        ENDIF
      TFDR(NF)=TF
      NRD(1,NF)=ND
      QREQ(NF)=QR
C
      ENDIF                                                             ! END OF CONDITION: if (iallotsw.LT.2)
C
C7E8I---"WATER CONSERVATION POOL" ACCOUNTING SYSTEM
C
C     CHECK IF AT LEAST ONE FARM ALONG A SEGMENT SHOWS SWSUM < AFDELSW, LOOP THROUGH ALL FARMS OF A SEGMENT,
C     AND ACCOUNT ACCORDINGLY FOR A REDUCTION OF THE MAIN DIVERSION AMOUNT OF THE SEGMENT.
C     (SINCE A SEGMENT INCLUDES FARMS WITH SWRED AND FARMS WITHOUT, AND AS WELL FARMS WITHOUT ANY
C     ACREAGE-OPTIMIZATION, THIS LOOP HAS TO RUN IN ANY CASE!)
C
      IF(IUNITSFR.GT.0.AND.(IRDFL.NE.0. OR. ISRDFL.GT.0)
     1                .AND.(IDEFFL.EQ.2.OR. IDEFFL.EQ.4) ) THEN
      IFCRIDOLD=0
      DO L=3,NSTRM
C     Asssuming that all farm-reaches belong to the same canal-segment, determine from first
c     reach in a farm, to which segment it belongs and reduce the diversion amount from the
c     stream into that canal by the unused surface-water amount of each farm along that canal.
c     (Note: DIVADD is the Diversion into segment locked in RP, which water can be added to 
c     or subtracted from. The result is the final Diversion-amount which goes into FM of SFR package!)
c     The SWRED is added up for all farms along one segment, where QMAXF<QRED and SWRED occurs.
c     When looping through the reaches-loop, the SWRED of a previous segment is subtracted from
c     the currently specified Diversion into that segment, when the first reach of the next farm is
c     found to belong to a new segment.
c     For the case that no next segment exists anymore (meaning for the last canal-segment in the
c     domain), the cumulative SWRED is subtracted, when the super-ordinate farms-loop reaches
c     the last reach. In case only one canal-segment exists, then the last reach is equal to the
c     first reach.
c     Therefore this loop needs to be executed for ALL farms (not only the ones where QMAXF<QREQ
c     or SWRED occurs), because otherwise - in case the last farm doesn't require a SWRED - no
c     cumulative SWRED would be substracted from the originally specified or calculated Diversion. 
      IF(IFCRID(NF,L).EQ.NF .AND. IFCRID(NF,L).GT.0 .AND.               !do only for reaches along farms
     1 IFCRID(NF,L).NE.IFCRID(NF,L-1) .AND. IFCRID(NF,L).NE.            !determine first reach of a farm: farm-ID of 1st reach is in general different from previous one.                                                                                 !however: corner points reaches may have a zero farm-ID. As long as the second previous reach is still of the same farm-ID, a reach cannot be considered a first reach.
     2 IFCRID(NF,L-2).AND.IFCRID(NF,L).NE.IFCRIDOLD) THEN
      IFCRIDOLD=IFCRID(NF,L)
      NSEG=ISTRM(4,L) 
      DIVSEG=DBLE(SEG(2,ISTRM(4,L)))
      DIVPER=DIVADD(ISTRM(4,L))
      IF(LSTCHK(3)) THEN
        write(iout,*) "divper :",divper
      ENDIF
        IF(NSEG.NE.NSEGOLD) THEN                                         !update diversion for "old" segments as soon as following segments are detected
          IF(NSEGOLD.NE.0) THEN        
            IF(DIVSEGOLD.EQ.DIVPEROLD) THEN    
            DIVSEGOLD=DIVPEROLD-SWRED                                   !for 1st iter. where diversion rate specified per stress period gets reduced
            ELSEIF(DIVSEGOLD.LT.DIVPEROLD) THEN
            DIVSEGOLD=DIVSEGOLD-SWRED                                   !for all following iterations: refer to the latest reduced diversion-rate in from previous iteration(s)
            ENDIF
            IF(DIVSEGOLD.LT.0) DIVSEGOLD=0.D0                           !make sure reduction of the diversion amount cannot be less than zero.
            SEG(2,NSEGOLD)=DBLE(DIVSEGOLD)                              !SEND NEW REDUCED DIVERSION ALREADY DURING CURRENT ITERATION TO SFR-FM, AND FROM THERE TO SOLVER
            DIVTMP(NSEGOLD)=DIVSEGOLD                                   !STORE RED. DIVERSION AMOUNT TEMPORARILY TO SEND IT TO SFR-FM DURING NEXT ITERATION, WHEN ALSO REDUCED CU, PE, ET IS PROCESSED
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,'("diversion from segment# ",I4,":",f20.7)')
     1     NSEGOLD,DIVSEGOLD
           ENDIF
          ENDIF
          SWRED=0.D0                                                    !if farms start to belong to a new segment, start adding up the unused sw-amount again!
        ENDIF
        SWRED=SWRED+(ADSW-SWSUM)/EF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,*) SWRED,ADSW,SWSUM,EF
        ENDIF
        ADSW=0.D0                                                       !reset adsw & swsum from last farm in segment, in order not to add them again to swred
        SWSUM=0.D0                                                      !... in case next farm's unused amount is zero and therefore doesn't overwrite the old values from the previous farm
        IF(NF.EQ.NFARMS) THEN                                           !update diversion for for last segment containing the last farm of domain: when the farms loop reaches the last farm, the cumulative SWRED will be subtracted from the current segment-diversion
            IF(DIVSEG.EQ.DIVPER) THEN            
            DIVSEG=DIVPER-SWRED                                         !for 1st iter. where diversion rate specified per stress period gets reduced
            ELSEIF(DIVSEG.LT.DIVPER) THEN
            DIVSEG=DIVSEG-SWRED                                         !for all following iterations: refer to the latest reduced diversion-rate in from previous iteration(s)
            ENDIF
            IF(DIVSEG.LT.0) DIVSEG=0.D0                                 !make sure reduction of the diversion amount cannot be less than zero.
            SEG(2,NSEG)=DBLE(DIVSEG)                                    !SEND NEW REDUCED DIVERSION ALREADY DURING CURRENT ITERATION TO SFR-FM, AND FROM THERE TO SOLVER
            DIVTMP(NSEG)=DIVSEG                                         !STORE RED. DIVERSION AMOUNT TEMPORARILY TO SEND IT TO SFR-FM DURING NEXT ITERATION, WHEN ALSO REDUCED CU, PE, ET IS PROCESSED
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,'("diversion from segment# ",I4,":",f20.7)')
     1      NSEG,DIVSEG
            ENDIF
            SWRED=0.D0                                                    !after segment-diversion of last segment has been updated within the current iteration, reset SWRED for next iteration!
        ENDIF
      NSEGOLD=NSEG
      DIVSEGOLD=DIVSEG
      DIVPEROLD=DIVPER
      ENDIF
c
      ENDDO
      ENDIF
C-------END OF WATER CONSERVATION POOL------------------------------------------------------------
C-------END OF ACREAGE REDUCTION -----------------------------------------------------------------
C
C8F-----SUM OF EXCESSES OR DEFICIENCIES OF ALL WELLS IN A FARM
C       BY COMPARING THE AVERAGE PUMPING REQUIREMENT WITH THE MAX.CAPACITY OF EACH WELL
ccrth
   80 IF (NWPERF(NF).GT.0) THEN
       IF(IALLOTGW.eq.0)then                                 !Added test for Groundwater Allotment IALLOTGW--rth
        QAVF(NF)=QREQ(NF)/DBLE(NWPERF(NF))
       ELSEIF(IALLOTGW.gt.0)then
           !(ALLOT/QMAXFW)*QREQ / N
           !IF (QRECQ>ALLOT) 
           !SCOTT TESTING ALLOTGW
         IF( QREQ(NF).GT.ALLOTGW(2,NF)) THEN
           QAVF(NF)=ALLOTGW(2,NF)/DBLE(NWPERF(NF))
        END IF
        !
        !QAVF(NF)=QFRACF(NF)*QREQ(NF)/DBLE(NWPERF(NF))
       ENDIF
      ENDIF
ccrth
c 
      QSUMDEF=0.D0
      QSUMEXC=0.D0
      DO M=1,NFWELS
      IF(IDINT(FWELL(5,M)).EQ.NF) THEN  
         IF (FWELL(6,M).LE.QAVF(NF)) THEN
         QDEFICIENCY=QAVF(NF)-FWELL(6,M)
         QSUMDEF=QSUMDEF+QDEFICIENCY
         ELSE
         QEXCESS=FWELL(6,M)-QAVF(NF)
         QSUMEXC=QSUMEXC+QEXCESS
         ENDIF                         
      ENDIF
      ENDDO
      QDEF(NF)=QSUMDEF
      QEXC(NF)=QSUMEXC
C
500   ENDDO                                                             !````````````````````````END OF FARMS LOOP ``````````````````````````````````````
C
C
C9===== DETERMINE NET-RECHARGE FOR EACH FARM CELL, ADD IT TO RHS AND HCOF, AND ===============================
C       DETERMINE SURFACE-WATER RUNOFF FOR EACH FARM CELL:
C       ======================================================================
C
C9A-----DETERMINE NET-RECHARGE AND RELATE IT TO HIGHEST INTERNAL CELL
C       (compare to option 3 in recharge package)
   41 DO IR=1,NROW
      DO IC=1,NCOL
         FNRCH(IC,IR)=0.D0
         DPERC(IC,IR)=0.D0
         SWRUN(IC,IR)=0.D0     
      ENDDO
      ENDDO
C
      DO IR=1,NROW
      DO IC=1,NCOL
      DO IL=1,NLAY
C9A1----FOR ALL FARM CELLS
         TI=0.D0
         TP=0.D0
         TG=0.D0
         TACT=0.D0
         CIR=0.D0
         CECT=0.D0
         EP=0.D0
         EACT=0.D0
         RMIN=0.D0
         RMAXP=0.D0
         RMAX=0.D0
         RIRR=0.D0
         RPREC=0.D0
         SWRUNOFF=0.D0
         DRDC=DBLE(DELR(IC)*DELC(IR))
      IF(IFID(IC,IR).GT.0) THEN
      IF(ICID(IC,IR).EQ.0) THEN
        TTOT(IC,IR)=0.D0
        TPPOT(IC,IR)=0.D0
        TGWA(IC,IR)=0.D0
        ETOT(IC,IR)=0.D0
        EPPOT(IC,IR)=0.D0
        EGWA(IC,IR)=0.D0
      ELSE
C
C9A2----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
C      IF(IBOUND(IC,IR,IL).LT.0) GO TO 150
C
C9A3----IF CELL IS INACTIVE MOVE DOWN A LAYER.
      IF(IBOUND(IC,IR,IL).EQ.0) THEN
        IF(IL.NE.NLAY) GO TO 160 !.OR.ILGR.GT.0
      ENDIF
C
C9A4----DEFINE AND INITIALZIE LOCAL VARIABLES
      HH=HNEW(IC,IR,IL)
      TMAX=DNINT(TGW(IC,IR)*AC)/AC
      TG=  DNINT(TGW(IC,IR)*AC)/AC
      GSE= DNINT(GSURF(IC,IR)*AC)/AC
      TRZ= DNINT(RT2D(IC,IR)*AC)/AC
      SS=GSE-TRZ
      PSIA=SOIL2D(1,IC,IR)
      LXX=SS-PSIA
      P= DNINT(PFLR(IC,IR)*AC)/AC
      TP=DNINT(TPPOT(IC,IR)*AC)/AC
      EP=DNINT(EPPOT(IC,IR)*AC)/AC
      PET=EP+TP
      NC=ICID(IC,IR)
      HCOFF=DBLE(HCOF(IC,IR,IL))  
      hcoffold=hcoff
      RHSS=DBLE(RHS(IC,IR,IL))        
      CIRP=0.D0
      CIR=0.D0
      AVI=0.D0
      TDROLD=0.D0
C
      IF(IIESWFL.GT.0.OR.(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.GT.0))THEN
        IF(NC.GT.0) THEN
        FIESWP=FIESW(2,NC)
        FIESWI=FIESW(3,NC)
        ELSEIF(NC.LT.0) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,42) IC,IR 
        ENDIF
   42   FORMAT(/,1X,'FALLOW CELL IN ROW ',I5,' AND CELL ',I5,' REQUIRES'
     1  ,' FRACTION OF INEFFICIENT LOSSES TO SURFACEWATER RUNOFF ',
     2  'CALCULATED AS RISE-OVER-RUN',/,1X,'SPECIFY IIESWL=0')
        STOP
        ELSE
        FIESWP=0.D0
        FIESWI=0.D0
        ENDIF
      ELSEIF
     1 (IIESWFL.EQ.0.OR.(IIESWFL.EQ.-1.AND.FMPDAT(1)%IIESWFL.EQ.0)) THEN
        FIESWP=RISERUN(IC,IR)
        FIESWI=RISERUN(IC,IR)
      ENDIF
c     for constant head cells or no-flow cells in bottom layer (bedrock layer),
c     no percolation is possible. Assumption: all inefficient losses go to surface water
      IF( IBOUND(IC,IR,IL).LT.0.OR.
     1   (IBOUND(IC,IR,IL).EQ.0.AND.(IL.EQ.NLAY) ) ) THEN !.OR.ILGR.GT.0
      FIESWP=1.D0
      FIESWI=1.D0
      ENDIF
C
C9B-----FOR ALL ACTIVE CROP CELLS (NOT FALLOWED DUE TO FALLOW SEASON & NOT FALLOW CROP-ID):
C         FOR BOTH CONSUMPTIVE USE CONCEPTS (ICCFL=1 AND 3 OR 2 AND 4):
C         RE-COMPUTE THE HEAD-DEPENDENT ACTUAL ET AND COMPUTE NET-RECHARGE FOR EACH CELL
C         AND ADD TERMS TO RHS AND HCOF.
      IF((IROTFL.NE.KPER.AND.IROTFL.GE.0.AND.NC.GT.0).OR.NC.GT.0) THEN
C
      IF(FTE(2,NC).EQ.0.D0)THEN
       CECT=0.D0      
      ELSE
       CECT=(FTE(4,NC)/FTE(2,NC))
      ENDIF
C
C9B1----FOR CONSUMPTIVE USE CONCEPT 1:
C       (USING ANALYTICAL SOLUTION TO DETERMINE T-REDUCTION CAUSED BY WILTING AND ANOXIA, WHICH REQUIRES
C        ROOT WATER UPTAKE STRESS RESPONSE FUNCTION & SOIL PARAMETERS):
C         RE-EVALUATE TRANSPIRATION AFTER POSSIBLE DEFICIENCY SCENARIOS
C         AND CALCULATE:
C         - PRELIMINARY CROP IRRIGATION REQUIREMENT & CROP IRRIGATION REQUIREMENT,
C         - PRELIMINARY FARM NET RECHARGE (DOES NOT YET INCLUDE Egw),
C         AND ADD NON-HEAD-DEP. AND HEAD-DEP. TERMS OF FNRCH TO RHS AND HCOF.
      IF(ICCFL.EQ.1.OR.ICCFL.EQ.3) THEN
C
C9B1A---INITIALIZE PARAMETERS WHICH ENTER ANALYTICAL FUNCTION
      TPOT=0.D0
      DRZ=0.D0
      NEXP=0.D0
      PSIDRY=0.D0
      PSIWET=0.D0
      XWET=0.D0
      XDRY=0.D0
      UXX=0.D0
      MXX=0.D0
C
C9B1B---DETERMINE REDUCTION-PERCENTAGE FO Tgw FROM PREVIOUS ITERATION (FOR ACREAGE-OPTIMIZATION)
      IF(TGWO(IC,IR).GT.0D0)  PCTG=TMAX/TGWO(IC,IR)
      IF(TGWO(IC,IR).EQ.0D0)  PCTG=1D0
C
C9B1C---CONVERT FLOWRATE OF ORIGINAL MAXIMUM TRANSPIRATION BACK TO FLUX
C       (REASON: UXX AND MXX BELOW ARE FUNCTIONS OF T-FLUX NOT T-FLOWRATE; WITH:
C        UXX =    Head Elevation of Upper Extinction of Transpiration due to Anoxia
C        MXX =    Head Elevation of Elimination of crop-unproductive Wilting Zone)
      TMAX=TGWO(IC,IR)/DRDC                                             !here: TMAX = potential crop transpiration Tc-pot
      TPOT=TMAX
      RRZ=GSE-HH
C
C9B1D---DEFINE STRESS RESPONSE FUNCTION FOR UPTAKE FROM UNSATURATED ROOT ZONE:
C       PRESSURE HEADS AT WHICH UPTAKE IS ZERO [PSI0,PSI3] OR OPTIMAL [PSI1,PSI2]
      IF(PSI(2,NC).LT.0.D0) THEN
         PSI0=DABS(PSI(2,NC))
      ELSE
         PSI0=0.D0
      ENDIF
      IF(PSI(3,NC).LT.0.D0) THEN
         PSI1=DABS(PSI(3,NC))
      ELSE
         PSI1=0.D0
      ENDIF
      IF(PSI(4,NC).LT.0.D0) PSI2=DABS(PSI(4,NC))
      IF(PSI(5,NC).LT.0.D0) PSI3=DABS(PSI(5,NC))
C
C9B1E---APPLY INSTRINSIC ANALYTICAL SOLUTIONS FOR SPECIFIED SOIL TYPES, OR
C       FORMULATE ANALTYICAL SOLUTIONS USING USER-SPECIFED COEFFICIENTS FOR:
C       DRZ - DEPLETED ROOT ZONE  ( DRZ = FUNCTION(A,B,C,Tc-pot,TRZ) )        
C       N   - SINUOSITY COEFFICIENT ( N = FUNCTION(D,E,DRZ)   )
C     
      IF(TPOT.LE.0.D0) GOTO 52      
      IF(IDINT(SOIL2D(2,IC,IR)).EQ.-999) THEN                           !DEFAULT ANALYTICAL SOLUTION FOR SILT
      DRZ=(DEXP(0.320D0*DLOG(TRZ*MLT)+                        
     1(-0.329D0)*DLOG(TPOT*MLT)+2.852D0))                               !0.320149668    -0.328586793    2.85192125
      NEXP=1.303D0*DLOG(DRZ)-2.042D0                                    !y = 1.3027Ln(x) - 2.0416
      ELSEIF(IDINT(SOIL2D(2,IC,IR)).EQ.-998) THEN                       !DEFAULT ANALYTICAL SOLUTION FOR SANDYLOAM
      DRZ=(DEXP(0.201D0*DLOG(TRZ*MLT)+
     1(-0.195D0)*DLOG(TPOT*MLT)+3.083D0))                               !0.200738267    -0.195485538    3.083110101         
      NEXP=3.201D0*DLOG(DRZ)-3.903D0                                    !y = 3.2012Ln(x) - 3.9025
      ELSEIF(IDINT(SOIL2D(2,IC,IR)).EQ.-997) THEN                       !DEFAULT ANALYTICAL SOLUTION FOR SILTYCLAY
      DRZ=(DEXP(0.348D0*DLOG(TRZ*MLT)+
     1(-0.327D0)*DLOG(TPOT*MLT)+1.731D0))                               !0.348098866    -0.327445062    1.730759566
      NEXP=0.530D0*DLOG(DRZ)-0.377D0                                    !y = 0.5298Ln(x) - 0.3767
      ELSE                                                              !READ IN FUNCTION FOR ANALYTICAL SOLUTION
      DRZ=(DEXP(SOIL2D(2,IC,IR)*DLOG(TRZ*MLT)+
     1SOIL2D(3,IC,IR)*DLOG(TPOT*MLT)+SOIL2D(4,IC,IR)))            
      NEXP=SOIL2D(5,IC,IR)*DLOG(DRZ)+SOIL2D(6,IC,IR)
      ENDIF
      IF(DRZ.GT.PSI3*MLT) DRZ=PSI3*MLT
      IF(NEXP.LT.1D-30) NEXP=1D-30
C
C9B1F---EVALUATE PRESSURE HEADS BETWEEN WHICH UPTAKE IS CONSIDERED OPTIMAL    
      PSIDRY=(PSI2+PSI3)/2.D0
      PSIWET=(PSI0+PSI1)/2.D0
      IF(PSIWET.EQ.0D0) THEN
      XWET=0.D0
      ELSE      
C
C9B1G---SOLVE ANALYTICAL FUNCTION FOR DEPTHS BETWEEN WHICH UPTAKE IS OPTIMAL:
C       ANALYTICAL FUNCTION: PSI(DEPTH) = FUNCTION (DRZ,NEXP,PSI3,DEPTH)
C                            PSI(DEPTH)   IS SOLVED ITERATIVELY BY BISECTION-METHOD
C                                         FOR DEPTH(PSIWET) AND FOR DEPTH(PSIDRY)
      XWET=RTFUNC(DRZ,PSI3*MLT,NEXP,PSIWET*MLT,EPS,ic,ir)/MLT    !DRZ ORIGINALLY DERIVED IN CM
      ENDIF
      XDRY=RTFUNC(DRZ,PSI3*MLT,NEXP,PSIDRY*MLT,EPS,ic,ir)/MLT
      IF(XWET.LT.0D0) XWET=0D0     
   52 IF(TPOT.LE.0.D0) THEN
      XWET=PSIWET
      XDRY=PSIDRY
      ENDIF   
C
C9B1H---DETERMINE: UXX =  Head Elevation of Upper Extinction of Transpiration due to Anoxia
C                  MXX =  Head Elevation of Elimination of crop-unproductive Wilting Zone
      UXX=GSE-XWET
      if(xwet.gt.psiwet) uxx=gse-psiwet
      if(xwet.lt.0d0)    uxx=gse
      if(xwet.gt.trz)    uxx=ss
      MXX=GSE-XDRY      
      IF(xdry.gt.trz)    mxx=ss
C
C9B1I---DETERMINE MAXIMUM POSSIBLE TRANSPIRATION FROM GROUNDWATER 
      TMAX=(TPOT/TRZ)*(UXX-MXX)
      TMAX=TMAX*DRDC*PCTG
C
C9B1J---RECOMPTE CIR AND TDROLD (demand before applying deficit scenario) in order
C       - to compare them to the available irrigation after the application of a deficit scenario, AVI (supply),
C       - and to determine, if
C       (1) no irrigation related inefficient losses occur (if AVI < CIR), or
C       (2) the irrigation related losses are equal to "AVI - CIR" 
C       instead of the regular case, where the TDR is met: "TDR - CIR".      
C       (note that (1) is reached by setting efficiency=1 in (3)
C
C9B1J1--RECOMPUTE CIR:
C       TO SATISFY THE "PRELIMINARY CROP IRRIGATION REQUIREMENT" (= TRANSPIRATORY IRRIGATION REQUIREMENT):
C           (1) USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND
C       TO COMPUTE THE TOTAL CROP IRRIGATION REQUIREMENT:
C           (2) INCREASE THE TRANSPIRATORY IRRIGATION REQUIREMENT PROPORTIONALLY FOR EVAPORATIVE LOSSES
C               (PROPORTIONALITY FACTOR: FRACTION OF TRANSPIRATION / FRACTION OF EVAPORATION)
      IF(HH.GT.MXX) THEN
      CIR=0.D0
      ELSEIF(HH.LT.SS) THEN
      CIR=(TPOT/TRZ)*(UXX-SS)*DRDC*PCTG-TGWA(IC,IR)                     !TGWA is either equal to TACT (sufficiency case, or eventual acreage reductions) or zero for waterstacking!
      ELSE
      CIR=(TPOT/TRZ)*(UXX-HH)*DRDC*PCTG-TGWA(IC,IR)
      ENDIF
C       USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND ADJUST CIR FOR T + E COMPONENTS
      IF(IDINT(CU(3,NC)).EQ.1.OR.TP.GE.CIR) THEN
      CIR=0.D0
      ELSE
      CIR=(CIR-TP)*(1.D0+CECT)                                          !CIR here needed to calc. TDROLD, and to be compared later to AVI
      ENDIF
C
C9B1J2--FOR DEFICIT IRRGIATION WITH OR WITHOUT WATER-STACKING:
C       RECOMPUTE TDROLD (BEFORE DEFICIT SCENARIO) & AND AVI (AFTER DEFICIT SCENARIO)
      IF(IDEFFL.LT.0) THEN
      TDROLD=CIR/EF2D(IC,IR)
      TDROLD=dnint(TDROLD*ac)/ac       
      AVI=TDR(IC,IR)
      EFOLD=EF2D(IC,IR)
C
C       NO INEFFICIENT LOSSES OCCUR (WITH IRRIGATION < CIR):
C       EFFICIENCY IS SET TO 1 (100%), SO THAT RECHARGE RELATED TO IRRIGATION [CIR/EF-CIR]
C       WILL BE ZERO AND THE ONLY RECHARGE COMPONENTS ARE [P-Tp-Ep]*(1-IEswP) - ETgw.
      IF(IDEFFL.LT.0 .AND. AVI.LE.CIR) EF2D(IC,IR)=1.D0                 !(see 1 in 11B1J) ; for fallowed non-crop cells (water-stacking): cir,tgwa,pe are all zero  
c
C       IF INEFFICENT LOSSES OCCUR:
C       COMPUTE MINIMUM AND MAXIMUM RECHARGE TERMS THAT DO NOT CHANGE WITH HEAD
      IF(IDEFFL.LT.0.AND.AVI.GT.CIR.AND.DABS(AVI-TDROLD).GT.FPS) THEN   !(see 2 in 11B1J)
      RMIN=AVI*(1.D0-FIESWI)+(P-EP)*(1.D0-FIESWP)
      IF(HH.GE.SS) YY=0.D0
      IF(HH.LT.SS) YY=SS
      RMAXP=AVI*(1.D0-FIESWI)+(P-EP-
     1(TPOT/TRZ)*(UXX-YY)*DRDC*PCTG)*(1.D0-FIESWP)
      RMAX=(AVI-((TPOT/TRZ)*(UXX-YY)*DRDC*PCTG-TP)*(1.D0+CECT))*
     1(1.D0-FIESWI)+(P-PET)*(1.D0-FIESWP)
      GO TO 91
      ENDIF
      ENDIF
C
C9B1K---FOR REGULAR CASE (NO DEFICIT) AND FOR ACREAGE-OPTIMIZATION:
C       COMPUTE MINIMUM AND MAXIMUM RECHARGE TERMS THAT DO NOT CHANGE WITH HEAD
      IF(IDINT(CU(3,NC)).EQ.1) THEN
      EFOLD=EF2D(IC,IR)
      EF2D(IC,IR)=1.D0                                                  !sets irrigation related recharge of non-irrigation cells to zero (is reset to saved efficiency later!)
      ENDIF
      RMIN=(P-EP)*(1.D0-FIESWP)                                         !(3)
      IF(HH.GE.SS) YY=0.D0
      IF(HH.LT.SS) YY=SS
      RMAXP=(P-EP-
     1(TPOT/TRZ)*(UXX-YY)*DRDC*PCTG)*(1.D0-FIESWP)
      RMAX=((TPOT/TRZ)*(UXX-YY)*DRDC*PCTG-TP)*(1.D0+CECT)*
     1(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+(P-PET)*(1.D0-FIESWP)
      IF(((TPOT/TRZ)*(UXX-YY)*DRDC*PCTG-TP).LT.0)THEN
      PET=(TPOT/TRZ)*(UXX-YY)*DRDC*PCTG+EP
      RMAX=(P-PET)*(1.D0-FIESWP)
      ENDIF
C
C9B1L---EVALUATE THE FOLLOWING TERMS FOR VARIOUS RANGES OF WATER-LEVEL ELEVATIONS:
C       - ACTUAL TRANSPIRATION FROM GROUNDWATER (TACT);
C       - PRELIMINARY CROP IRRIGATION REQUIREMENT (CIRP) & CROP IRRIGATION REQUIREMENT (CIR);
C       - PRELIMINARY FARM NET RECHARGE (FNRCH; DOES NOT YET INCLUDE Egw).
C       ADD NON-HEAD-DEPENDENT PART OF FNRCH TO RIGHT-HAND-SIDE OF FINTIE DIFFERENCE EQUATION, AND
C       ADD HEAD-DEPENDENT PART OF FNRCH TO CENTER-NODE HEAD-COEFFICIENT (HCOF).
C       
   91 IF(HH.GE.UXX) THEN
      TACT=0.D0
      CIR=0.D0
      IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
        IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0) RMIN=0.D0
      ENDIF
      FNRCH(IC,IR)=RMIN
      RHSS=RHSS-RMIN
      ENDIF
C      
      IF(HH.GT.MXX .AND. HH.LT.UXX) THEN   
      THI=(TPOT/TRZ)*UXX
      THD=TPOT/TRZ
      THI=THI*(DRDC*PCTG)
      THD=THD*(DRDC*PCTG)
      TACT=THI-THD*HH
      CIR=0.D0
      IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
        IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0) RMIN=0.D0
      ENDIF
      FNRCH(IC,IR)=RMIN-TACT      
      RHSS=RHSS-RMIN+THI
      HCOFF=HCOFF+THD
      ENDIF
C
      IF(HH.GE.SS .AND. HH.LE.MXX) THEN
      TACT=TMAX
      CIRP=(TPOT/TRZ)*(UXX-HH)*(DRDC*PCTG)-TACT                         !WILL BE UPDATED BELOW FOR ALL ICCFL
      IF(IDEFFL.LT.0.AND.AVI.GT.CIR.AND.DABS(AVI-TDROLD).GT.FPS) THEN 
          IF(TP.GE.CIRP) THEN
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TMAX
              RHSS=RHSS+TMAX
              ELSE
              FNRCH(IC,IR)=RMAXP-TMAX*FIESWP+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)*HH
              RHSS=RHSS-RMAXP+TMAX*FIESWP
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)
              ENDIF
            ELSE
              FNRCH(IC,IR)=RMAXP-TMAX*FIESWP+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)*HH
              RHSS=RHSS-RMAXP+TMAX*FIESWP
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)      
            ENDIF
          CIR=0.D0
          ELSE
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TMAX              
              RHSS=RHSS+TMAX
              ELSE
              FNRCH(IC,IR)=
     1        RMAX+TMAX*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)+
     2        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0+CECT)*(1.D0-FIESWI)*HH
              RHSS=RHSS-RMAX-TMAX*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0+CECT)*(1.D0-FIESWI)
              ENDIF
            ELSE
              FNRCH(IC,IR)=
     1        RMAX+TMAX*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)+
     2        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0+CECT)*(1.D0-FIESWI)*HH
              RHSS=RHSS-RMAX-TMAX*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0+CECT)*(1.D0-FIESWI)
            ENDIF
          ENDIF
      ELSE
          IF(TP.GE.CIRP) THEN
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TMAX              
              RHSS=RHSS+TMAX
              ELSE
              FNRCH(IC,IR)=RMAXP-TMAX*FIESWP+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)*HH
              RHSS=RHSS-RMAXP+TMAX*FIESWP
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)
              ENDIF
            ELSE          
              FNRCH(IC,IR)=RMAXP-TMAX*FIESWP+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)*HH
              RHSS=RHSS-RMAXP+TMAX*FIESWP
              HCOFF=HCOFF+
     1        (TPOT/TRZ)*(DRDC*PCTG)*(1.D0-FIESWP)
            ENDIF
          CIR=0.D0
          ELSE
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TMAX              
              RHSS=RHSS+TMAX
              ELSE
              FNRCH(IC,IR)=RMAX-TMAX*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*
     1        (1.D0-FIESWI)+1.D0)-(TPOT/TRZ)*(DRDC*PCTG)*
     2        (1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)*HH
              RHSS=RHSS-RMAX+
     1        TMAX*((1.D0+CECT)*(1/EF2D(IC,IR)-1)*(1.D0-FIESWI)+1.D0)
              HCOFF=HCOFF-(TPOT/TRZ)*(DRDC*PCTG)*
     2        (1.D0+CECT)*(1/EF2D(IC,IR)-1)*(1.D0-FIESWI)
              ENDIF
            ELSE          
              FNRCH(IC,IR)=RMAX-TMAX*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*
     1        (1.D0-FIESWI)+1.D0)-(TPOT/TRZ)*(DRDC*PCTG)*
     2        (1.D0+CECT)*(1/EF2D(IC,IR)-1)*(1.D0-FIESWI)*HH
              RHSS=RHSS-RMAX+
     1        TMAX*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              HCOFF=HCOFF-(TPOT/TRZ)*(DRDC*PCTG)*
     2        (1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)
            ENDIF
          ENDIF
      ENDIF
      ENDIF
C
      IF(HH.LT.SS. AND. HH.GT.LXX) THEN
      THI=TMAX*(1-SS/PSIA)                
      THD=TMAX/PSIA       
      TACT=THI+THD*HH                         
      CIRP=(TPOT/TRZ)*(UXX-SS)*(DRDC*PCTG)-TACT
      IF(IDEFFL.LT.0.AND.AVI.GT.CIR.AND.DABS(AVI-TDROLD).GT.FPS) THEN
          IF(TP.GE.CIRP) THEN
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TACT
              RHSS=RHSS+THI
              HCOFF=HCOFF-THD
              ELSE
              FNRCH(IC,IR)=RMAXP-TACT*FIESWP
              RHSS=RHSS-RMAXP+THI*FIESWP
              HCOFF=HCOFF-THD*FIESWP
              ENDIF            
            ELSE          
              FNRCH(IC,IR)=RMAXP-TACT*FIESWP
              RHSS=RHSS-RMAXP+THI*FIESWP
              HCOFF=HCOFF-THD*FIESWP
            ENDIF
          CIR=0.D0
          ELSE
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TACT
              RHSS=RHSS+THI
              HCOFF=HCOFF-THD
              ELSE
              FNRCH(IC,IR)=RMAX+TACT*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              RHSS=RHSS-RMAX-THI*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              HCOFF=HCOFF+THD*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              ENDIF              
            ELSE
              FNRCH(IC,IR)=RMAX+TACT*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              RHSS=RHSS-RMAX-THI*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
              HCOFF=HCOFF+THD*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
            ENDIF
          ENDIF
      ELSE
          IF(TP.GE.CIRP) THEN
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TACT
              RHSS=RHSS+THI
              HCOFF=HCOFF-THD
              ELSE
              FNRCH(IC,IR)=RMAXP-TACT*FIESWP             
              RHSS=RHSS-RMAXP+THI*FIESWP
              HCOFF=HCOFF-THD*FIESWP
              ENDIF              
            ELSE
              FNRCH(IC,IR)=RMAXP-TACT*FIESWP             
              RHSS=RHSS-RMAXP+THI*FIESWP
              HCOFF=HCOFF-THD*FIESWP
            ENDIF
          CIR=0.D0
          ELSE
            IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
              IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0)THEN
              FNRCH(IC,IR)=-TACT
              RHSS=RHSS+THI
              HCOFF=HCOFF-THD
              ELSE
              FNRCH(IC,IR)=RMAX-
     1        TACT*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              RHSS=RHSS-RMAX+
     1        THI*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              HCOFF=HCOFF-
     1        THD*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              ENDIF
            ELSE
              FNRCH(IC,IR)=RMAX-
     1        TACT*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              RHSS=RHSS-RMAX+
     1        THI*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
              HCOFF=HCOFF-
     1        THD*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
            ENDIF
          ENDIF   
      ENDIF
      ENDIF
C
      IF(HH.LE.LXX) THEN
      TACT=0.D0
      CIRP=(TPOT/TRZ)*(UXX-SS)*(DRDC*PCTG)
      IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
        IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0) RMAX=0.D0
      ENDIF
      FNRCH(IC,IR)=RMAX      
      RHSS=RHSS-RMAX
      ENDIF
C
C9B1M---USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND
C       ADJUST PRELIMINARY CIR FOR T + E COMPONENTS
      IF(TP.GE.CIRP) THEN
      TP=CIRP 
      CIR=0.D0
      ELSE
      CIR=(CIRP-TP)*(1.D0+CECT)                                         !NOT NEEDED FOR FDE, BUT FOR RPREC --> TO DETERMINE SW-RUNOFF
      IF(IDINT(CU(3,NC)).EQ.1.AND.NC.GT.0) CIR=0.D0
      ENDIF
C
      IF(IDEFFL.LT.0.OR.(IDINT(CU(3,NC)).EQ.1.AND.NC.GT.0)) THEN
      EF2D(IC,IR)=EFOLD                                                 !reset efficiency
      ENDIF
C     
      ENDIF                                                             !END OF CONDITION: IF(ICCFL.EQ.1.OR.ICCFL.EQ.3)
C
C
C9B2----FOR CONSUMPTIVE USE CONCPET 2 (WITHOUT SOIL AND PLANT PARAMETERS (BUT WITH CAPILLARY FRINGE & ROOTZONE))
C         RE-EVALUATE TRANSPIRATION AFTER POSSIBLE DEFICIENCY SCENARIOS
C         AND CALCULATE:
C         - PRELIMINARY CROP IRRIGATION REQUIREMENT & CROP IRRIGATION REQUIREMENT,
C         - PRELIMINARY FARM NET RECHARGE (DOES NOT YET INCLUDE Egw),
C         AND ADD NON-HEAD-DEP. AND HEAD-DEP. TERMS OF FNRCH TO RHS AND HCOF.   
      IF(ICCFL.EQ.2.OR.ICCFL.EQ.4) THEN
C     
C9B2A---RECOMPTE CIR AND TDROLD (demand before applying deficit scenario) in order
C       - to compare them to the available irrigation after the application of a deficit scenario, AVI (supply),
C       - and to determine, if
C       (1) no irrigation related inefficient losses occur (if AVI < CIR), or
C       (2) the irrigation related losses are equal to "AVI - CIR" 
C       instead of the regular case, where the TDR is met: "TDR - CIR".      
C       (note that (1) is reached by setting efficiency=1 in (3)
C
C9B2A1--RECOMPUTE CIR:
C       TO SATISFY THE "PRELIMINARY CROP IRRIGATION REQUIREMENT" (= TRANSPIRATORY IRRIGATION REQUIREMENT):
C           (1) USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND
C       TO COMPUTE THE TOTAL CROP IRRIGATION REQUIREMENT:
C           (2) INCREASE THE TRANSPIRATORY IRRIGATION REQUIREMENT PROPORTIONALLY FOR EVAPORATIVE LOSSES
C               (PROPORTIONALITY FACTOR: FRACTION OF TRANSPIRATION / FRACTION OF EVAPORATION)

      IF(HH.GT.SS) THEN
      CIR=0.D0
      ELSE
      CIR=TG-TGWA(IC,IR)
      ENDIF
C       USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND ADJUST CIR FOR T + E COMPONENTS
      IF(TP.GE.CIR) THEN
      CIR=0.D0
      ELSE
      CIR=(CIR-TP)*(1.D0+CECT)                                             !CIR here needed to calc. TDROLD, and to be compared later to AVI
      ENDIF
C
C9B2A2--FOR DEFICIT IRRGIATION WITH OR WITHOUT WATER-STACKING:
C       RECOMPUTE TDROLD (BEFORE DEFICIT SCENARIO) & AND AVI (AFTER DEFICIT SCENARIO)
      IF(IDEFFL.LT.0) THEN
      TDROLD=CIR/EF2D(IC,IR)
      TDROLD=dnint(TDROLD*ac)/ac       
      AVI=TDR(IC,IR)
      EFOLD=EF2D(IC,IR)
C
C       NO INEFFICIENT LOSSES OCCUR (WITH IRRIGATION < CIR):
C       EFFICIENCY IS SET TO 1 (100%), SO THAT RECHARGE RELATED TO IRRIGATION [CIR/EF-CIR]
C       WILL BE ZERO AND THE ONLY RECHARGE COMPONENTS ARE [P-Tp-Ep]*(1-IEswP) - ETgw.
      IF(IDEFFL.LT.0 .AND. AVI.LE.CIR) EF2D(IC,IR)=1.D0                 !(see 1 in 11B2A)               !for fallowed non-crop cells (water-stacking): cir,tgwa,pe are all zero
C
C       IF INEFFICENT LOSSES OCCUR:
C       COMPUTE MINIMUM AND MAXIMUM RECHARGE TERMS THAT DO NOT CHANGE WITH HEAD         
      IF(IDEFFL.LT.0.AND.AVI.GT.CIR.AND.DABS(AVI-TDROLD).GT.FPS) THEN   !(see 2 in 11B2A)
      RMIN=AVI*(1.D0-FIESWI)+(P-EP)*(1.D0-FIESWP)
      RMAXP=AVI*(1.D0-FIESWI)+(P-EP-TG)*(1.D0-FIESWP)
      RMAX=(AVI-
     1(TG-TP)*(1.D0+CECT))*(1.D0-FIESWI)+(P-PET)*(1.D0-FIESWP)
      GO TO 90
      ENDIF
      ENDIF
C
C9B2B---FOR REGULAR CASE (NO DEFICIT) AND FOR ACREAGE-OPTIMIZATION:
C       COMPUTE MINIMUM AND MAXIMUM RECHARGE TERMS THAT DO NOT CHANGE WITH HEAD
      IF(IDINT(CU(3,NC)).EQ.1) THEN
      EFOLD=EF2D(IC,IR)
      EF2D(IC,IR)=1.D0                                                  !sets irrigation related recharge of non-irrigation cells to zero (is reset to saved efficiency later!)
      ENDIF
      RMIN=(P-EP)*(1.D0-FIESWP)                                          !(3)
      RMAXP=(P-EP-TG)*(1.D0-FIESWP)
      RMAX=(TG-TP)*(1.D0+CECT)*
     1(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+(P-PET)*(1.D0-FIESWP)
      IF((TG-TP).LT.0)THEN
      PET=TG+EP
      RMAX=(P-PET)*(1.D0-FIESWP)
      ENDIF
C
C9B2C---EVALUATE THE FOLLOWING TERMS FOR VARIOUS RANGES OF WATER-LEVEL ELEVATIONS:
C       - ACTUAL TRANSPIRATION FROM GROUNDWATER (TACT);
C       - PRELIMINARY CROP IRRIGATION REQUIREMENT (CIRP) & CROP IRRIGATION REQUIREMENT (CIR);
C       - PRELIMINARY FARM NET RECHARGE (FNRCH; DOES NOT YET INCLUDE Egw).
C       ADD NON-HEAD-DEPENDENT PART OF FNRCH TO RIGHT-HAND-SIDE OF FINTIE DIFFERENCE EQUATION, AND
C       ADD HEAD-DEPENDENT PART OF FNRCH TO CENTER-NODE HEAD-COEFFICIENT (HCOF).
C
   90 IF(HH.GT.GSE) THEN
      TACT=0.D0
      CIR=0.D0                                 
      FNRCH(IC,IR)=RMIN-TACT
      RHSS=RHSS-RMIN+TACT
      ELSE IF(HH.GT.SS) THEN                      
      THI=TMAX*(GSE/TRZ)
      THD=-TMAX/TRZ
      TACT=THI+THD*HH
      RHSS=RHSS-RMIN+THI
      HCOFF=HCOFF-THD
      CIR=0.D0                                 
      FNRCH(IC,IR)=RMIN-TACT
      ELSE IF(HH.LT.LXX) THEN
      TACT=0.0D0
      CIRP=TG
      FNRCH(IC,IR)=RMAX
      RHSS=RHSS-RMAX  
      ELSE
      THI=TMAX*(1-SS/PSIA)
      THD=TMAX/PSIA
      TACT=THI+THD*HH         
      CIRP=TG-TACT         
      IF(IDEFFL.LT.0.AND.AVI.GT.CIR.AND.DABS(AVI-TDROLD).GT.FPS) THEN 
          IF(TP.GE.CIRP) THEN
          FNRCH(IC,IR)=RMAXP-TACT*FIESWP
          RHSS=RHSS-RMAXP+THI*FIESWP
          HCOFF=HCOFF-THD*FIESWP
          CIR=0.D0
          ELSE
          FNRCH(IC,IR)=RMAX+TACT*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
          RHSS=RHSS-RMAX-THI*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
          HCOFF=HCOFF+THD*((1.D0+CECT)*(1.D0-FIESWI)-1.D0)
          ENDIF
      ELSE
          IF(TP.GE.CIRP) THEN
          FNRCH(IC,IR)=RMAXP-TACT*FIESWP
          RHSS=RHSS-RMAXP+THI*FIESWP
          HCOFF=HCOFF-THD*FIESWP
          CIR=0.D0
          ELSE
          FNRCH(IC,IR)=RMAX-
     1    TACT*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
          RHSS=RHSS-RMAX+
     1    THI*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
          HCOFF=HCOFF-
     1    THD*((1.D0+CECT)*(1/EF2D(IC,IR)-1.D0)*(1.D0-FIESWI)+1.D0)
          ENDIF    
      ENDIF      
      ENDIF
C
C9B2D---USE PRIMARILY TRANSPIRATION FROM PRECIPITATION IF AVAILABLE, AND
C       ADJUST PRELIMINARY CIR FOR T + E COMPONENTS
      IF(TP.GE.CIRP) THEN
      TP=CIRP 
      CIR=0.D0
      ELSE
      CIR=(CIRP-TP)*(1.D0+CECT) 
      IF(IDINT(CU(3,NC)).EQ.1.AND.NC.GT.0) CIR=0.D0
      ENDIF
C
      IF(IDEFFL.LT.0.OR.(IDINT(CU(3,NC)).EQ.1.AND.NC.GT.0)) THEN
      EF2D(IC,IR)=EFOLD                                                 !reset efficiency
      ENDIF
C
      ENDIF                                                             !END OF CONDITION: IF(ICCFL.EQ.2.OR.ICCFL.EQ.4)
C
C9C-----FOR ALL CELLS FALLOWED DUE TO FALLOW SEASON OR DUE TO FALLOW CROP-ID:
C         FOR BOTH CONSUMPTIVE USE CONCEPTS (ICCFL=1 AND 3 OR 2 AND 4):
C         PRECIPITATION RELATED TO NET RECHARGE OCCURS IN FALLOWED CELLS!
C         - CALCULATE PRELIMINARY FARM NET RECHARGE (DOES NOT YET INCLUDE Egw), AND
C         - ADD NON-HEAD-DEP. FNRCH TO RHS.
      ELSEIF(IROTFL.EQ.KPER.OR.ICID(IC,IR).EQ.-1) THEN        
      TTOT(IC,IR)=0.D0
      TPPOT(IC,IR)=0.D0
      TGWA(IC,IR)=0.D0
      FNRCH(IC,IR)= (P-EP)*(1.D0-FIESWP)
      RHSS=RHSS-FNRCH(IC,IR)                                  
      ENDIF
C
      TGWA(IC,IR)=TACT
      TI=CIR/(1.D0+CECT)
      IF(TI.LE.0D0) TI=0D0
      IF(IDEFFL.LT.0.AND.AVI.LT.CIR) TI=AVI/(1.D0+CECT)
      TTOT(IC,IR)=TI+TP+TACT
      IF(NC.GT.0)THEN
        IF(IDINT(CU(3,NC)).EQ.1) TTOT(IC,IR)=TP+TACT
      ENDIF
C
C9D-----EVAPORATION FROM GROUND-WATER (SEPARATE FORM TRANSPIRATION, SINCE MUTUALLY INDEP. IF CIR,TDR,AFDELSW,Q)   
C
C9D1----DEFINE LOCAL VARIABLES
      EPOT=DNINT(EGW(IC,IR)*AC)/AC
      EPOT=EPOT-EP
      XX=GSE-PSIA                         
C
C9D2----EVALUATE ACTUAL EVAPORATION FROM GROUNDWATER (EACT) FOR VARIOUS RANGES OF WATER-LEVEL ELEVATIONS:
C       ADD NON-HEAD-DEPENDENT PART OF EACT TO RIGHT-HAND-SIDE OF FINTIE DIFFERENCE EQUATION, AND
C       ADD HEAD-DEPENDENT PART OF EACT TO CENTER-NODE HEAD-COEFFICIENT (HCOF)
      IF(HH.GT.GSE) THEN                      
      EACT=EPOT
      RHSS=RHSS+EACT
      ELSE IF(HH.LT.XX) THEN
      EACT=0.0D0
      ELSE
      EHI=EPOT*(1-GSE/PSIA)
      EHD=EPOT/PSIA
      EACT=EHI+EHD*HH         
      RHSS=RHSS+EHI                                                     !RHSS+eact  for solving nonlinearly
      HCOFF=HCOFF-EHD 
      ENDIF
      EGWA(IC,IR)=EACT
      ETOT(IC,IR)=TI*CECT+EP+EACT
      IF(NC.GT.0)THEN
        IF(IDINT(CU(3,NC)).EQ.1) ETOT(IC,IR)=EP+EACT
        TDR(IC,IR)=(TI+TI*CECT)/EF2D(IC,IR)                             !NO EFFICIENCY ASSIGNED IF NC=-1 --> EFF=0 WOULD CAUSE NAN
        IF(IDINT(CU(3,NC)).EQ.1) TDR(IC,IR)=0D0                         !SET ZERO FOR NON-IRRIGATION CROPS/VEGETATION
      ENDIF
C
C9E-----FULL AND PARTIAL TRANSPIRATION FROM THE SATURATED ROOT ZONE
C
C9E1----DEFINE POSTITVE PRESSURE HEADS, AT WHICH UPTAKE IS FULL (PSI1) AND ZERO (PSI0)
C
      IF(IROTFL.EQ.KPER.OR.NC.LT.0.OR.ICCFL.EQ.2.OR.ICCFL.EQ.4)THEN
      TGWSAT1=0.D0
      TGWSAT2=0.D0
      GOTO 123
      ELSEIF(NC.GT.0)THEN
        IF(PSI(2,NC).LE.0.D0.AND.PSI(3,NC).LE.0.D0)THEN
        TGWSAT1=0.D0
        TGWSAT2=0.D0
        GOTO 123
        ELSE
        IF(PSI(2,NC).GT.0.D0) PSI0=PSI(2,NC)
        IF(PSI(3,NC).GT.0.D0) PSI1=PSI(3,NC)
        ENDIF
      ENDIF
C
C9E2----EVALUATE ACTUAL TRANSPIRATION (FULL UPTAKE) FROM THE SATURATED ROOT ZONE (TGWSAT1)
      IF(MIN(GSE,HH)-MAX(GSE-TRZ,HH-PSI1).LT.0D0) THEN
      TGWSAT1=0.D0
      ELSEIF(MIN(GSE,HH).EQ.GSE.AND.MAX(GSE-TRZ,HH-PSI1).EQ.GSE-TRZ)THEN
      TGWSAT1=TPOT *(DRDC*PCTG)
      RHSS=RHSS+TGWSAT1
      ELSEIF(MIN(GSE,HH).EQ.GSE.AND.MAX(GSE-TRZ,HH-PSI1).EQ.HH-PSI1)THEN
      THISAT1=((TPOT/TRZ)*(GSE+PSI1)) *(DRDC*PCTG)
      THDSAT1=(-TPOT/TRZ) *(DRDC*PCTG)
      TGWSAT1=THISAT1+THDSAT1*HH
      RHSS=RHSS+THISAT1
      HCOFF=HCOFF-THDSAT1
      ELSEIF(MIN(GSE,HH).EQ.HH.AND.MAX(GSE-TRZ,HH-PSI1).EQ.GSE-TRZ)THEN
      THISAT1=((TPOT/TRZ)*(TRZ-GSE)) *(DRDC*PCTG)
      THDSAT1=(TPOT/TRZ) *(DRDC*PCTG)
      TGWSAT1=THISAT1+THDSAT1*HH
      RHSS=RHSS+THISAT1
      HCOFF=HCOFF-THDSAT1
      ELSEIF(MIN(GSE,HH).EQ.HH.AND.MAX(GSE-TRZ,HH-PSI1).EQ.HH-PSI1)THEN
      TGWSAT1=((TPOT/TRZ)*PSI1) *(DRDC*PCTG)
      RHSS=RHSS+TGWSAT1
      ENDIF
C
C9E3----EVALUATE ACTUAL TRANSPIRATION (PARTIAL UPTAKE) FROM SATURATED ROOT ZONE (TGWSAT2)
      IF(MIN(HH-PSI1,GSE)-MAX(HH-PSI0,SS).LT.0D0) THEN
      TGWSAT2=0.D0
      ELSEIF(MIN(HH-PSI1,GSE).EQ.HH-PSI1.AND.MAX(HH-PSI0,SS).EQ.HH-PSI0)
     1THEN
      TGWSAT2=((TPOT/TRZ)*((PSI0-PSI1)/2)) *(DRDC*PCTG)
      RHSS=RHSS+TGWSAT2
      ELSEIF(MIN(HH-PSI1,GSE).EQ.HH-PSI1.AND.MAX(HH-PSI0,SS).EQ.SS)THEN
      ALPHA=(1+((PSI0-HH+SS)/(PSI0-PSI1)))/2
      THISAT2=((-TPOT/TRZ)*ALPHA*(SS+PSI1)) *(DRDC*PCTG)
      THDSAT2=((TPOT/TRZ)*ALPHA) *(DRDC*PCTG) 
      TGWSAT2=THISAT2+THDSAT2*HH
      RHSS=RHSS+THISAT2
      HCOFF=HCOFF-THDSAT2
      ELSEIF(MIN(HH-PSI1,GSE).EQ.GSE.AND.MAX(HH-PSI0,SS).EQ.HH-PSI0)THEN
      ALPHA=((GSE-HH+PSI0)/(PSI0-PSI1))/2
      THISAT2=((TPOT/TRZ)*ALPHA*(GSE+PSI0)) *(DRDC*PCTG)
      THDSAT2=((-TPOT/TRZ)*ALPHA) *(DRDC*PCTG)
      TGWSAT2=THISAT2+THDSAT2*HH
      RHSS=RHSS+THISAT2
      HCOFF=HCOFF-THDSAT2
      ELSEIF(MIN(HH-PSI1,GSE).EQ.GSE.AND.MAX(HH-PSI0,SS).EQ.SS)THEN
      THISAT2=
     1((TPOT/(2*(PSI0-PSI1)))*(GSE+SS+2*PSI0)) *(DRDC*PCTG)
      THDSAT2=(-TPOT/(PSI0-PSI1)) *(DRDC*PCTG)
      TGWSAT2=THISAT2+THDSAT2*HH
      RHSS=RHSS+THISAT2
      HCOFF=HCOFF-THDSAT2
      ENDIF
      TTOT(IC,IR)=TTOT(IC,IR)+TGWSAT1+TGWSAT2
      TGWA(IC,IR)=TGWA(IC,IR)+TGWSAT1+TGWSAT2
C     
C
C9F-----CALCULATE NET RECHARGE:
C       UPDATE PRELIMINARY FARM NET RECHARGE (Egw NOT YET INCLUDED) BY EVAPORATION FROM GROUNDWATER
C       AND BY FULL AND PARTIAL TRANSPIRATION FROM SATURATED PORTIONS OF THE ROOT ZONE (E.G. FOR RICE)
123   FNRCH(IC,IR)=FNRCH(IC,IR)-EACT-TGWSAT1-TGWSAT2
      IF(DABS(FNRCH(IC,IR)).LT.FPS) FNRCH(IC,IR)=0.D0
C
C9G-----FOR POSITIVE HEAD COEFFICIENTS:
C       POSITIVE HCOF CAN RENDER MATRIX NON-DIAGONALLY DOMINANT (especially when using PCG)
C       THEREFORE, SUBTRACT POSITIVE HCOF*HEAD FROM BOTH SIDES OF EQUATION
C       (i.e. undo separation of head-dependent sourc/sink term and locate entire term on RHS)        
      if(hcoff.gt.0.D0) then
      rhss=rhss-(hcoff-hcoffold)*hh
      hcoff=hcoffold                                                    !reset to previous hcof
      endif
C
C9H-----REDEFINE HCOF AND RHS AS REALS
C       SPECIAL CASE: FNRCH CALCULATION WAS ALLOWED FOR IBOUND=0 & IL=NLAY, BUT HCOF AND RHS CALCULATION EXCLUDED FOR THIS CASE:
C         IF ANY LAYER IS IBOUND>0 BELOW ANOTHER LAYER WITH IBOUND=0 THEN THAT LAYER IS THE UPPER MOST ACTIVE LAYER RECEIVING FNRCH.
C         HOWEVER, IF ALL LAYERS DOWN TO THE BOTTOM LAYER ARE IBOUND=0, WE ASSUME A HARDROCK FORMATION AND FNRCH CANNOT BE APPLIED.
C         IN THAT CASE, INEFFICIENT LOSSES WILL ALL BE SENT TO RUNOFF.
      IF(IBOUND(IC,IR,IL).GT.0) THEN 
      HCOF(IC,IR,IL)=SNGL(HCOFF)
      RHS(IC,IR,IL)=SNGL(RHSS)
C
C9I-----FIND NO. OF ACCURATE DIGITS OF REAL AND ROUND TO THE LAST ACCURATE DIGIT (helps, but doesn't help too much!)
      IF(DABS(HCOFF-HCOF(IC,IR,IL)).GT.(FPS**3)) THEN 
      DIG1=DLOG10(DABS(HCOFF-DBLE(HCOF(IC,IR,IL)))) 
      IF(DIG1.LT.0D0) DIG2=10D0**(-DINT(DIG1)) 
      IF(DIG1.GT.0D0) DIG2=10D0**(-DINT(DIG1+1.D0)) 
      HCOF(IC,IR,IL)=SNGL(DNINT(HCOFF*DIG2)/DIG2) 
      ENDIF 
      IF(DABS(RHSS-RHS(IC,IR,IL)).GT.FPS) THEN 
      DIG1=DLOG10(DABS(RHSS-DBLE(RHS(IC,IR,IL)))) 
      IF(DIG1.LT.0D0) DIG2=10D0**(-DINT(DIG1)) 
      IF(DIG1.GT.0D0) DIG2=10D0**(-DINT(DIG1+1.D0)) 
      RHS(IC,IR,IL)=SNGL(DNINT(RHSS*DIG2)/DIG2) 
      ENDIF
      ENDIF
C
C9J-----DETERMINE SURFACE-WATER RUNOFF FROM EACH FARM-CELL:
C       (DO THAT FOR ALL 3 CASES: USUAL CASE, AVI > CIR, AVI < CIR)
C   
C9J1----UPDATE TDR ONLY FOR CELLS WITH ORIGINAL CIR OR WITH REDUCED CIR DUE TO REDUCED ACREAGE 
C       (not for ideffl<0, since tdr may be equal to available deficit irrigation supply [NOT demand])
      IF(IDEFFL.GE.0.AND.EF2D(IC,IR).GT.0D0) TDR(IC,IR)=CIR/EF2D(IC,IR)
C
C9J2----COMPUTE INEFFICIENT LOSSES DUE TO IRRIGATION:
      RIRR=TDR(IC,IR)-CIR
      IF(RIRR.LE.0D0) RIRR=0.D0                                         !CAN HAPPEN, IF TDR(or AVI) < CIR
C
C9J3----COMPUTE INEFFICIENT LOSSES DUE TO PRECIPITATION:
      RPREC=P-TP-EP
      IF(RPREC.LE.0D0) RPREC=0.D0
C
C9J4----COMPUTE SURFACE-WATER RUNOFF FOR EACH FARM-CELL:
      DPERC(IC,IR)=RIRR*(1.D0-FIESWI)+RPREC*(1.D0-FIESWP)
      IF(IUNITUZF.GT.0.AND.ICCFL.GE.3) THEN
        IF(IUZFBND(IC,IR).GT.0.AND.IFID(IC,IR).GT.0) THEN
        FINF(IC,IR)=SNGL(DPERC(IC,IR))/(DELR(IC)*DELC(IR))
          IF(FINF(IC,IR).GT.VKS(IC,IR)) THEN
            EXCESPP(IC,IR)=(FINF(IC,IR)-VKS(IC,IR))*(DELR(IC)*DELC(IR))
            FINF(IC,IR)=VKS(IC,IR)
          ELSE
            EXCESPP(IC,IR)=0.
          ENDIF
        ENDIF
      ENDIF
      SWRUNOFF=RIRR*FIESWI+RPREC*FIESWP
      SWRUN(IC,IR)=SWRUNOFF
C 
      ENDIF                                                             !END OF ICID IF-CONDITION
      ENDIF                                                             !END OF IFID IF-CONDITION
C
      GO TO 150
  160 ENDDO                                                             !END OF NLAY LOOP
  150 ENDDO                                                             !END OF NCOL LOOP
      ENDDO                                                             !END OF NROW LOOP
C
C10==== DETERMINE SURFACE-WATER RUNOFF FOR EACH FARM: =======================================================
C       =============================================
C
C10A----SUM UP ALL SWRUNOFF FOR EACH FARM AND ROUT IT TO THE RESPECTIVE FARM-DRAIN
C       (if SFR is not linked to FMP, surface-water runoff is still calculated,
C        but does not impact the GW-budget in any way.)
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                              ! seb SKIP INACTIVE FARMS FOR SP
      SWRUNSUM=0.D0         
         DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
           IR=FMLOC(NF)%RC(1,I)
           IC=FMLOC(NF)%RC(2,I)
           SWRUNSUM=SWRUNSUM+SWRUN(IC,IR)
        ENDDO
        !
C seb ADD CONENCTION OF DRT TO FMP BY SENDING DRAIN RETURN FLOW TO FARM RUN OFF.
        IF ( IUNITDRT.NE.0 .AND. LFID(NF) 
     +      .AND. ANY(DRTFLOW(:)%FID.NE.0) )THEN
          DO I=1, NFARMS
            IF(DRTFLOW(I)%FID.EQ.NF)THEN                                !NOTE DRTFLOW(1:NFARMS)
              SWRUNSUM=SWRUNSUM+DRTFLOW(I)%FLO                          !seb ADD DRTFLOW TO ALLOW ADDED FLOWS FROM DRT
              EXIT
            END IF
            IF(DRTFLOW(I)%FID.EQ.0)EXIT  !NO FARMS WITH A DRT FOUND
          END DO
        END IF
C
C10B----IF FMP IS LINKED TO SFR, AND IF DRAINS IN SFR EXIST -> SEND SURFACE-WATER RUNOFF TO DRAINS:
      IF(IUNITSFR.GT.0) THEN
C
C10B1---PRORATE SWRUNOFF OVER ALL FARM DRAIN REACHES, IF DRAIN IS LOCATED WITHIN FARM
C       OR RETURN CUMULATIVE RUNOFF TO SPECIFIED REACH OR REACH FOUND CLOSEST TO LOWEST ELEVATION OF FARM.
C       (BOTH SPECIFIED AND CLOSEST REACH ARE DEFINED TO BE FARM DRAIN REACHES, I.E.: IFDRID.EQ.NF)
      FDSLEN=FDSEGL(NF)   
        DO L=1,NSTRM
        RCHLEN=DBLE(STRM(1,L))
         IF(IFDRID(NF,L).EQ.NF) THEN    
           IF(FDSEGL(NF).GT.0) THEN
           RRUNOFF=SWRUNSUM*RCHLEN/FDSLEN     
           STRM(12,L)=STRM(12,L)+SNGL(RRUNOFF)                          !drain runoff has be cumulative to allow for multiple farms discharging into the same reach
           ELSEIF(INT(FDSEGL(NF)).EQ.0) THEN                            !seb ADDED INT TO ENSURE PROP
           STRM(12,L)=STRM(12,L)+SNGL(SWRUNSUM)
           ENDIF
C---          
           IF(ILGR.NE.0.AND.LGRITER.GT.1.AND.IGRID.EQ.1.AND.
     1       NGRIDS.GT.1) THEN 
             DO N=2,NGRIDS
               IF(LGRDAT(N)%IFMPGRID.EQ.N.AND.
     1            LGRDAT(N)%ISFRGRID.EQ.0)THEN
                 IF(FMPDAT(N)%ISRRFL.EQ.-1) THEN
                   IF(FMPDAT(N)%IFA(NF).NE.0) THEN
                   SWRUNSUMC=0.D0
                     DO IR=1,GLOBALDAT(N)%NROW
                     DO IC=1,GLOBALDAT(N)%NCOL
                      IF(FMPDAT(N)%IFID(IC,IR).EQ.FMPDAT(N)%IFA(NF))THEN
                        SWRUNSUMC=SWRUNSUMC+FMPDAT(N)%SWRUN(IC,IR)
                      ENDIF
                     ENDDO
                     ENDDO
                     IF(FDSEGL(NF).GT.0) THEN
                     STRM(12,L)=STRM(12,L)+SNGL(SWRUNSUMC*RCHLEN/FDSLEN)!PRORATE ADDITIONAL RETURNFLOW FROM CHILD FARM OF EQUAL ID TO PARENT FARM WITH A DRAIN CONNECTION
                     ELSEIF(FDSEGL(NF).EQ.0) THEN
                     STRM(12,L)=STRM(12,L)+SNGL(SWRUNSUMC)              !RE-USE SWRUNSUM VARIABLE FOR CHILD MODEL FARM                        
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
             ENDDO
           ENDIF
C---        
         ENDIF    
        ENDDO
C
CWSCHMID-07/02/08: CHECK FOR REMOTE REACH OUTSIDE FARM CLOSEST TO LOWEST ELEVATION WAS MOVED TO FMP3RP.
C
      ENDIF                                                             !END OF CONDITION: IF(IUNITSFR.GT.0)  
      ENDIF
      ENDDO 
C
C11==== UPDATE TOTAL FARM DELIVERY REQUIREMENT AND NON-ROUTED DELIVERIES: ====================================
C       =================================================================
C
C11A----UPDATE TOTAL FARM DELIVERY REQUIREMENT (ONLY FOR NON-DEFICIENCY CASES)
      DO NF=1,NFARMS
      IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                              ! seb SKIP INACTIVE FARMS FOR SP
        TF=0D0        
C       DON'T UPDATE FOR ACREAGE-OPTIMIZED FARMS  --> NOT USED: CAUSES FB BUDGET PROBLEMS
c       IF(IDEFFL.GT.0.AND.QREQOLD(NF)-QMAXF(NF).GT.FPS) GOTO 550
        DO I=1,FMLOC(NF)%Count                                            !seb removed double do loop
          IR=FMLOC(NF)%RC(1,I)
          IC=FMLOC(NF)%RC(2,I)
          IF(ICID(IC,IR).GT.0)TF=TF+TDR(IC,IR)
        ENDDO
C
C11B----UPDATE NON-ROUTED DELIVERY
      IF(INRDFL.EQ.0) ND=0D0
      IF(INRDFL.NE.0) THEN
C
         DO IT=1,MXNRDT
            INRDR(IT)=IDINT(UNRD(3*IT,NF))                              !define nrd-ranks as array to find maximum rank for current farm
            INRDU(IT)=IDINT(RNRD(2,IT,NF))                              !define nrd-use flags for a ranked array of nrd-types for current farm
         ENDDO
         ITTOT=MAXVAL(INRDR)                                            !find maximum rank for current farm (could be < max. number of nrd-types MXNRDT)
c
         DSUM=0.0d0
         DO IRT=1,ITTOT
            DSUM=DSUM+RNRD(1,IRT,NF)
            ITU=ITTOT-IRT                                               !check for delivery types unnecessary to meet TFDR (ITU) in sequence of ranking 
            IF(TF-DSUM.LE.0.0D0) GOTO 778
         ENDDO
778      IF(TF.LT.FPS) ITU=ITTOT
c
         ND=0.0D0
         SUFDSUM=0.0D0
         SURPLUS1=0.0D0
         SURPLUS2=0.0D0
         DO IRT=1,ITTOT
            IF(TF.GT.DSUM) THEN
            ND=DSUM                                                     !non-routed delivery can't be greater than the sum of deliveries
            ELSE
               IF(IRT.LT.ITTOT-ITU) THEN
               SUFDSUM=SUFDSUM+RNRD(1,IRT,NF)                           !sum of deliveries that are used in their entirety to satisfy TF
               ENDIF
               IF(IRT.EQ.ITTOT-ITU) THEN
                  IF(INRDU(IRT).EQ.0) THEN
                  ND=TF                                                 !as long as the current Ranked Type IRT exceeds TF, only part of IRT will be used & the total ND is equal to TF
                  ELSEIF(INRDU(IRT).GT.0) THEN
                  ND=SUFDSUM+RNRD(1,IRT,NF)                             !if not only the sufficient but the abolute amount of IRT is to be used ...
                    IF(INRDU(IRT).EQ.1) SURPLUS1=ND-TF                  !gather excess surplus component desired to be recharged into the canal
                    IF(INRDU(IRT).EQ.2) SURPLUS2=ND-TF                  !gather excess surplus component desired to be injected into farm wells
                  ENDIF
               ENDIF
               IF(IRT.GT.ITTOT-ITU.AND.INRDU(IRT).GT.0) THEN
               ND=ND+RNRD(1,IRT,NF)                                     !if IRT tyes are unnecessary, but nontheless specified to be used in their absolute amounts
                 IF(INRDU(IRT).EQ.1) SURPLUS1=SURPLUS1+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be recharged into the canal
                 IF(INRDU(IRT).EQ.2) SURPLUS2=SURPLUS2+RNRD(1,IRT,NF)   !gather unneeded surplus components desired to be injected into farm wells
               ENDIF
            ENDIF
         ENDDO
C
      ENDIF
C
C11C----UPDATE DEMAND AND NON-ROUTED DELIVERIES:
C       (PUMPAGE NOT UPDATED BUT CALCULATED PREVIOUSLY DURING THIS ITERATION IN 7E5A, 7E7D, AND 7E8G;
C        (SEMI-)ROUTED DELIVERIES ARE ADJUSTED IN BD-ROUTINE)
      IF(IALLOTSW.GE.2.AND.
     1((IDEFFL.NE.0.AND.ITER2.EQ.0.AND.QREQOLD(NF)-QMAXF(NF).LT.0.D0)
     2 .OR.IDEFFL.EQ.0)) then                                           !FOR ZERO SCENARIO MAKE SURE THE FINAL FDS BUDGET IS EQUAL TO THE INITIAL BUDGET!
C     FOR PRIOR APROPRIATION:
C     UPDATE FOR PREDEFICIENCY RUN DURING DEFICIENCY SCENARIOS
C     AND FOR ZERO SCENARIO 
      TFDROLD(NF)=TF
      NRD(2,NF)=ND
      endif
      TFDR(NF)=TF
      NRD(1,NF)=ND
C      
550   CONTINUE
      ENDIF
      ENDDO
C
C12==== PRIOR APPROPRIATION: SET FLAG IN CASE A DEFICIENCY EXISTS; CORRECT SMALL BUDGET INACCURACIES =============== 
C
C12A----SET FLAG TO ACTIVATE PRIOR APPROPRIATION ROUTINE IN NEXT ITERATION
C       IN CASE A DEFICIENCY EXISTS AND A CERTAIN DEFICIENCY SCENARIO IS DESIRED.
C
555   if(IALLOTSW.GE.2) then
         IF(IDEFFL.EQ.-2.OR.IDEFFL.GT.0) THEN
C
            IF(MAXVAL(QREQOLD-QMAXF).LE.0.D0) THEN
              ITER2=0
              N=1
              CALL PRIORCOR(TFDR,N,QREQ)
            ENDIF
            IF(MAXVAL(QREQ-QMAXF).GT.0.D0.AND.ITER2.EQ.0) THEN
              ITER2=2
C
C12B----CALL CORRECTION ROUTINES FOR PRIOR APPROPRIATION CORRECTING MINOR CHANGES IN BUDGET
C       Once a the delivery to the farm (and the accordingly nessecary diversion rate into the respective canal
c       has been determinedl, then the flow rates for such farms are excempted from further iterations:
c       However, this can occasionally lead small residual inaccuracies when a solution is found!
c       Objectives of the following correction routine:
C       - If senior farm on upstream canal experiences QREQ, then junior farm's RDEL on downstream canal should be 0!
C       - If junior farm on upstream canal experiences QREQ, then senior farm's QREQ on downstream canal should be 0!
c
c           for acreage-optimization,the corrected SW-Delivery needs to enter optimization as supply constraint.
c           (e.g., a junior farm's SW-del. was reduced from 5 to 0, and
c            upstream senior farm got that credited back ... say from 1000 to 1005, then: sw-suppply constrains 
c            from downstream junior farm and upstream senior farm are 0 and 1005 respectively for optimization!)
              IF(IDEFFL.GT.0.AND.IALLOTSW.NE.2) THEN
                N=2
                ISTARTFL=0
                CALL PRIORCOR(TFDROLD,N,QREQOLD)
                DO NF=1,NFARMS
                IF(IFA(NF).NE.0) THEN
                QREQ(NF)=QREQOLD(NF)
                ENDIF
                ENDDO
              ENDIF
            ENDIF
c
C           for water-stacking, the correction pre-scenario budget can be done
c           after all all farms are evaluated both for pre- and post-scenario.
c           (input terms into scenario do not have to be the corrected ones).
            if(ideffl.eq.-2.AND.IALLOTSW.NE.2) then  
              n=2
              CALL PRIORCOR(TFDROLD,N,QREQOLD)
            endif
         ELSEIF(IDEFFL.EQ.0) THEN
           N=2
           CALL PRIORCOR(TFDROLD,N,QREQOLD)
           N=1
           CALL PRIORCOR(TFDR,N,QREQ)
         ELSEIF(IALLOTSW.NE.2) THEN
           n=1
           CALL PRIORCOR(TFDR,N,QREQ)
         ENDIF
      ENDIF
C
C13==== ADD FARM WELL DISCHARGE TO RHS: =============================================================================
C       ===============================
C
C13A----IF NUMBER OF FARM WELLS <= 0 THEN SKIP FARM WELL DISCHARGE CALCULATION.
      IF(NFWELS.LE.0) GOTO 300
C
C13B----NOCIRNOQ OPTION: SET QMAX TO ZERO FOR WELLS WHOSE CELLS HAVE A ZERO CROP IRRIGATION REQUIREMENT
      IF(FWLAUX(2).EQ."NOCIRNOQ") THEN                                  ! seb USE STATIC ASSINGMENT RATHER THEN LOOP
       DO NF=1,NFARMS
        IF(IFA(NF).NE.0 .AND. LFID(NF)) THEN                            ! seb SKIP INACTIVE FARMS FOR SP
         QSUMF=0.D0
         DO L=1,NFWELS
          FID=IDINT(FWELL(5,L))
          IF(FID.EQ.IFA(NF))THEN
            IR=IDINT(FWELL(2,L))
            IC=IDINT(FWELL(3,L))
            IL=IDINT(FWELL(1,L))
            IF(AUXV(2,L).EQ.1 .AND. TDR(IC,IR).LT.FPS) FWELL(6,L)=0.D0
C           FOR WELLS IN CELLS WITH TDR>0 USED TOGETHER WITH WELLS IN CELLS WITH TDR=0 TO RECOMPUTE QMAXF, MAKE SURE QMAX=0 FOR IBOUND=0               
            IF(IL.GT.0) THEN
              IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
            ENDIF
            QSUMF=QSUMF+FWELL(6,L)
          ENDIF
         ENDDO
         QMAXF(NF)=QSUMF
        ENDIF
       ENDDO
C
C seb ADDED TO FIX POTENTIAL ISSUE WITH ALLOTMENT and "NOCIRNOQ"
        IF(IALLOTGW.GT.0)THEN
!         WHERE(LFID .AND. QMAXF.GT.ALLOTGW(2,:))                        !ASSIGN VALUE WHERE LFID IS TRUE
!           QMAXF=ALLOTGW(2,:)                                           !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!         END WHERE
!         WHERE(.NOT. LFID)                                    !ASSIGN VALUE WHERE LFID IS TRUE
!           QMAXF=0d0                                          !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
!         END WHERE    
!        END IF                                                           ! End of block for IALLOTGW
        DO NF=1,NFARMS
         IF(LFID(NF))THEN
             IF(QMAXF(NF)>ALLOTGW(2,NF)) QMAXF(NF)=ALLOTGW(2,NF)         !RESET QMAXF for each Farm equal to the Groundwater Allotment either for entire simulation or at each stress period
         ELSE
             QMAXF(NF)=0D0
         ENDIF
        ENDDO
       END IF
      ENDIF
C
C13C----PROCESS EACH WELL IN THE FARM-WELL LIST.
      IERR=0                                                            !seb added flag for bad well link
      DO 40 L=1,NFWELS
      IL =IDINT(FWELL(1,L))
      IR =IDINT(FWELL(2,L))
      IC =IDINT(FWELL(3,L))
      WID=IDINT(FWELL(4,L))
      FID=IDINT(FWELL(5,L))
      IF (.NOT. LFID(FID)) CYCLE                                        !SKIP FARM WELLS ASSOCIATED WITH FARMS THAT ARE INACTIVE
      if(IUNITMNW1.gt.0 .AND. WID.LT.0)THEN                             ! SCOTT THIS SEEMS TO DO A LOT OF REDUNDANT CHECKS FROM WHAT WAS DONE AT BEGINING OF FM ROUTINE
      ILOLD=IL
      if(WID.LT.0) THEN                                                 !SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth  --CHANGED IDINT(FWELL(4,L)) to ID
        ISNGLWELL=0
        do n=1,nwell2
          if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then
            if(n.eq.1) then
              m=IDINT(well2(1,n))
              j = int(mod((m-1),ncol*nrow)/ncol) + 1
              i = mod((m-1),ncol) + 1
              if(ir.eq.j.and.ic.eq.i) then
              ISNGLWELL=ISNGLWELL+1
              IL=int((m-1)/(ncol*nrow))+1
              endif           
            endif
            if(n.gt.1)then
              if(well2(8,n-1).lt.1D30)then
                m=IDINT(well2(1,n))
                j = int(mod((m-1),ncol*nrow)/ncol) + 1
                i = mod((m-1),ncol) + 1
                if(ir.eq.j.and.ic.eq.i) then
                ISNGLWELL=ISNGLWELL+1
                IL=int((m-1)/(ncol*nrow))+1
                endif
              endif 
            endif
          endif
          if(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
            m=IDINT(well2(1,n))
            j = int(mod((m-1),ncol*nrow)/ncol) + 1
            i = mod((m-1),ncol) + 1
            if(ir.eq.j.and.ic.eq.i) then
            IL=int((m-1)/(ncol*nrow))+1
            endif
          endif
        enddo
        IF(ISNGLWELL.GT.1) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,112) ISNGLWELL,IR,IC,WID                          !CHANGED IDINT(FWELL(4,L)) to WID seb
          ENDIF
          STOP
        ELSEIF(ISNGLWELL.EQ.1) THEN
          FWELL(1,L)=IL
        ENDIF
      ENDIF
      ENDIF
C     MAKE SURE QMAX=0 FOR IBOUND=0 (ACTUALLY DOUBLE FAIL-SAFE .. SHOULD BE BYPASSED IN C13C1 ANYWAY)        
      IF(IL.GT.0) THEN
        IF(IBOUND(IC,IR,IL).EQ.0) FWELL(6,L)=0D0
      ENDIF
      QMAX=FWELL(6,L)
C
      IF(IL.LE.0.OR.IL.GT.NLAY) THEN                                    !seb ADDED ERROR CHECK FOR BAD MNW1-FMP Linked wells
       IF(LSTCHK(1))WRITE(IOUT,'(/A,/A,I7,A,I7,A,2I7,2A,/2A,I7)') 
     +  '***FMP-MNW1 Link Error***','Farm ',FID,' with Farm Well ',WID,
     +  ' at Row, Col ',IR,IC,' has Well ID < 0 to indicate',
     +  ' MNW1 link, but no MNW1 well is found at that location',
     +  'CHECK INPUT DATA SET FOR CORRECT LINK',
     +  'Debug Info, Code found Layer as ', IL
        IERR=1
        GO TO 40
      END IF
C13C1---IF THE CELL IS INACTIVE THEN BYPASS PROCESSING, OR ELSE SUBTRACT Q FROM RHS
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 40
C
C13C2---DETERMINE PUMPING RATE FOR EACH WELL
C
C13C2A--CHECK FOR CAPACITY CRITERION OF EACH FARM:
C       QMAXF < QREQ THEN JUST PUMP QMAX AND REDUCE ACREAGE SO THAT QMAXF=QREQ
      IF (QMAXF(FID).LT.QREQ(FID)) THEN
      Q=-QMAX
      ELSE
C
C13C2B---IF QMAXF > QREQ: THEN
C        --> PULL THE VALUES OF FARM-WELL DEFICIENCY AND EXCESS COMPARED TO Q-AVERAGE
C        --> CHECK FOR CAPACITY CRITERION OF INDIVIDUAL WELLS:
C            PUMP WELLS ON AVERAGE WITH QAVF:
C              WELLS WITH LESS THAN QAVF: PUMP QMAX
C              WELLS WITH MORE THAN QAVF: PUMP QAVF +
C              SHARE WHICH ACCOUNTS FOR THE DEFICIENCIES OF 'WELLS WITH LESS THAN QAVF'
         IF (QMAX.LE.QAVF(FID)) THEN 
           Q=-QMAX
         ELSE
           Q=-(QAVF(FID)+(QDEF(FID)*(QMAX-QAVF(FID))/QEXC(FID)))
         ENDIF
      ENDIF
C
C13C3---FOR WELLS SIMULATED WITH THE MNW1-PACKAGE:
C       SEND PUMPING RATE AS "DESIRED PUMPING-RATE" TO MNW-PACKAGE
      IF(IUNITMNW1.GT.0.AND.WID.LT.0) THEN
      NOD=(IL-1)*NROW*NCOL + (IR-1)*NCOL + IC
      N=0
      QACT=0.D0
        DO WHILE (N.LT.NWELL2)
        N=N+1
        IF(IDINT(WELL2(1,N)).EQ.NOD) THEN
C       CHECK IF TOP NODE OF A WELL BELONGS TO A MULTI-NODE WELL (SAME CRITERION AS IN MNW)
C       (IF SO, THEN WELL2(7,N) TELLS, WHICH NODE IS THE BOTTOM NODE OF THE WELL)
          IF(ILOLD.EQ.0.AND.WELL2(8,N).GT.1.0D30) N=IDINT(WELL2(7,N)) 
          WELL2(2,N)=Q
          WELL2(15,N)=Q
C         RESET QACT=WELL2(3,N) FROM PREVIOUS STRESS PERIOD OR TIME STEP,
C         NO MATTER IF SIMULATED QMAX IS RESET OR NOT.
          IF(ISTARTFL.EQ.1) THEN
          WELL2(3,N)=0.D0
          WELL2(17,N)=0.D0
          ENDIF
C         ANTICIPATE QACT CALCULATION IN MNW IN ORDER TO AVOID A BUDGET DISCREPANCY
C         BETWEEN FMP AND MNW IN A LAST ITERATION
          IF(ILOLD.NE.0) QACT=(WELL2(7,N)-HNEW(IC,IR,IL))*WELL2(11,N)   ! SCOTT THIS MAY NEED TO BE REWORKED TO BREAK DEPENDENCE ON LAY=0 FLAG
        ENDIF
        ENDDO
      IF(ILOLD.NE.0.AND.QACT.GT.Q.AND.-Q.EQ.FWELL(6,L).AND.             !rtc broke if into multiple parts ! SCOTT THIS MAY NEED TO BE REWORKED TO BREAK DEPENDENCE ON LAY=0 FLAG
     1   N.GT.0)then
        IF(WELL2(11,N).NE.-1D0) THEN
        FWELL(6,L)=-QACT
        QMAXF(FID)=QMAXF(FID)-(QACT-Q)
        Q=QACT
        endif
      ENDIF
        FWELL(NFWLVL,L)=Q
C
C---FOR WELLS SIMULATED WITH THE MNW2
C       SEND PUMPING RATE AS "DESIRED PUMPING-RATE" TO MNW2
      ELSEIF(IUNITMNW2.GT.0.AND.WID.LT.0) THEN
        J=MNW2LOC(L)
        MNW2(5,J)=Q                                                     !SET QDES TO Q IN MNW2 NOTE THAT MNW2 DOES NOT REALLY MAKE USE OF QDEV IN BETWEEN STRESS PERIODS
        FIRSTNODE=IDINT(MNW2(4,J))
        LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )             !J=MNW2LOC(L) TRANSLATES FARM WELL LOCATION TO MNW2 WELL LOCATION        
        MNW2(18,J)=Q                                                    !SET QACT TO Q SO MNW2 CAN ADJUST IT
        MNWNOD(4,FIRSTNODE:LASTNODE)=0D0
        MNWNOD(4,FIRSTNODE)=Q                                            !SET LAST NODE TO Q SO MNW2 CAN SPREAD PUMPING ACROSS THE REMAINDER OF NODES
        !IF(ABS(FWELL(NFWLVL,L)-Q).GT.FPS.OR.ISTARTFL.EQ.1)THEN         !ENFORCE AN UPDATE WHEN QRATE CHANGES
        !  WRITE(*,*)'UPDATING VALUES'
        !  MNW2(18,J)=Q                                                    !SET QACT TO Q SO MNW2 CAN ADJUST IT
        !  MNWNOD(4,FIRSTNODE:LASTNODE)=0D0
        !  MNWNOD(4,LASTNODE)=Q                                            !SET LAST NODE TO Q SO MNW2 CAN SPREAD PUMPING ACROSS THE REMAINDER OF NODES
        !END IF
C        ANTICIPATE QACT CALCULATION IN MNW IN ORDER TO AVOID A BUDGET DISCREPANCY
C        BETWEEN FMP AND MNW IN A LAST ITERATION
         !IF(ILOLD.NE.0) QACT=(WELL2(7,N)-HNEW(IC,IR,IL))*WELL2(11,N)    ! SCOTT WHY IS THIS NECESSARY FOR SINGLE NODE WELLS
        QMNW2DES=MNW2(5,J)                                     
        QMNW2ACT=SUM(MNWNOD(4,FIRSTNODE:LASTNODE))                      !SUM ALL RATES FROM EACH NODE TO GET ACTUAL PUMPING RATE
        !IF(QMNW2ACT.GT.Q.AND.ABS(Q+FWELL(6,L)<FPS)) THEN                !ABS(Q+FWELL(6,L))<FPS CHECKS TO SEE IF Q=FWELL(6,L)
        ! QMAXF(FID)=QMAXF(FID)-(QMNW2ACT-Q)
        ! FWELL(6,L)=-QMNW2ACT                                           !NEW MAXIMUM RATE FOR FARM WELL
        ! Q=QMNW2ACT                                                     !NEW FLOW RATE FROM FARM WELL
        !ENDIF
        FWELL(NFWLVL,L)=Q
C
ccrth
C2B-----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
      ELSEIF(Iunitnwt.GT.0.and.IDINT(FWELL(4,L)).GT.0)THEN              !currently only allow NWT-smoothing for deficit irrigation or water
       FWELL(NFWLVL,L)=Q
ccrth      ELSEIF(IDINT(FWELL(4,L)).GT.0
ccrth     1    .AND.Iunitnwt.NE.0.and.IDEFFL.lt.0)THEN                  !currently only allow NWT-smoothing for deficit irrigation or water stacking options
          QSAVE(L)=FWELL(NFWLVL,L)
          Hd = HNEW(ic,ir,il)
          bbot = Botm(IC, IR, Lbotm(IL))
          ttop = Botm(IC, IR, Lbotm(IL)-1)
          SatDif=Hd-bbot
ccrth          Qp=ZERO
        IF (LAYTYPUPW(il).GT.0) THEN
ccrth        IF (LAYTYPUPW(il).GT.0.and.(Hd-bbot).lt.0.1) THEN
          Qp = Q*SMOOTH3(Hd,Ttop,Bbot,dQp,2)
          RHS(IC,IR,IL)=RHS(IC,IR,IL)-Qp
          QXTF(FID) = QXTF(FID) + (QSAVE(L)-Qp)                         !Accumulate the reduced pumpage for all single-aquifer farm wells in each farm if using NWT with smoothing rth
ccrsn Derivative for RHS
          ij = Icell(IC,IR,IL)
          A(IA(ij)) = SNGL(DBLE(A(IA(ij))) + dQp)
      ELSEIF(LAYTYPUPW(IL).LE.0)THEN
          RHS(IC,IR,IL)=SNGL(DBLE(RHS(IC,IR,IL))-Q)
      ENDIF
       if(Qp.ne.ZERO)then
         FWELL(NFWLVL,L)=Qp
         Q=Qp
       endif
      ELSEIF(IDINT(FWELL(4,L)).GT.0.and.Iunitnwt.EQ.0)THEN 
C13C4---FOR WELLS SIMULATED WITH THE FARM PROCESS:
C       SEND FLOW RATE TO RHS OF FINITE DIFFERENCE EQUATION
      RHS(IC,IR,IL)=SNGL(DBLE(RHS(IC,IR,IL))-Q)
      FWELL(NFWLVL,L)=Q
      ENDIF
ccrth
C
   40 CONTINUE
      IF(IERR.NE.0) THEN
      CALL USTOP('***FMP-MNW1 Link Error***'//
     +           ' SEE LST FILE FOR TRANSCRIPT OF ERRORS')
      END IF
C
  300 IF(ILGR.NE.0) THEN
      IF(LGRDAT(1)%ISFRGRID.GT.0.AND.ISFRGRID.EQ.0) IUNITSFR=0
      ENDIF
C      
C14==== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
      SUBROUTINE FMP3QCNVG(IUNITMNW1,IUNITMNW2,IUNITPCG,
     1                    IUNITSIP,IUNITDE4,IUNITGMG,IUNITNWT,IUNITPCGN)
C-----VERSION 1 09/21/09 FMP3QCNVG
C     ******************************************************************
C     ADJUST CLOSURE CRITERIA TO ALLOW CONVERGENCE OF MNW-PUMPING TO 
C     FMP-PUMPING REQUIREMENT (EXCLUDE MNW-WELLS NOT LINKED TO FMP)
C     (still called within BD loop as net MNW rates are not yet computed
C      within FM-routine - will be called within FM loop in next version)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,    ONLY:NFWELS,FWELL,QCLOSE,HPCT,RPCT,LFID,
     +                       MNW2LOC,MNW2NAM 
      USE FMPBLK,       ONLY:FPS,ZERO
      USE GWFMNW1MODULE, ONLY:NWELL2,WELL2
      USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD                               !seb MNW2 LINK DATA
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,IOUT,HNEW
      USE SIPMODULE,    ONLY:HCLOSE
      USE PCGMODULE,    ONLY:HCLOSEPCG,RCLOSEPCG
      USE GMGMODULE,    ONLY:HCLOSEGMG,RCLOSEGMG
      USE DE4MODULE,    ONLY:HCLOSEDE4
      USE PCGN,        ONLY: HCLOSEPCGN
      USE GWFNWTMODULE, ONLY:Tol,Ftol             !rth added for head closure criteria of NWT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:      
C     ------------------------------------------------------------------
      INTEGER IUNITMNW1,IUNITPCG,IUNITDE4,IUNITGMG,IUNITNWT,
     1 IUNITSIP,IUNITMNW2,IUNITPCGN
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      INTEGER N,L,NOD,IR,IC,IL,M,J,I,WID,FID,FIRSTNODE,LASTNODE         !FORMERLY IMPLICIT INTEGER
      DOUBLE PRECISION HCL,RCL !, QACT                                  !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION:: Qdes,Qact
C     ------------------------------------------------------------------
C
      IF(NFWELS.EQ.0) RETURN                                            !seb NO WELLS TO PROCESS
C1======DEFINE HCLOSE AND RCLOSE OF VARIOUS SOLVERS
      IF(IUNITMNW1.GT.0.OR.IUNITMNW2.GT.0) THEN
        IF(IUNITPCG.GT.0) THEN
          HCL=DBLE(HCLOSEPCG)
          RCL=DBLE(RCLOSEPCG)
        ELSEIF(IUNITGMG.GT.0) THEN
          HCL=DBLE(HCLOSEGMG)
          RCL=DBLE(RCLOSEGMG)
        ELSEIF(IUNITDE4.GT.0) THEN
          HCL=DBLE(HCLOSEDE4)
          RCL=ZERO
        ELSEIF(IUNITSIP.GT.0) THEN
          HCL=DBLE(HCLOSE)
          RCL=ZERO
ccrth   
        ELSEIF(IUNITNWT.GT.0) THEN
          HCL=Tol
          RCL=Ftol
ccrth   
Cseb 
        ELSEIF(IUNITPCGN.NE.0) THEN
          HCL=HCLOSEPCGN
          RCL=ZERO
        ENDIF
      END IF
C
C2======LOOP THROUGH ALL MNW1-WELLS LINKED TO FMP
      IF(IUNITMNW1.GT.0) THEN
      N=0
        DO WHILE (N.LT.NWELL2)
        N=N+1
          DO L=1,NFWELS
          IF(IDINT(FWELL(4,L)).LT.0) THEN
            NOD=0
            IR=IDINT(FWELL(2,L))
            IC=IDINT(FWELL(3,L))
            IL=IDINT(FWELL(1,L))        
C
C2A------DEFINE LAYER NUMBER OF SINGLE-LAYER MNW-WELLS THAT HAVE BEEN GIVEN A ZERO LAYER NUMBER
            if(IDINT(FWELL(4,L)).LT.0) then                             !SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth
              if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then
                if(n.eq.1)then
                  m = IDINT(well2(1,n))
                  j = int(mod((m-1),ncol*nrow)/ncol) + 1
                  i = mod((m-1),ncol) + 1
                  if(ir.eq.j.and.ic.eq.i) then
                    IL=int((m-1)/(ncol*nrow))+1
                  endif
                endif
                if(n.gt.1)then
                  if(well2(8,n-1).lt.1D30)then
                    m = IDINT(well2(1,n))
                    j = int(mod((m-1),ncol*nrow)/ncol) + 1
                    i = mod((m-1),ncol) + 1
                    if(ir.eq.j.and.ic.eq.i) then
                      IL=int((m-1)/(ncol*nrow))+1
                    endif
                  endif
                endif
              endif
            endif
C
C2B------FIND MNW-NODES WHERE ACTUAL PUMPING (Well2(3,n)) DID NOT CONVERGE 
C        TO DESIRED PUMPING (Well2(2,n) BEYOND A DIMENSONLESS TOLERANCE
C
C--------ACTUAL Q(k) OF SOLUTION (NOT USED AT THIS POINT, BUT KEPT FOR FUTURE)
C        (OPTIONALLY ALSO Q(k-1)& Q(k) COULD BE CHECKED FOR CONVERGENCE)
C           m = IDINT(well2(1,n))
C           k = int((m-1)/(ncol*nrow))+1
C           j = int(mod((m-1),ncol*nrow)/ncol) + 1
C           i = mod((m-1),ncol) + 1
C          QACT=(WELL2(10,N)-HNEW(i,j,k))*WELL2(11,N)
C
C2B1-----IF QCLOSE IS NOT MET REDUCE CURRENT HCLOSE OR RCLOSE BY A SPECIFIED FRACTION
C        AND CONTINUE SOLVER ITERATION LOOP
            IF(IDINT(FWELL(4,L)).LT.0) THEN                             !SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth
              NOD=(IR-1)*NCOL + IC
              DO M=1,NLAY
                IF(IDINT(WELL2(1,N))-(M-1)*NROW*NCOL.EQ.NOD) THEN
                  IF(WELL2(8,N).GT.1.0D30) THEN
                    N=IDINT(WELL2(7,N))
                  ENDIF
                IF(DABS(WELL2(3,N)).GT.FPS.AND.DABS(WELL2(2,N)).GT.FPS.
     1             AND. DABS(WELL2(2,N)/WELL2(3,N)-1D0).GT.QCLOSE) THEN
                    HCL=HCL*(1.D0-HPCT)
                    RCL=RCL*(1.D0-RPCT)
                    IF(LSTCHK(3)) THEN
                      WRITE(IOUT,5) HPCT*100,RPCT*100, 
     1              HCL,RCL,WELL2(3,N),WELL2(2,N),M,IR,IC
                    ENDIF
5                   FORMAT(1X,/,'HCLOSE & RCLOSE LOWERED BY ',F4.1,
     1              ' % & ',F4.1,' % TO ',1PG17.3,' AND ',1PG17.3,/,1X,
     2              'FOR MNW-PUMPING OF',F15.7,' TO CONVERGE TO FMP ',
     3              'PUMPING REQUIREMENT OF ',F15.7,/,1X,'IN LAYER',
     4              I5,', ROW',I5,', COLUMN',I5,'.',/,
     5              1X,'SOLVER CONTINUES TO ITERATE WITH ADJUSTED ',
     6              'HCLOSE AND RCLOSE',/)
c                    ICNVG=0 !only for future version of MNW with net rates computed in FM loop
                    GOTO 1000
                  ENDIF                 
                ENDIF
              ENDDO
            ELSEIF(IL.GT.0) THEN
              NOD=(IL-1)*NROW*NCOL + (IR-1)*NCOL + IC
              IF(IDINT(WELL2(1,N)).EQ.NOD)THEN
                 IF(DABS(WELL2(3,N)).GT.FPS.AND.DABS(WELL2(2,N)).GT.FPS.
     1             AND. DABS(WELL2(2,N)/WELL2(3,N)-1D0).GT.QCLOSE) THEN
                   HCL=HCL*(1.D0-HPCT)
                   RCL=RCL*(1.D0-RPCT)
                   IF(LSTCHK(3)) THEN
                     WRITE(IOUT,5) HPCT*100.D0,RPCT*100.D0, 
     1             HCL,RCL,WELL2(3,N),WELL2(2,N),IL,IR,IC
                   ENDIF
C                   ICNVG=0 !only for future version of MNW with net rates computed in FM loop
                   GOTO 1000
                 ENDIF   
              ENDIF
            ENDIF
          ENDIF      
          ENDDO     
C       
        ENDDO       
      ENDIF
C
C3======LOOP THROUGH ALL MNW2-WELLS LINKED TO FMP
      IF(IUNITMNW2.GT.0) THEN
       DO L=1,NFWELS
        FID=IDINT(FWELL(5,L))
        WID=IDINT(FWELL(4,L))
        IF( LFID(FID) .AND. WID.LT.0 ) THEN                            !ONLY PROCESS MNW2 LINKED WELLS THAT ARE ASSOCIATED WITH ACTIVE FARMS.
         J=MNW2LOC(L)
         Qdes=MNW2(5,J)
         FIRSTNODE=IDINT(MNW2(4,J))
         LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )
         Qact=SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
         IF(DABS(Qdes).GT.FPS.AND.DABS(Qact).GT.FPS 
     +                       .AND.DABS(Qdes/Qact-1D0).GT.QCLOSE) THEN
           HCL=HCL*(1.D0-HPCT)
           RCL=RCL*(1.D0-RPCT)
           IF(LSTCHK(3)) THEN
             IR=IDINT(FWELL(2,L))
             IC=IDINT(FWELL(3,L))
             WRITE(IOUT,10) HPCT*100D0,RPCT*100D0, 
     +                      HCL,RCL,Qact,Qdes,WID,TRIM(MNW2NAM(L)),
     +                      IR,IC
             WRITE(*,10) HPCT*100D0,RPCT*100D0, 
     +                      HCL,RCL,Qact,Qdes,WID,TRIM(MNW2NAM(L)),
     +                      IR,IC
           ENDIF
10         FORMAT(1X,/,'HCLOSE & RCLOSE LOWERED BY ',F4.1,
     +     ' % & ',F4.1,' % TO ',1PG17.3,' AND ',1PG17.3,/,1X,
     +     'FOR MNW2-PUMPING OF',F15.7,' TO CONVERGE TO FMP ',
     +     'PUMPING REQUIREMENT OF ',F15.7,/,1X,' FOR FMP WELL',
     +     I5,' LINKED TO MNW2 WELL',A,' LOCATED IN ROW ',I5', COL ',
     +     I5,'.',/,1X,'SOLVER CONTINUES TO ITERATE WITH ADJUSTED ',
     +     'HCLOSE AND RCLOSE',/)
c           ICNVG=0 !only for future version of MNW with net rates computed in FM loop
           GOTO 1000
         ENDIF 
        END IF
       ENDDO
      ENDIF
C
1000  IF(IUNITMNW1.GT.0.OR.IUNITMNW2.GT.0)THEN
        IF(IUNITPCG.GT.0) THEN
          HCLOSEPCG=SNGL(HCL)
          RCLOSEPCG=SNGL(RCL)
        ELSEIF(IUNITGMG.GT.0) THEN
          HCLOSEGMG=SNGL(HCL)
          RCLOSEGMG=SNGL(RCL)
        ELSEIF(IUNITDE4.GT.0) THEN
          HCLOSEDE4=SNGL(HCL)
        ELSEIF(IUNITSIP.GT.0) THEN
          HCLOSE=SNGL(HCL)
        ELSEIF(IUNITNWT.GT.0) THEN    !Added NWT linkage to FMP rth
          Tol=SNGL(HCL)
          Ftol=SNGL(RCL)
        ENDIF 
      END IF
C
C2==== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3WELBD(KSTP,KPER,IUNITMNW1,IUNITMNW2,     !Added IunitNWT for Farm wells simulated in combination with NWT by rth
     1    IUNITSIP,IUNITDE4,IUNITUZF,IUNITNWT,IGRID)
C-----VERSION 2 09/18/2009 FMP3WELBD
C     ******************************************************************
C     CALCUALTE AND PRINT FARM DEMAND AND SUPPLY BUDGET
C     CALCULATE AND PRINT COMPACT OR DETAILED FARM BUDGET
C     CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM WELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,    ONLY:EFF,TFDR,TFDROLD,NRD,QREQ,QREQOLD,QMAXF,
     1    NFARMS,NFWELS,MXFWEL,FWELL,IDEFFL,IFWLCB,ISDPFL,NFWLVL,IFWLAL,
     2    IROTFL,IEBFL,REDPCT,GWREDPCT,SWREDPCT,NRREDPCT,IOPFL,NCROPS,
     3    IFBPFL,VFB,IPFWEL,IFID,ICID,PFLR,TTOT,ETOT,SWRUN,DPERC,EGWA,
     4    TGWA,FCSEGL,EPPOT,TPPOT,IRDFL,ISRDFL,FWLAUX,ICCFL,IFCRID,IFA,
     5    FMPDAT,PSIRAMPF,IUNITRAMPF,QXTF,QSAVE,SATTHK,IALLOTGW,ALLOTGW,      !Added Variables for reducing pumpage in Farm Wells for NWT by rth  added FMPOUT seb
     6    FMPOUT,MNW2NAM,SFMP3PNT,LFID,MNW2LOC 
      USE FMPBLK
      USE GWFMNW1MODULE, ONLY:NWELL2,WELL2, IWL2CBMNW1=>IWL2CB          !IWL2CBMNW1 is local name of IWL2CB seb
      USE GWFMNW2MODULE, ONLY:MNW2,MNWNOD,  IWL2CBMNW2=>IWL2CB          ! MNW2 LINK VARIABLES seb
      USE GWFUZFMODULE, ONLY:IRUNFLG,IUZFBND,IRUNBND,EXCESPP,SEEPOUT,   !Added IRUNFLG to check for type of runoff accounted for by FMP from UZF
     1    REJ_INF
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,IOUT,IBOUND,HNEW,BUFF,
     1                       ITMUNI,BOTM,LBOTM,LAYHDT                   !Added BOTM and LBOTM for NWT connection with Farm wells by rth
      USE GWFBASMODULE, ONLY:MSUM,ICBCFL,DELT,PERTIM,TOTIM,VBVL,VBNM,
     1                       IAUXSV,IBDOPT
      USE GWFSFRMODULE, ONLY:NSTRM,GWFSFRDAT
ccrth Added additional function interface and module reference needed for NWT connection with Farm wells
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell                        !Added Variables for reducing pumpage in Farm Wells for NWT by rth
      USE CVT2STR, ONLY: NUM2STR
      IMPLICIT NONE
ccrth!External function interface
      INTERFACE 
        FUNCTION SMOOTH3(H,T,B,dQ,ISS)
        DOUBLE PRECISION SMOOTH3
        DOUBLE PRECISION, INTENT(IN) :: H
        DOUBLE PRECISION, INTENT(IN) :: T
        DOUBLE PRECISION, INTENT(IN) :: B
        DOUBLE PRECISION, INTENT(OUT) :: dQ
        INTEGER, INTENT(IN) :: ISS
        END FUNCTION SMOOTH3
      END INTERFACE
ccrth
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER KSTP,KPER,IUNITMNW1,IUNITUZF,IGRID,ISS,
     1  IUNITMNW2,IUNITSIP,IUNITDE4,IUNITNWT, iw1                       !added for NWT connection to Farm Wells by rth
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT,TEXT2,TEXT3,TEXT4
      CHARACTER*24 ANAME(10)
      CHARACTER*14 TIMEUNIT
      CHARACTER*14 TIME
      CHARACTER*8  FBTYPE
      CHARACTER*21 FDSVAL(9)
      CHARACTER*17 CVAL(48)
      CHARACTER*10 fmt_number
      CHARACTER*30 fmt_string
      DATA TEXT  /'      FARM WELLS'/
      DATA TEXT2 /'FARM WELLS      '/
      DATA TEXT3 /'FM DEMAND&SUPPLY'/
      DATA  ANAME(1)/'            QREQ EXCEEDS'/
      DATA  ANAME(2)/'                QMAXF BY'/
      DATA  ANAME(3)/'  ORIGINAL QREQ EXCEEDED'/
      DATA  ANAME(4)/'                QMAXF BY'/
      DATA  ANAME(5)/'  QREQ OF PRIORITY CROPS'/
      DATA  ANAME(6)/'  STILL EXCEEDS QMAXF BY'/
      DATA  ANAME(7)/'  SW-DEMAND FALLS BEHIND'/
      DATA  ANAME(8)/'   ORIGINAL SW-SUPPLY BY'/
      DATA  ANAME(9)/'  GW-DEMAND FALLS BEHIND'/
      DATA ANAME(10)/'                QMAXF BY'/
      INTEGER N,L,NOD,IR,IC,IL,M,J,I,NF,ISWFLAG,IVAL,IOUTFILE,IBD,      !FORMERLY IMPLICIT INTEGER
     1 IBDLBL,NAUX,IMAWFL,NFW
      REAL RIN,ROUT                                                     !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION RATIN,RATOUT,QQ,TF,ND,AVSUPPLY,QR,SWOLD,SWNEW,QF,
     1 DUM1,DUM2,DUM3,TG,EG,TT,ET,P,SR,DP,EXT,TOTIN,TOTOUT,DISC,RD,SRD,
     2 EP,TP,EI,TI,FDS(9),VAL(24),QMAXTOL,NDOUT,SRDOUT,RDOUT,QFOUT,Q             !added local variable Q for NWT connection to Farm Wells by rth
      double precision Qp,Hd,Ttop,Bbot,dQp,SatDif                                !added for NWT connection to Farm Wells by rth 
C     ------------------------------------------------------------------
      INTEGER:: FID, WID, IWL2CB!,IWL2CB1,IWL2CB2
      INTEGER:: FIRSTNODE,LASTNODE 
      DOUBLE PRECISION::Qdes,Qact
C     ------------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
      CALL SGWF2NWT1PNT(Igrid)   !seb lgr
      CALL SGWF2UPW1PNT(Igrid)   !seb lgr
      CALL SGWF2UZF1PNT(Igrid)   !seb lgr
      CALL SGWF2SFR7PNT(Igrid)   !seb lgr
      CALL SGWF2MNW2PNT(IGRID)   !seb lgr
      CALL SGWF2MNW1PNT(Igrid)   !seb lgr
C
C1===== ADJUST CLOSURE CRITERIA TO ALLOW CONVERGENCE OF MNW-PUMPING TO FMP-PUMPING REQUIREMENT
C       (EXCLUDE MNW-WELLS NOT LINKED TO FMP)
C
C PULL CORRECT CELL TO CELL FLAG FROM MNW1/MNW2
      IF(     IUNITMNW1.GT.0) THEN
        IWL2CB=IWL2CBMNW1
      ELSEIF( IUNITMNW2.GT.0) THEN
        IWL2CB=IWL2CBMNW2
      END IF
ccrth Initialize extra vairiables needed for NWT connection to Farm wells
      IF(IUNITNWT.GT.0) THEN
        Qp = 0.0      
        iw1 = 1
      ELSE
        Qp = 1D0
      ENDIF
ccrth
      IF(IUNITMNW1.GT.0) THEN                                           ! SCOTT THIS ENTIRE BRANCH/IF DOES NOT SEEM TO HAVE ANY PARTICULAR PURPOSE
      N=0
        DO WHILE (N.LT.NWELL2)
        N=N+1
          DO L=1,NFWELS
          IF(IDINT(FWELL(4,L)).LT.0) THEN
            NOD=0
            IR=IDINT(FWELL(2,L))
            IC=IDINT(FWELL(3,L))
            IL=IDINT(FWELL(1,L))        
C
            if(IDINT(FWELL(4,L)).LT.0) then                             !SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth
              if(well2(8,n).lt.1D30.and.well2(7,n).lt.1D30) then
                if(n.eq.1)then
                  m=IDINT(well2(1,n))
                  j = int(mod((m-1),ncol*nrow)/ncol) + 1
                  i = mod((m-1),ncol) + 1
                  if(ir.eq.j.and.ic.eq.i) then
                    IL=int((m-1)/(ncol*nrow))+1
                  endif
                endif
                if(n.gt.1)then
                  if(well2(8,n-1).lt.1D30)then
                    m=IDINT(well2(1,n))
                    j = int(mod((m-1),ncol*nrow)/ncol) + 1
                    i = mod((m-1),ncol) + 1
                    if(ir.eq.j.and.ic.eq.i) then
                      IL=int((m-1)/(ncol*nrow))+1
                    endif
                  endif
                endif
              endif
            endif
          ENDIF      
          ENDDO     
C       
        ENDDO 
      ENDIF
C
C
C2===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
      IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
      IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
      IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
      IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
      IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
      IF(TOTIM.GE.TPU.OR.TOTIM.LT.TPL) THEN
         WRITE(TIME,'(1PE14.7)') TOTIM
      ELSE
         WRITE(TIME,'(F14.2)') TOTIM
      END IF
C
C3===== PRINT HEADER FOR FARM DEMAND & SUPPLY LIST ==================================================
C
      IF(KPER.NE.IROTFL) THEN
C
C3A-----PRINT HEADER FOR FARM DEMAND & SUPPLY LIST PRINTED TO LIST-FILE OR TO FILE
C
C3A1----PRINT HEADER TO LIST FILE
      IF((ISDPFL.EQ.-1.AND.ICBCFL.NE.0).OR.ISDPFL.LT.-1) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,9) KPER,KSTP,NFARMS,TIME,TIMEUNIT
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.EQ.-1.OR.IDEFFL.EQ.0)   WRITE(IOUT,10)    ANAME(1)
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.EQ.-1.OR.IDEFFL.EQ.0)   WRITE(IOUT,11)    ANAME(2)
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.EQ.-2)   WRITE(IOUT,10)          ANAME(3),ANAME(5)
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.EQ.-2)   WRITE(IOUT,11)          ANAME(4),ANAME(6)
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.GT.0)    WRITE(IOUT,10) ANAME(3),ANAME(7),ANAME(9)
      ENDIF
      IF(LSTCHK(3)) THEN
      IF(IDEFFL.GT.0)    WRITE(IOUT,11) ANAME(4),ANAME(8),ANAME(10)
      ENDIF
    9 format(1X,/,1X,'FARM DEMAND AND SUPPLY',7X,'PERIOD ',I4,8X,
     1'STEP',I4,4X,I9,' FARMS',7X,'ELAPSED TIME',A14,1X,A14)
   10 format(139X,A24,A24,A24)                      
   11 format(1X,'FARM-ID',8X,'OFE',20X,
     1'TFDR',18X,'NR-SWD',19X,'R-SWD',20X,'QREQ',23X,'Q',
     2A24,A24,A24)    
C
C3A2----PRINT HEADER TO ASCII FILE
C       (TO BUILD TIME SERIES FOR FARMS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM)
      ELSEIF(ISDPFL.EQ.1) THEN
C      OPEN(UNIT=1001, FILE='FDS.OUT', STATUS='UNKNOWN')                  !Opened in Allocate and read as FMPOUT%UNIT(1)  seb
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1)
     1                                 WRITE(FMPOUT%UNIT(1),31) TIMEUNIT !WRITE TO 'FDS.OUT' seb
   31 FORMAT
     1(2X,'PER',2X,'STP',A14,4X,'FID',9X,'OFE',14X,          !seb line used to be; (2X,'PER',2X,'STP',A14,3X,'GRID'4X,'FID',9X 
     2'TFDR-INI',12X,'NR-SWD-INI',13X,'R-SWD-INI',14X,'QREQ-INI',
     314X,'TFDR-FIN',12X,'NR-SWD-FIN',13X,'R-SWD-FIN',14X,
     4'QREQ-FIN',17X,'Q-FIN',2X,'DEF-FLAG')
C
C3A3----PRINT HEADER TO BINARY FILE IF FDS-LIST IS WRITTEN FOR EACH TIME STEP
C       (TO BUILD TIME SERIES FOR FARMS: PER,STP, AND TIME ARE INCLUDED FOR EACH FARM)
      ELSEIF(ISDPFL.GT.1.AND.IBDOPT.NE.2) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1) ISDPFL,IGRID,KSTP,KPER
      ENDIF
    1 FORMAT(1X,'SAVING "FARM DEMAND AND SUPPLY" ON UNIT',I5,
     1'FOR GRID ',I5,' AT TIME STEP',I5,', STRESS PERIOD',I4)
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1) THEN
      WRITE(ISDPFL)  '         PER','         STP',TIMEUNIT,
     1'        GRID','         FID','         OFE',
     2'    TFDR-INI','  NR-SWD-INI','   R-SWD-INI','    QREQ-INI',
     3'    TFDR-FIN','  NR-SWD-FIN','   R-SWD-FIN','    QREQ-FIN',
     4'       Q-FIN','    DEF-FLAG'
      ENDIF
C3A4----PRINT HEADER TO BINARY FILE IF FDS-LIST IS WRITTEN TO COMPACT BUDGET FILE
      ELSEIF(ISDPFL.GT.1.AND.ICBCFL.EQ.2) THEN
      CALL UBDSV2(KSTP,KPER,TEXT3,ISDPFL,NCOL,NROW,NLAY,
     1            NFARMS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C

C4===== PRINT HEADER FOR FARM BUDGET (MASS BALANCE OF INs AND OUTs) ======================================
C
C4A-----PRINT HEADER FOR FARM BUDGET PRINTED TO ASCII OR BINARY FILE
C
C4A1----PRINT HEADER TO ASCII FILE
C       (TO BUILD TIME SERIES FOR FARMS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM)
C
C4A1A   HEADER FOR COMPACT FARM BUDGET
      IF(IFBPFL.EQ.1) THEN
C      OPEN(UNIT=1008, FILE='FB_COMPACT.OUT', STATUS='UNKNOWN')           !FMPOUT%UNIT(7) OPENED AT AR ROUTINE seb
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1) 
     1                                 WRITE(FMPOUT%UNIT(7),41) TIMEUNIT
   41     FORMAT(
     1  '         PER','         STP',A14           ,!'        GRID',
     *  '         FID',
     2  '            Q-p-in','           Q-sw-in','           Q-gw-in',
     3  '          Q-ext-in','          Q-tot-in','          Q-et-out',
     4  '       Q-ineff-out','          Q-sw-out','          Q-gw-out',
     5  '         Q-tot-out','          Q-in-out','  Q-Discrepancy[%]',
     6  '            V-p-in','           V-sw-in','           V-gw-in',
     7  '          V-ext-in','          V-tot-in','          V-et-out',
     8  '       V-ineff-out','          V-sw-out','          V-gw-out',
     9  '         V-tot-out','          V-in-out','  V-Discrepancy[%]')
C
C4A1B   HEADER FOR DETAILED FARM BUDGET
      ELSEIF(IFBPFL.EQ.2) THEN
C      OPEN(UNIT=1008, FILE='FB_DETAILS.OUT', STATUS='UNKNOWN')         !FMPOUT%UNIT(8) OPENED AT AR ROUTINE seb
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1) 
     1                                 WRITE(FMPOUT%UNIT(8),42) TIMEUNIT
   42     FORMAT(
     1  '         PER','         STP',A14           ,!'        GRID',   ! seb removed GRID FROM OUTPUT
     *  '         FID',
     2  '            Q-p-in','          Q-nrd-in','          Q-srd-in',
     3  '           Q-rd-in','        Q-wells-in','          Q-egw-in',
     4  '          Q-tgw-in','          Q-ext-in','          Q-tot-in',
     5  '          Q-ei-out','          Q-ep-out','         Q-egw-out',
     6  '          Q-ti-out','          Q-tp-out','         Q-tgw-out',
     7  '         Q-run-out','          Q-dp-out','         Q-nrd-out',
     8  '         Q-srd-out','          Q-rd-out','       Q-wells-out',
     9  '         Q-tot-out','          Q-in-out','  Q-Discrepancy[%]',
     *  '            V-p-in','          V-nrd-in','          V-srd-in',
     *  '           V-rd-in','        V-wells-in','          V-egw-in',
     *  '          V-tgw-in','          V-ext-in','          V-tot-in',
     *  '          V-ei-out','          V-ep-out','         V-egw-out',
     *  '          V-ti-out','          V-tp-out','         V-tgw-out',
     *  '         V-run-out','          V-dp-out','         V-nrd-out',
     *  '         V-srd-out','          V-rd-out','       V-wells-out',
     *  '         V-tot-out','          V-in-out','  V-Discrepancy[%]')
C
C4A2----PRINT HEADER TO BINARY FILE IF FB-LIST IS WRITTEN FOR EACH TIME STEP
C       (TO BUILD TIME SERIES FOR FARMS: PER,STP, AND TIME ARE INCLUDED FOR EACH FARM)
      ELSEIF(IFBPFL.GT.2.AND.IBDOPT.NE.2) THEN
      FBTYPE="COMPACT "
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) FBTYPE="DETAILED"          ! SCOTT INTERGER DIVISION CAN CAUSE STRANGE EFFECTS DEPENDING ON THE COMPILER
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,43) FBTYPE,IFBPFL,IGRID,KSTP,KPER
      ENDIF
   43 FORMAT(1X,'SAVING "',A8,' FARM BUDGET" ON UNIT',I5,
     1'FOR GRID ',I5,'AT TIME STEP',I5,', STRESS PERIOD',I4)
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1) THEN
C
C4A2A   HEADER FOR DETAILED FARM BUDGET (FOR EVEN UNIT NUMBER)
        IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) THEN
       WRITE(IFBPFL)
     1  '         PER','         STP',TIMEUNIT,      '        GRID',
     *  '         FID',
     2  '      Q-p-in','    Q-nrd-in','    Q-srd-in','     Q-rd-in',
     3  '  Q-wells-in','    Q-egw-in','    Q-tgw-in','    Q-ext-in',
     4  '    Q-tot-in','    Q-ei-out','    Q-ep-out','   Q-egw-out',
     5  '    Q-ti-out','    Q-tp-out','   Q-tgw-out','   Q-run-out',
     6  '    Q-dp-out','   Q-nrd-out','   Q-srd-out','    Q-rd-out',
     7  ' Q-wells-out','   Q-tot-out','    Q-in-out','  Q-Discrep.',
     8  '      V-p-in','    V-nrd-in','    V-srd-in','     V-rd-in',
     9  '  V-wells-in','    V-egw-in','    V-tgw-in','    V-ext-in',
     *  '    V-tot-in','    V-ei-out','    V-ep-out','   V-egw-out',
     *  '    V-ti-out','    V-tp-out','   V-tgw-out','   V-run-out',
     *  '    V-dp-out','   V-nrd-out','   V-srd-out','    V-rd-out',
     *  ' V-wells-out','   V-tot-out','    V-in-out','  V-Discrep.'
C
C4A2B   HEADER FOR COMPACT FARM BUDGET (FOR ODD UNIT NUMBER)
        ELSE
       WRITE(IFBPFL)
     1  '         PER','         STP',TIMEUNIT,      '        GRID',
     *  '         FID',
     2  '      Q-p-in','     Q-sw-in','     Q-gw-in','    Q-ext-in',
     3  '    Q-tot-in','    Q-et-out',' Q-ineff-out','    Q-sw-out',
     4  '    Q-gw-out','   Q-tot-out','    Q-in-out','  Q-Discrep.',
     5  '      V-p-in','     V-sw-in','     V-gw-in','    V-ext-in',
     6  '    V-tot-in','    V-et-out',' V-ineff-out','    V-sw-out',
     7  '    V-gw-out','   V-tot-out','    V-in-out','  V-Discrep.'
        ENDIF
      ENDIF
C
C4A3----PRINT HEADER TO BINARY FILE IF FB-LIST IS WRITTEN TO "MF2K5 COMPACT BUDGET" FILE
      ELSEIF(IFBPFL.GT.2.AND.ICBCFL.EQ.2) THEN
      TEXT4="COMPACT FARM BUD"
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) TEXT4="DETAILED FARM BD" 
      CALL UBDSV2(KSTP,KPER,TEXT4,IFBPFL,NCOL,NROW,NLAY,
     1            NFARMS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C
      DO NF=1,NFARMS
C
      ISWFLAG=0
      IF(IRDFL.NE.0.OR.ISRDFL.GT.0) THEN
      DO L=1,NSTRM
      IF(IFCRID(NF,L).EQ.NF)THEN
         IF(ISRDFL.GT.0.AND.FCSEGL(NF).EQ.0.D0) ISWFLAG=1
         IF(IRDFL.NE.0.AND.FCSEGL(NF).GT.0.D0) ISWFLAG=2
      ENDIF
      ENDDO
      ENDIF
      IF(IGRID.GT.1) THEN
      IF(FMPDAT(1)%IRDFL.NE.0.OR.FMPDAT(1)%ISRDFL.GT.0) THEN
      DO L=1,GWFSFRDAT(1)%NSTRM
      IF(FMPDAT(1)%IFCRID(NF,L).EQ.NF)THEN
         IF(FMPDAT(1)%ISRDFL.GT.0.AND.FMPDAT(1)%FCSEGL(NF).EQ.0.D0)
     1      ISWFLAG=1
         IF(FMPDAT(1)%IRDFL.NE.0.AND.FMPDAT(1)%FCSEGL(NF).GT.0.D0)
     1      ISWFLAG=2
      ENDIF    
      ENDDO
      ENDIF
      ENDIF
C5===== CALCULATE AND PRINT FARM DEMAND & SUPPLY LIST ==================================================
C
C5A-----PRINT SUGGESTION TO LOWER "HCLOSE," IF QREQ OR HEAD-DEPENDENT EFFICIENCY
C       DID NOT CONVERGE AT THE END OF THE CURRENT TIME STEP
C       (info: this needs to be here and not the end of the farms loop: 
c        reason: if later those criteria will be used to stop the program 
c        (rahter then just to inform) then no data should be printed yet)
C
C5A1----QREQ DOES NOT CONVERGE TO QMAXF   
      IF(IDEFFL.GT.0) THEN
      QMAXTOL=QMAXF(NF)*1.005
      IF(IDEFFL.GT.0 .AND. QREQ(NF).GT.QMAXTOL) THEN
      WRITE(*,12) NF
   12 FORMAT('HCLOSE MUST BE SMALLER FOR QREQ OF FARM ',I4,
     1 ' TO CONVERGE TO QMAXTOL')
      ENDIF
      ENDIF
C
C5A2----HEAD-DEPENDENT EFFICIENCY DOES NOT CONVERGE BEYOND A
C       CONVERGENCE-CRITERION OF 1%
      IF(IEBFL.GT.1) THEN
      IF(DABS(EFF(NCROPS+2,NF)-EFF(NCROPS+3,NF)).GT.0.01D0) THEN
      WRITE(*,13) NF
   13 FORMAT('HCLOSE MUST BE SMALLER FOR HEAD-DEP. EFFICIENCY OF FARM ',
     1I4,' TO CONVERGE')
      ENDIF
      ENDIF
C
C5B-----EVALUATE ADDITIONAL INFORMATION FOR DEMAND AND SUPPLY LIST IN LIST-FILE
      IF((ISDPFL.EQ.-1.AND.ICBCFL.NE.0).OR.ISDPFL.LT.-1) THEN
C
C-------INITIALIZE LOCAL VARIABLES
      SWOLD=0.D0
      SWNEW=0.D0
      DUM1=0.D0
      DUM2=0.D0
      DUM3=0.D0
C
C5B1----FOR ZERO SCENARIO
      IF(IDEFFL.EQ.0.AND. QREQ(NF).GT.QMAXF(NF)) THEN
      DUM1=QREQ(NF)-QMAXF(NF)
      DUM2=0D0
      DUM3=0D0
      ENDIF
C
C5B2----FOR DEFICIT-IRRIGATION WITH OR W/O WATER-STACKING
      IF(IDEFFL.LT.0.AND. TFDR(NF).LT.TFDROLD(NF)) THEN
      DUM1=QREQOLD(NF)-QMAXF(NF)
      DUM2=0D0
      DUM3=0D0
        IF(QREQ(NF).GT.QMAXF(NF)) DUM2=QREQ(NF)-QMAXF(NF)
      ENDIF
C
C5B3----FOR ACREAGE-OPTIMIZATION  
      IF(IDEFFL.GT.0 .AND. TFDR(NF).LT.TFDROLD(NF)) THEN
      DUM1=QREQOLD(NF)-QMAXF(NF)
      DUM2=0D0
      DUM3=0D0
c     if new optimized demand is smaller than originally available supply,... 
c     (because 1. available supply not optimally profitable, or
c              2. reduced upstream supply caused downstream reductions in supply and demand)
      TF=TFDR(NF)
      QR=QREQ(NF)
      AVSUPPLY=(TFDROLD(NF)-QREQOLD(NF)+QMAXF(NF))  
        IF(AVSUPPLY-TF.GT.FPS) THEN
        SWOLD=(TFDROLD(NF)-QREQOLD(NF))
        SWNEW=(TFDR(NF)-QR)
        IF (DABS(SWOLD-SWNEW).GT.FPS) DUM2=SWOLD-SWNEW
        IF(DABS(QMAXF(NF)-QR).GT.FPS) DUM3=QMAXF(NF)-QR
        ENDIF
      ENDIF
C
C5C-----PRINT DEMAND AND SUPPLY INTO LIST FILE (AND RESET DUMMY VARIABLES)
      TF=TFDR(NF)
      ND=NRD(1,NF)
      QR=QREQ(NF)
      IF(QR.LT.QMAXF(NF)) QF=QR
      IF(QR.GE.QMAXF(NF)) QF=QMAXF(NF)        
C
C     IF(IDEFFL.LT.0) EFF(NCROPS+2,NF)=TF/(TF-QR+QF)*EFF(NCROPS+2,NF)
C     
      IF(IDEFFL.EQ.-1.OR.IDEFFL.EQ.0) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,21)NF,EFF(NCROPS+2,NF),TF,ND,TF-ND-QR,QR,QF,DUM1
      ENDIF
      ELSEIF(IDEFFL.EQ.-2) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,22)NF,EFF(NCROPS+2,NF),TF,ND,TF-ND-QR,QR,QF,DUM1,DUM2
      ENDIF
      ELSEIF(IDEFFL.GT.0) THEN
      IF(LSTCHK(3)) THEN
        write(IOUT,23)NF,EFF(NCROPS+2,NF),TF,ND,TF-ND-QR,QR,QF,DUM1,
     1   DUM2,DUM3
      ENDIF
      ENDIF
   21 FORMAT(I8,F11.4,5F24.4,1F24.4)
   22 FORMAT(I8,F11.4,5F24.4,2F24.4)
   23 FORMAT(I8,F11.4,5F24.4,3F24.4)
      DUM1=0D0
      DUM2=0D0
      DUM3=0D0
C
C5D-----PRINT DEMAND AND SUPPLY PER FARM TO FILE
      ELSEIF(ISDPFL.GT.0) THEN
      TF=TFDR(NF)
      ND=NRD(1,NF)
      QR=QREQ(NF)
      IF(TFDROLD(NF).EQ.0D0) THEN
      TFDROLD(NF)=TF
      NRD(2,NF)=ND
      QREQOLD(NF)=QR
      ENDIF
      IF(ISWFLAG.GT.0) SWOLD=TFDROLD(NF)-NRD(2,NF)-QREQOLD(NF)
      IF(QR.LT.QMAXF(NF)) QF=QR
      IF(QR.GE.QMAXF(NF)) QF=QMAXF(NF)
C
c     IF(IDEFFL.LT.0) EFF(NCROPS+2,NF)=TF/(TF-QR+QF)*EFF(NCROPS+2,NF)
C
C5D1----PRINT TO ASCII FILE
      IF(ISDPFL.EQ.1) THEN
      FDS=0.D0
      FDS(1)=TFDROLD(NF)
      FDS(2)=NRD(2,NF)
      IF(ISWFLAG.GT.0) FDS(3)=SWOLD
      FDS(4)=QREQOLD(NF)
      FDS(5)=TF
      FDS(6)=ND
      IF(ISWFLAG.GT.0) FDS(7)=TF-ND-QR
      FDS(8)=QR
      FDS(9)=QF
      DO I=1,9
      IF(DABS(FDS(I)).GE.1D10.OR.
     1  (DABS(FDS(I)).LT.1D-1.AND.DABS(FDS(I)).GT.FPS)) THEN
         WRITE(FDSVAL(I),'(1PE21.4)') FDS(I)
      ELSE
         WRITE(FDSVAL(I),'(F21.4)') FDS(I)
      END IF
      ENDDO
!      write(fmt_number,'(i10)') 9
      fmt_string=
     1'(2I5,A14,I7,F12.4,'//NUM2STR(UBOUND(FDS,1))//'(X,A),I10)'    !CHANGED FROM:TRIM(ADJUSTL(fmt_number))
      WRITE(FMPOUT%UNIT(1),FMT=TRIM(fmt_string))                    !WRITE TO 'FDS.OUT' seb
     1KPER,KSTP,TIME,NF,EFF(NCROPS+2,NF),(FDSVAL(I),I=1,9),IDEFFL   !seb LINE USED TO BE: KPER,KSTP,TIME,IGRID
C
C5D2----PRINT TO BINARY FILE
      ELSEIF((ISDPFL.GT.1.AND.IBDOPT.NE.2).OR.
     1       (ISDPFL.GT.1.AND.ICBCFL.EQ.2)) THEN
      write(ISDPFL)
     1 KPER, KSTP, TIME, IGRID, NF, EFF(NCROPS+2,NF),
     2 TFDROLD(NF), NRD(2,NF), SWOLD,    QREQOLD(NF),
     3 TF,          ND,        TF-ND-QR, QR,          QF, IDEFFL
      ENDIF
      ENDIF
C
C5E-----RESET SOME VARIABLES  
      TFDROLD(NF)=0.0D0                                                 !NOTE THAT TFDROLD, NRD(2 & QREQOLD 
      NRD(2,NF)=0.0D0                                                   !ARE ONLY RESET IN NEXT TIMESTEP, IF
      QREQOLD(NF)=0.0D0                                                 !A DEFICIENCY SCENARIO KICKS IN (OTHERWISE THEY REMAIN ZERO) !!!
C

C6===== CALCULATE AND PRINT FARM BUDGET (MASS BALANCE OF INs AND OUTs) ==================================
C
      IF(IFBPFL.GT.0) THEN
C
C6A-----CLEAR RATES
      DO I=1,24
      VAL(I)=0D0
      ENDDO
      P=0.D0
      SRD=0.D0
      RD=0.D0
      EP=0.D0
      EI=0.D0
      EG=0.D0
      ET=0.D0      
      TP=0.D0
      TI=0.D0
      TG=0.D0
      TT=0.D0
      SR=0.D0
      DP=0.D0
      EXT=0.D0
      NDOUT=0.D0
      SRDOUT=0.D0
      RDOUT=0.D0
      QFOUT=0.D0
      TOTIN=0.D0
      TOTOUT=0.D0
      DISC=0.D0
C
C6B-----CUMULATE RATES FOR FARMS THAT ARE NOT YET AVAILABLE PER FARM AS GLOBAL VARIABLES
      DO IC=1,NCOL
      DO IR=1,NROW
        IF(IFID(IC,IR).EQ.IFA(NF))THEN
        IF(ICID(IC,IR).NE.0)THEN
        P=P+DNINT(PFLR(IC,IR)*AC)/AC
        TT=TT+TTOT(IC,IR)
        ET=ET+ETOT(IC,IR)
        TG=TG+TGWA(IC,IR)
        EG=EG+EGWA(IC,IR)
        IF(IUNITUZF.GT.0.AND.ICCFL.EQ.3)THEN
ccrth modified to allow UZF runoff to not be accounted for when IRUNFLG=0
          IF(IUZFBND(IC,IR).GT.0.AND.IRUNFLG.GT.0)THEN
          IF(IRUNBND(IC,IR).GT.0)THEN
            SR=SR+SWRUN(IC,IR)
     1           +DBLE(EXCESPP(IC,IR)+SEEPOUT(IC,IR)+REJ_INF(IC,IR))
            DP=DP+DPERC(IC,IR)
     1           -DBLE(EXCESPP(IC,IR)+SEEPOUT(IC,IR)+REJ_INF(IC,IR))
            ENDIF
ccrth
          ELSE
            SR=SR+SWRUN(IC,IR)
            DP=DP+DPERC(IC,IR)
          ENDIF
        ELSE
        SR=SR+SWRUN(IC,IR)
        DP=DP+DPERC(IC,IR)
        ENDIF
        EP=EP+DNINT(EPPOT(IC,IR)*AC)/AC
        IF(DNINT(TPPOT(IC,IR)*AC)/AC.GE.TTOT(IC,IR)-TGWA(IC,IR)) THEN
          TP=TP+TTOT(IC,IR)-TGWA(IC,IR)
          EI=EI+0.D0
          TI=TI+0.D0
        ELSE
          TP=TP+DNINT(TPPOT(IC,IR)*AC)/AC     
          EI=EI+ETOT(IC,IR)-DNINT(EPPOT(IC,IR)*AC)/AC-EGWA(IC,IR)
          TI=TI+TTOT(IC,IR)-DNINT(TPPOT(IC,IR)*AC)/AC-TGWA(IC,IR)
        ENDIF
        ENDIF
        ENDIF
      ENDDO
      ENDDO
C
C6C-----DEFINE RATES FOR SEMI-ROUTED, ROUTED, AND GROUNDWATER DELIVERIES
      TF=TFDR(NF)
      ND=NRD(1,NF)
      QR=QREQ(NF)
      IF(QR.LT.QMAXF(NF)) QF=QR
      IF(QR.GE.QMAXF(NF)) QF=QMAXF(NF)
      IF((TF-ND-QR).NE.0.D0) THEN
        IF(IRDFL.NE.0.OR.(IGRID.GT.1.AND.FMPDAT(1)%IRDFL.NE.0)) THEN
          IF(ISWFLAG.EQ.2) RD=TF-ND-QR
        ENDIF
        IF(ISRDFL.GT.0.OR.(IGRID.GT.1.AND.FMPDAT(1)%ISRDFL.GT.0)) THEN
          IF(ISWFLAG.EQ.1) SRD=TF-ND-QR
        ENDIF
      ENDIF
C
C6D-----DEFINE TOTAL RATES INTO AND OUT OF A FARM (AND EXTERNAL DELIVERIES FOR ZERO SCENARIO),
C       AND PERCENT DISCREPANCY 
        IF(ND.LT.0D0) THEN
          NDOUT=-ND
          ND=0D0
        ENDIF
        IF(ISWFLAG.EQ.1.AND.SRD.LT.0D0) THEN
          SRDOUT=-SRD
          SRD=0D0
        ENDIF
        IF(ISWFLAG.EQ.2.AND.RD.LT.0D0) THEN
          RDOUT=-RD
          RD=0D0
        ENDIF
        IF(QF.LT.0D0) THEN
          QFOUT=-QF
          QF=0D0
        ENDIF
      TOTIN=P+ND+SRD+RD+QF+EG+TG
      TOTOUT=ET+TT+SR+DP+NDOUT+SRDOUT+RDOUT+QFOUT
      If(IDEFFL.EQ.0.AND.TOTIN.LT.TOTOUT) THEN
      EXT=TOTOUT-TOTIN
      TOTIN=TOTIN+EXT
      ENDIF
      TOTIN=TOTIN
      TOTOUT=TOTOUT
      IF(DABS(TOTIN+TOTOUT).GT.FPS) THEN
      DISC=((TOTIN-TOTOUT)/((TOTIN+TOTOUT)/2.D0))*100
      ENDIF
C
C?------INITIALIZE CUMULATIVE VOLUME VARIABLE
      IF(KPER.EQ.1.AND.KSTP.EQ.1) THEN
         DO I=1,23
           VFB(I,NF)=0.D0
         ENDDO
      ENDIF
C6E-----CALCULATE COMPACT FARM BUDGET
C
C6E1----DEFINE RATES OF INDIVIDUAL INFLOW AND OUTFLOW COMPONENTS FOR THIS TIME STEP
      IF(INT(IFBPFL/2).NE.INT((IFBPFL+1)/2)) THEN
      VAL(1)=P
      VAL(2)=ND+SRD+RD
      VAL(3)=QF+EG+TG
      VAL(4)=EXT
      VAL(5)=TOTIN
      VAL(6)=TT+ET
      VAL(7)=SR+DP
      VAL(8)=NDOUT+SRDOUT+RDOUT
      VAL(9)=QFOUT+EG+TG !EG & TG ARE THROUGH FLOWS
      VAL(10)=TOTOUT
      VAL(11)=TOTIN-TOTOUT
      VAL(12)=DISC
C
C6E2----DEFINE CUMULATIVE VOLUMES OF INDIVIDUAL INFLOW AND OUTFLOW COMPONENTS,
C       AND OF TOTAL CUMULATIVE VOLUMES INTO AND OUT FO A FARM,
C       AND PERCENT DISCREPANCY
      DO I=1,11
      VFB(I,NF)=VFB(I,NF)+VAL(I)*DBLE(DELT)
      ENDDO
        IF(DABS(VFB(5,NF)+VFB(10,NF)).GT.FPS) THEN
          VFB(12,NF)=(VFB(11,NF)/((VFB(5,NF)+VFB(10,NF))/2.D0))*100.D0
        ELSE
          VFB(12,NF)=0.D0
        ENDIF
      ENDIF
C
C6F-----CALCULATE DETAILED FARM BUDGET
C
C6F1----DEFINE RATES OF INDIVIDUAL INFLOW AND OUTFLOW COMPONENTS FOR THIS TIME STEP
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) THEN
      VAL(1)=P
      VAL(2)=ND
      IF(ISWFLAG.EQ.1) VAL(3)=SRD
      IF(ISWFLAG.EQ.2) VAL(4)=RD
      VAL(5)=QF
      VAL(6)=EG
      VAL(7)=TG
      VAL(8)=EXT
      VAL(9)=TOTIN
      VAL(10)=EI
      VAL(11)=EP
      VAL(12)=EG
      VAL(13)=TI
      VAL(14)=TP
      VAL(15)=TG
      VAL(16)=SR
      VAL(17)=DP
      VAL(18)=NDOUT
      IF(ISWFLAG.EQ.1) VAL(19)=SRDOUT
      IF(ISWFLAG.EQ.2) VAL(20)=RDOUT
      VAL(21)=QFOUT
      VAL(22)=TOTOUT
      VAL(23)=TOTIN-TOTOUT
      VAL(24)=DISC
C
C6F2----DEFINE CUMULATIVE VOLUMES OF INDIVIDUAL INFLOW AND OUTFLOW COMPONENTS,
C       AND OF TOTAL CUMULATIVE VOLUMES INTO AND OUT FO A FARM,
C       AND PERCENT DISCREPANCY
      DO I=1,23
      VFB(I,NF)=VFB(I,NF)+VAL(I)*DBLE(DELT)
      ENDDO
        IF(DABS(VFB(9,NF)+VFB(22,NF)).GT.FPS) THEN
          VFB(24,NF)=(VFB(23,NF)/((VFB(9,NF)+VFB(22,NF))/2.D0))*100.D0
        ELSE
          VFB(24,NF)=0.D0
        ENDIF
      ENDIF
C
C6G----PRINT FARM BUDGET TO ASCII FILE
      IF(IFBPFL.EQ.1.OR.IFBPFL.EQ.2) THEN
      IF(IFBPFL.EQ.1) IVAL=12
      IF(IFBPFL.EQ.2) IVAL=24
      DO I=1,IVAL
      IF(DABS(VAL(I)).GE.1D10.OR.
     1  (DABS(VAL(I)).LT.1D-1.AND.DABS(VAL(I)).GT.FPS)) THEN
         WRITE(CVAL(I),'(1PE17.4)') VAL(I)
      ELSE
         WRITE(CVAL(I),'(F17.4)') VAL(I)
      END IF
      IF(DABS(VFB(I,NF)).GE.1D10.OR.
     1  (DABS(VFB(I,NF)).LT.1D-1.AND.DABS(VFB(I,NF)).GT.FPS)) THEN
         WRITE(CVAL(I+IVAL),'(1PE17.4)') VFB(I,NF)
      ELSE
         WRITE(CVAL(I+IVAL),'(F17.4)') VFB(I,NF)
      END IF
      ENDDO
!      write(fmt_number,'(i10)') IVAL*2
      fmt_string='(2I12,A14,I12,X,'//NUM2STR(2*IVAL)//'(A,X))'          !SWITCHED OUT TRIM(ADJUSTL(fmt_number))
      IF(IFBPFL.EQ.1)  WRITE(FMPOUT%UNIT(7),FMT=TRIM(fmt_string))        !seb
     1 KPER,KSTP,TIME,NF,(CVAL(I),I=1,IVAL*2)
      IF(IFBPFL.EQ.2)  WRITE(FMPOUT%UNIT(8),FMT=TRIM(fmt_string))
     1 KPER,KSTP,TIME,NF,(CVAL(I),I=1,IVAL*2)                           !seb LINE USED TO BE: KPER,KSTP,TIME,IGRID
C
C6H-----PRINT FARM BUDGET TO BINARY FILE
      ELSEIF((IFBPFL.GT.2.AND.IBDOPT.NE.2).OR.
     1       (IFBPFL.GT.2.AND.ICBCFL.EQ.2)) THEN
      IF(INT(IFBPFL/2).EQ.INT((IFBPFL+1)/2)) THEN
      IVAL=24
      ELSE
      IVAL=12
      ENDIF
      WRITE(IFBPFL) KPER, KSTP, TIME, IGRID, NF,
     1 (VAL(I),I=1,IVAL),(VFB(I,NF),I=1,IVAL)
C     
      ENDIF
      ENDIF
C
      END DO ! NF=1,NFARMS 
C
C7===== PRINT 2D-ARRAY OF FRACTIONS OF ACTIVE ACREAGE FOR EACH CELL =========================================
C
      IF(IOPFL.EQ.-1.OR.IOPFL.EQ.1) THEN
C        IF(IOPFL.EQ.-1) IOUTFILE=IOUT                                  !Opened in AR seb
C        IF(IOPFL.EQ.1) THEN
C          OPEN(UNIT=1006, FILE='ACR_OPT.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1006
C        ENDIF
        WRITE(FMPOUT%UNIT(6),40) KPER,KSTP,NFARMS,TIME,TIMEUNIT
   40   format(1X,/,1X,
     1  'FRACTIONS OF ACTIVE CELL ACREAGE (-1.000 = NO-FARM CELL)',7X,
     2  'PERIOD ',I4,8X,'STEP',I4,4X,I9,' FARMS',7X,'ELAPSED TIME',
     3  A14,1X,A14) 
!        write(fmt_number,'(i10)') NCOL
        fmt_string='(1X,'//NUM2STR(NCOL)//'F8.3)'                       !CHANGED FROM TRIM(ADJUSTL(fmt_number))
        DO I=1,NROW
        WRITE(FMPOUT%UNIT(6),FMT=TRIM(fmt_string))(REDPCT(J,I),J=1,NCOL)
        ENDDO
      ENDIF
C     
      ENDIF
C
C8===== CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM WELLS ================================================
C
C8A-----CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL BUDGET FLAG.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IFWLCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IFWLCB.GT.1) IBD=ICBCFL
      IBDLBL=0
C
C8B-----IF FINAL PUMPING RATES WILL BE SAVED AS A LIST TO ASCII FILE, WRITE HEADER
C       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
      IF(IFWLCB.EQ.1) THEN
C      OPEN(UNIT=1002, FILE='FWELLS.OUT', STATUS='UNKNOWN')               !Opened in AR seb
      IF(IGRID.EQ.1.AND.KPER.EQ.1.AND.KSTP.EQ.1) 
     1                                WRITE(FMPOUT%UNIT(2),33) TIMEUNIT
   33 FORMAT
     1(2X,'PER',2X,'STP',A14,3X,
     2'GRID    FARM-WELL ID  FARM ID  LAYER    ROW    COL         RATE')
      ENDIF
C
C8C----IF FINAL PUMPING RATES WILL BE SAVED AS A LIST TO COMPACT BUDGET BINARY FILE,
C      WRITE HEADER.
      IF(IBD.EQ.2) THEN
        NAUX=NFWLVL-7-IFWLAL
        IF(IAUXSV.EQ.0) NAUX=0
        IF(IUNITMNW1.GT.0) THEN
         IMAWFL=0
         DO L=1,NFWELS
         IF(IDINT(FWELL(4,L)).LT.0) THEN                                ! CHANGED FROM ZERO LAYER CHECK TO WELL ID<0
           IR=IDINT(FWELL(2,L))
           IC=IDINT(FWELL(3,L))
           IL=IDINT(FWELL(1,L))   
           do n=1,nwell2
             if(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
               m=IDINT(well2(1,n))
               j = int(mod((m-1),ncol*nrow)/ncol) + 1
               i = mod((m-1),ncol) + 1
               if(ir.eq.j.and.ic.eq.i) then
               IL=int((m-1)/(ncol*nrow))+1
               endif
             endif
           enddo
           IMAWFL=1
           IF(IPFWEL.EQ.1.AND.KPER.EQ.1.AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,60) IDINT(FWELL(2,L)),IDINT(FWELL(3,L)),IL
           ENDIF
   60      FORMAT(1X,'CUMULATIVE PUMPING RATE OF MULTI-AQUIFER FARM',
     1     ' WELL (ROW:',I8,'; COL.:',I8,') IS LISTED IN BINARY FILE',
     2     ' FOR TOP NODE IN LAYER ',I3)
           ENDIF
         ENDIF
         ENDDO
         IF(IMAWFL.EQ.1) THEN
           IF((IPFWEL.EQ.0.OR.KPER.GT.1).AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,65)
           ENDIF
   65      FORMAT(1X,'CUMULATIVE PUMPING RATES OF MULTI-AQUIFER FARM ',
     1     'WELLS ARE LISTED IN BINARY FILE FOR RESPECTIVE TOP NODES')
           ENDIF
           IF(IPFWEL.EQ.1.AND.KPER.GT.1.AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,66)
           ENDIF
   66      FORMAT(1X,'(ROW & COLUMN LOCATION AND LAYER OF TOP NODES ',
     1     'ARE ONLY PRINTED TO LISTING FILE ONCE FOR STRESS PERIOD 1)')
           ENDIF
         ENDIF
        ENDIF
C
        IF(IUNITMNW2.GT.0) THEN
         IMAWFL=0
         DO L=1,NFWELS
          FID=IDINT(FWELL(5,L))
          WID=IDINT(FWELL(4,L))
          IF( WID.LT.0 ) THEN                                           ! SCOTT SHOULD PRINTING BE MADE FOR ALL FARMS OR ONLY WELLS ASSOCIATED WITH FARMS THAT ARE ACTIVE
            IMAWFL=1   
           IF(IPFWEL.EQ.1.AND.KPER.EQ.1.AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,67) FID, TRIM(MNW2NAM(L)), 
     +                      IDINT(FWELL(2,L)),IDINT(FWELL(3,L))
           ENDIF
   67      FORMAT(1X,'CUMULATIVE PUMPING RATE OF FARM WELL,',I8
     1     ' LINKED TO MNW2 WELL ',A,' THAT IS LOCATED IN ROW ',I8,
     2     'COL ', I8, ' IS LISTED IN BINARY FILE' )
           ENDIF
         ENDIF
         ENDDO
         IF(IMAWFL.EQ.1) THEN
           IF((IPFWEL.EQ.0.OR.KPER.GT.1).AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,68)
           ENDIF
   68      FORMAT(1X,'CUMULATIVE PUMPING RATES OF FARM WELLS LINKED ',
     1     'TO MNW2 ARE LISTED IN BINARY FILE FOR RESPECTIVE TOP NODES')   ! SCOTT NOT SURE IF THIS IS CORRECT
           ENDIF
           IF(IPFWEL.EQ.1.AND.KPER.GT.1.AND.IWL2CB.EQ.0) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,69)
           ENDIF
   69      FORMAT(1X,'(ROW & COLUMN LOCATION AND LAYER OF TOP NODES ',
     1     'ARE ONLY PRINTED TO LISTING FILE ONCE FOR STRESS PERIOD 1)')
           ENDIF
         ENDIF
        END IF
C      
        NFW=NFWELS
        IF(IUNITMNW1.GT.0 .OR. IUNITMNW2.GT.0) THEN
         IF(IWL2CB.NE.0) THEN
          DO L=1,NFWELS
          IF(IDINT(FWELL(4,L)).LT.0) NFW=NFW-1 
          ENDDO
         ENDIF
        ENDIF
C 
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,FWLAUX,IFWLCB,NCOL,NROW,NLAY,
     1              NFW,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
      ENDIF
C
C8D-----CLEAR THE BUFFER.
      FORALL(IC=1:NCOL,IR=1:NROW,IL=1:NLAY) BUFF(IC,IR,IL)=ZER  !modified seb
C      DO IL=1,NLAY
C      DO IR=1,NROW
C      DO IC=1,NCOL
C         BUFF(IC,IR,IL)=ZER
C      ENDDO
C      ENDDO
C      ENDDO
C
C8E-----IF THERE ARE NO FARM WELLS, DO NOT ACCUMULATE FLOW.
      IF(NFWELS.EQ.0) GO TO 200
C  
C8F-----LOOP THROUGH EACH FARM WELL CALCULATING FLOW AND PRINTING
      DO 100 L=1,NFWELS
C
C8F1----GET LAYER, ROW & COLUMN OF CELL CONTAINING FARM WELL.
      IL=IDINT(FWELL(1,L))
      IR=IDINT(FWELL(2,L))
      IC=IDINT(FWELL(3,L))
      WID=IDINT(FWELL(4,L))
      FID=IDINT(FWELL(5,L))
      if(IUNITMNW1.gt.0.and.WID.LT.0) THEN                !SWAPPED ZERO LAYER CHECK FOR NEGATIVE WELL-ID  rth  MNW2 ALREADY HAS IN FWELL(1,:) THE TOP NODES LAYER
C     il=1
        do n=1,nwell2                                                   ! REPEAT THIS LOOP IS REPEATED NEEDLESS IN SUBSEQUENT SECTIONS OF CODE SEE COMMENTS THAT START WITH SCOTT
          if(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
            m=IDINT(well2(1,n))
            j = int(mod((m-1),ncol*nrow)/ncol) + 1
            i = mod((m-1),ncol) + 1
            if(ir.eq.j.and.ic.eq.i) then
            IL=int((m-1)/(ncol*nrow))+1
            endif
          endif
        enddo
      ENDIF
      QQ=ZERO
C
C8F2----IF THE CELL IS NO-FLOW, CONSTANT_HEAD, HEAD BELOW BOTTOM FOR CONVERTABLE, 
C-------OR ASSOCIATED WITH AN INACTIVE FARM IGNORE IT.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 99
      IF(LAYHDT(IL).NE.0 .AND. 
     +   HNEW(IC,IR,IL).LT.BOTM(IC,IR,LBOTM(IL))) GO TO 99              !seb skip cells that are convertable with head below bottom inactive
      IF(.NOT. LFID(FID) ) GO TO 99                                     !seb skip wells associated with inactive farms
C
C8F3----GET FLOW RATE AS CALCULATED IN FM MODULE.
      QQ=FWELL(NFWLVL,L)
ccrth
C      IF(KPER.eq.1.and.iw1.eq.1.and.Iunitnwt.ne.0)                           !Opened in AR seb
C     & OPEN(UNIT=1011, FILE='RED_FMP_PMP.OUT', STATUS='UNKNOWN')
cc
      IF(WID.GT.0.AND.                                              !Only use single-aquifer FMP wells and not ones connected to MNW1 =>Farm-well ID is negative
     1                Iunitnwt.GT.0)THEN                            !currently only allow NWT-smoothing for deficit irrigation or water stacking options
ccrth      IF(IUNITNWT.GT.0.and.IDEFFL.lt.0)THEN
        QSAVE(L) = QQ
        Q = QQ
        bbot = Botm(IC, IR, Lbotm(IL))
        ttop = Botm(IC, IR, Lbotm(IL)-1)
        Hd = HNEW(ic,ir,il)
        SatDif=Hd-bbot
        IF (LAYTYPUPW(il).GT.0.and.SatDif.LE.SATTHK) THEN
          Qp = SMOOTH3(Hd,Ttop,Bbot,dQp,2)
          Q = Q*Qp
        END IF
      ENDIF
! write wells with reduced pumping
      IF (IUNITNWT.GT.0) THEN !rgn         
      IF(QSAVE(L).LT.0)THEN
          Qp=QQ/QSAVE(L)
         ELSE
          Qp=1.0
         ENDIF
      END IF !rgn
       IF (Iunitnwt.NE.0 .AND. (Qp.LT.0.9999999D0.and.Qp.gt.0.D0) ) THEN
        IF ( iw1.EQ.1 ) THEN
         WRITE(FMPOUT%UNIT(10),*)                                       !WRITE TO 'RED_FMP_PMP.OUT'
         WRITE(FMPOUT%UNIT(10),300)KPER,KSTP
         WRITE(FMPOUT%UNIT(10),400)
        END IF
        WRITE(FMPOUT%UNIT(10),500)KPER,KSTP,IL,IR,IC,REAL(QSAVE(L)),
     1         REAL(QQ),REAL(Hd),REAL(bbot),WID,
     2         IDINT(FWELL(5,L))
        iw1 = iw1 + 1
      END IF
  300 FORMAT('FARM WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ',I5,
     1      ' TIME STEP ',I5)
  400 FORMAT('   PER   STP  LAY   ROW   COL   FMP-EST-Q      REDUCED-Q',
     1       '        GW-HEAD       CELL-BOT  FARM-WELL_No.',
     2       '       FARM_No.')
  500 FORMAT(2X,5(2X,I8),4G15.6,2(7x,I8))
ccrth
C
C8F4----PRINT FLOW RATE PER FARM-WELL TO LIST-FILE IF REQUESTED
C       (MULTI-AQUIFER WELL PUMPAGE INDICATED BY ZERO LAYER NUMBER)
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,61) TEXT2,KPER,KSTP,NFWELS,TIME,TIMEUNIT
         ENDIF
   61    FORMAT(1X,//,1X,A,'   PERIOD',I3,'   STEP',I4,
     1   4X,I9,' FARMS WELLS',7X,'ELAPSED TIME',A14,1X,A14,/)
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,62)
         ENDIF
   62    FORMAT(1X,
     1   'FARM-WELL ID  FARM ID  LAYER    ROW    COL         RATE')
         ENDIF
         IF(IUNITMNW1.GT.0.AND.IDINT(FWELL(1,L)).EQ.0) IL=0             ! SCOTT THIS IS STRANGE, IT IS SAYING IF MNW1 IS ON AND IL=0 THEN SET IL=0 SO IT SEEMS TO SERVE NO PURPOSE
         IF(LSTCHK(3)) THEN
          WRITE(IOUT,63) WID,IDINT(FWELL(5,L)),IL,IR,IC,QQ! seb MOVED TO PREVENT TRUNCATING QQ TO Q
         ENDIF
   63    FORMAT(4X,2I9,3I7,1PG17.8)
         IBDLBL=1
         IF(IUNITMNW1.GT.0.AND.WID.LT.0) THEN
C        reset layer associated with Q to top layer of multi-layer mnw-well
         do n=1,nwell2                                                  ! SCOTT THIS SET OF LOOPS IS ALREADY PREFORMED UNDER COMMENT REPEAT
          if(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
            m=IDINT(well2(1,n))
            j = int(mod((m-1),ncol*nrow)/ncol) + 1
            i = mod((m-1),ncol) + 1
            if(ir.eq.j.and.ic.eq.i) then
            IL=int((m-1)/(ncol*nrow))+1
            endif
          endif
         enddo
         ENDIF
      ENDIF
C
C8F5----PRINT FLOW RATE TO ASCII FILE
C       (MULTI-AQUIFER WELL PUMPAGE INDICATED BY ZERO LAYER NUMBER)
      IF(IFWLCB.EQ.1) THEN
         IF(IUNITMNW1.GT.0.AND.IDINT(FWELL(1,L)).EQ.0) IL=0             ! SCOTT THIS IS STRANGE, IT IS SAYING IF MNW1 IS ON AND IL=0 THEN SET IL=0 SO IT SEEMS TO SERVE NO PURPOSEWID.LT.0) IL=0
         WRITE(FMPOUT%UNIT(2),64) KPER,KSTP,TIME,IGRID,
     1   WID,IDINT(FWELL(5,L)),IL,IR,IC,QQ
   64    FORMAT(2I5,A14,I7,I16,I9,3I7,1PG17.8)
         IF(IUNITMNW1.GT.0.AND.WID.LT.0) THEN
C        reset layer associated with Q to top layer of multi-layer mnw-well
         do n=1,nwell2                                                  ! SCOTT THIS SET OF LOOPS IS ALREADY PREFORMED UNDER COMMENT REPEAT
          if(WELL2(8,N).GT.1.0D30.and.WELL2(7,N).LT.1.0D30) then
            m=IDINT(well2(1,n))
            j = int(mod((m-1),ncol*nrow)/ncol) + 1
            i = mod((m-1),ncol) + 1
            if(ir.eq.j.and.ic.eq.i) then
            IL=int((m-1)/(ncol*nrow))+1
            endif
          endif
         enddo
         ENDIF
      ENDIF      
C
C8F6----ADD FLOW RATE TO BUFFER.
      IF(IUNITMNW1.GT.0)THEN
        IF(IWL2CB.NE.0) THEN
          IF(WID.GT.0) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+SNGL(QQ)           ! SCOTT NOTE THAT FLOW RATES FOR MNW WELLS ARE NOT STORED                   
        ELSE
          BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+SNGL(QQ)
        ENDIF
      ELSE 
        BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+SNGL(QQ)
      ENDIF
C
C8F7----SEE IF FLOW IS POSITIVE OR NEGATIVE.
      IF(WID.GT.0) THEN
         IF    (QQ.LT.ZERO) THEN 
C8F9----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT (NOTE: MNW-WELLS ADDED IN MNW PACKAGE).
           RATOUT=RATOUT-QQ
         ELSEIF(QQ.GT.ZERO) THEN
C8F8----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN (NOTE: MNW-WELLS ADDED IN MNW PACKAGE).
           RATIN =RATIN +QQ
         END IF 
      ENDIF
C
C8F10---CELL-BY-CELL FLOWS (PUMPING RATES) WILL BE SAVED AS A LIST IN A BINARY FILE, 
C       IF "COMPACT BUDGET" IS SPECIFIED IN OUTPUT CONTROL. (ANALOGOUS TO WEL6)
C       OR IF RETURNING THE FLOW IN THE FARM-WELL ARRAY, COPY FLOW TO FARM-WELL.
   99 IF(IUNITMNW1.GT.0.OR.IUNITMNW2.GT.0)THEN
        IF(IWL2CB.NE.0) THEN
          IF(WID.GT.0.AND.IBD.EQ.2)
     1             CALL UBDSVB(IFWLCB,NCOL,NROW,IC,IR,IL,SNGL(QQ),
     2                  SNGL(FWELL(:,L)),NFWLVL,NAUX,7,IBOUND,NLAY)     !seb CHANGED FROM FWELL(1,L) TO FWELL(:,L)
        ELSE
          IF(IBD.EQ.2) CALL UBDSVB(IFWLCB,NCOL,NROW,IC,IR,IL,SNGL(QQ),
     1                  SNGL(FWELL(:,L)),NFWLVL,NAUX,7,IBOUND,NLAY)     !seb CHANGED FROM FWELL(1,L) TO FWELL(:,L)
        ENDIF
      ELSE
        IF(IBD.EQ.2) CALL UBDSVB(IFWLCB,NCOL,NROW,IC,IR,IL,SNGL(QQ),
     1                  SNGL(FWELL(:,L)),NFWLVL,NAUX,7,IBOUND,NLAY)     !seb CHANGED FROM FWELL(1,L) TO FWELL(:,L)       
      ENDIF
      IF(IFWLAL.NE.0) FWELL(NFWLVL,L)=QQ                                ! SCOTT THIS SEEMS STRANTE BECAUSE QQ NEVER CHANGES AND IS INITIALLY SET TO QQ=FWELL(NFWLVL,L)
  100 CONTINUE
ccrth==Write out the reduced flows aggregated over the entire farm if NWT smoothing is used with Single-aquifer Farm wells
      IF (Iunitnwt.NE.0 ) THEN
        IF(IUNITRAMPF.NE.IOUT.OR.LSTCHK(3)) THEN
            DO 101 I=1,NFARMS
              WRITE(IUNITRAMPF,*)
ccrth         WRITE(IUNITRAMPF,301)KPER,KSTP,IDINT(FWELL(5,L)),
ccrth     &    QXTF(IDINT(FWELL(5,L)))
  101       CONTINUE 
         END IF
      END IF
  301 FORMAT(' WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ',I5,
     1    ' TIME STEP ',I5,' for Farm ',I5,'Total Reduced Flow ',E15.6)
ccrth
C
C8G-----CELL-BY-CELL FLOWS (PUMPING RATES) WILL BE SAVED AS A 3-D ARRAY IN A BINARY FILE, 
C       IF "COMPACT BUDGET" IS NOT SPECIFIED IN OUTPUT CONTROL (ANALOGOUS TO WEL6),
C       AND PRINT INFO TELLING THAT MULTI-AQUIFER WELL PUMPAGE IN 3-D ARRAY IS
C       ASSOCIATED WITH NODE OF TOP LAYER OF MULTI-LAYER MNW-WELL.
      IF(IBD.EQ.1) THEN
      CALL UBUDSV(KSTP,KPER,TEXT,IFWLCB,BUFF,NCOL,NROW,NLAY,IOUT)
        IF(IUNITMNW1.GT.0) THEN
        IF(IWL2CB.EQ.0) THEN
         IMAWFL=0
         DO L=1,NFWELS
           IF(FWELL(1,L).EQ.0) THEN
             IMAWFL=1      
             IF(IPFWEL.EQ.1.AND.KPER.EQ.1) THEN
               IF(LSTCHK(3)) THEN
                 WRITE(IOUT,60) IDINT(FWELL(2,L)),IDINT(FWELL(3,L)),IL
               ENDIF
             ENDIF
           ENDIF
         ENDDO
         IF(IMAWFL.EQ.1) THEN
           IF(IPFWEL.EQ.0.OR.KPER.GT.1) THEN
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,65)
             ENDIF
           ENDIF
           IF(IPFWEL.EQ.1.AND.KPER.GT.1) THEN
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,66)
             ENDIF
           ENDIF
         ENDIF
        ENDIF
        ENDIF
      ENDIF
C-----MNW2 LINK RESET QDES AND QACT FOR TO PREVENT MNW2 WELLS FROM PUMPING WHEN NO LONGER LINKED TO (PREVENTS POTENTIAL USER ERROR WITH USING -1 ON MNW2 STRESS PERIOD INPUT)
      IF(IUNITMNW2.GT.0) THEN                          !RESET QACT and QDES for MNW2 on first interation
        DO L=1,NFWELS
          FID=IDINT(FWELL(5,L))
          WID=IDINT(FWELL(4,L))
          IF(WID.GE.0 .OR. .NOT.LFID(FID)) CYCLE                        !SKIP FARM WELLS THAT ARE NOT LINKED TO MNW2 AND FARM WELLS ASSOCIATED WITH INACTIVE FARMS
          J=MNW2LOC(L)
          FIRSTNODE=IDINT(MNW2(4,J))
          LASTNODE =IDINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )           !J=MNW2LOC(L) TRANSLATES FARM WELL LOCATION TO MNW2 WELL LOCATION
          MNW2(5,J)=0D0
          MNWNOD(4,FIRSTNODE:LASTNODE)=0D0
        ENDDO
      END IF
C
C8H-----MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=SNGL(RATIN)
      ROUT=SNGL(RATOUT)
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8I-----INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C9==== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3FNRBD(KSTP,KPER,IGRID)
C-----VERSION 2 09/21/09 FMP3FNRBD
C     ******************************************************************
C     CALCULATE AND PRINT VOLUMETRIC BUDGET FOR FARM NET RECHARGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,   ONLY:FNRCH,NFARMS,IFID,IFNRCB,IFA,FMPOUT,SFMP3PNT
      USE FMPBLK,      ONLY:TPL,TPU,ZERO,ZER
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,ITMUNI, 
     1                      HNEW, BOTM, LBOTM, LAYHDT
      USE GWFBASMODULE,ONLY:DELT,VBVL,VBNM,MSUM,ICBCFL,PERTIM,TOTIM
      USE CVT2STR,     ONLY: NUM2STR
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER KSTP,KPER,IGRID
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT
      CHARACTER*14 TIMEUNIT
      CHARACTER*14 TIME
      CHARACTER*10 fmt_number
      CHARACTER*20 fmt_string
      DATA TEXT /'FARM  NET  RECH.'/
      INTEGER IRCH(NCOL,NROW)
      INTEGER I,IL,IR,IC,IBD,J,NF,NOPT                                  !FORMERLY IMPLICIT INTEGER
      REAL ROUT, RIN                                                    !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION RATIN,RATOUT,QQ,FNRECH
C     ------------------------------------------------------------------
C      INCLUDE 'openspec.inc'                                           !No Longer Needed seb
C     ------------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
C
C1===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
      IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
      IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
      IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
      IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
      IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
      IF(TOTIM.GE.TPU.OR.TOTIM.GT.TPL) THEN
         WRITE(TIME,'(1PE14.7)') TOTIM
      ELSE
         WRITE(TIME,'(F14.2)') TOTIM
      END IF
C
C2===== CALCULATE VOLUMETRIC BUDGET OF FARM NET RECHARGE ===================================================
C
C2A-----INITIALIZE
C
C2A1----CLEAR THE RATE ACCUMULATORS, .
      RATIN=ZERO
      RATOUT=ZERO
C
C2A2----CLEAR THE BUFFER & SET FLAG FOR SAVING CELL-BY-CELL FLOW TERMS.
      FORALL(IC=1:NCOL,IR=1:NROW,IL=1:NLAY) BUFF(IC,IR,IL)=ZER  !modified seb
C      DO IL=1,NLAY
C      DO IR=1,NROW
C      DO IC=1,NCOL
C        BUFF(IC,IR,IL)=ZER
C      ENDDO
C      ENDDO
C      ENDDO
      IBD=0
      IF(IFNRCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IFNRCB.GT.3) IBD=ICBCFL
C
C2B-----ASSIGN FARM NET RECHARGE TO HIGHEST ACTIVE CELL IN A VERTICAL COLUMN (NOT "NO FLOW"),
C       AND PROCESS HORIZONTAL CELL LOCATIONS ONE AT A TIME.
C       (analogous to option 3 recharge package)
      ROW_LP: DO IR=1,NROW  !modified seb changed to named loop
      COL_LP: DO IC=1,NCOL
C
C2B1----LOOP THROUGH CELLS IN A VERTICAL COLUMN TO FIND WHERE TO PLACE FARM NET RECHARGE.
      IRCH(IC,IR)=1
      LAY_LP: DO IL=1,NLAY
C
C2B2----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
      IF(IBOUND(IC,IR,IL).LT.0) EXIT LAY_LP         !removed GO TO 29 seb
C
C2B3----IF CELL IS INACTIVE MOVE DOWN TO NEXT CELL.
      IF(IBOUND(IC,IR,IL).EQ.0)                   CYCLE LAY_LP  !removed GO TO 28 seb
      IF(LAYHDT(IL).NE.0 .AND. 
     +   HNEW(IC,IR,IL).LT.BOTM(IC,IR,LBOTM(IL))) CYCLE LAY_LP  !removed GO TO 28 seb
C
C2B4----CELL IS VARIABLE HEAD, SO APPLY NET RECHARGE TO IT. 
C       ADD NET RECHARGE TO BUFFER, AND STORE LAYER NUMBER IN IRCH.
      QQ=FNRCH(IC,IR)
      BUFF(IC,IR,IL)=SNGL(QQ)
      IRCH(IC,IR)=IL
C
C2B5----IF FARM NET RECHARGE IS POSITIVE ADD IT TO RATIN ELSE ADD IT TO RATOUT.
      IF(QQ.LT.ZERO) RATOUT=RATOUT-QQ
c      IF(QQ.EQ.ZERO) EXIT LAY_LP     !redundant statement if end of loop is reached automantic exit seb
      IF(QQ.GT.ZERO) RATIN=RATIN+QQ   !modified seb fixed .EQ. to .GT.
      EXIT LAY_LP
      END DO LAY_LP
      END DO COL_LP
      END DO ROW_LP
C
C
C3===== PRINT 2D-ARRAY OF CELL-BY-CELL NET RECHARGE FLOW RATES TO LIST FILE ===============================
C       (FOR TIME STEPS SPECIFIED BY OUTPUT CONTROL)
C
C3A-----WRITE HEADER TO LIST FILE.
      IF(IBD.LT.0) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1) TEXT,KPER,KSTP,NCOL,NROW,NLAY,TIME,TIMEUNIT
      ENDIF
    1 FORMAT(1X,//,1X,A,'   PERIOD',I3,'   STEP',I3,',',I4,' COLUMNS,',
     1I4,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)
C
C3B-----WRITE DATA AS ONE OR TWO RECORDS CONTAINING ONE VALUE PER LAYER.
      IF(NLAY.EQ.1) THEN
C
C3B1----WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//TRIM(ADJUSTL(fmt_number))//'G12.6)'
      DO I=1,NROW
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,FMT=TRIM(fmt_string))(BUFF(J,I,1),J=1,NCOL)
      ENDIF
      ENDDO
      ELSE
C
C3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
C       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
!      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//NUM2STR(NCOL)//'I12)'                           !CHANGED FROM TRIM(ADJUSTL(fmt_number))
      DO I=1,NROW
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,FMT=TRIM(fmt_string)) (IRCH(J,I),J=1,NCOL)
      ENDIF
      ENDDO
!      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//NUM2STR(NCOL)//'F12.4)'                         !CHANGED FROM TRIM(ADJUSTL(fmt_number))
      DO I=1,NROW
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,FMT=TRIM(fmt_string))(BUFF(J,I,IRCH(J,I)),J=1,NCOL)
      ENDIF
      ENDDO
c
      ENDIF
C
      ENDIF
C
C
C4===== PRINT NET RECHARGE FLOW RATES TO ASCII FILES ========================================================
C
C4A-----PRINT 2D-ARRAY OF CELL-BY-CELL NET RECHARGE FLOW RATE TO ASCII FILE
C       (FOR EACH TIME STEP)

C4A1----WRITE HEADER TO ASCII FILE.
      IF(IFNRCB.EQ.1) THEN
C      OPEN(UNIT=1004, FILE='FNRCH_ARRAY.OUT', STATUS='UNKNOWN')          !File Opened in AR seb
      WRITE(FMPOUT%UNIT(3),1) TEXT,KPER,KSTP,NCOL,NROW,NLAY,
     1                        TIME,TIMEUNIT
C
C4A2----WRITE DATA AS ONE OR TWO RECORDS CONTAINING ONE VALUE PER LAYER.
      IF(NLAY.EQ.1) THEN
C
C4A2A---WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
!      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//NUM2STR(NCOL)//'G12.6)'
      DO I=1,NROW
      WRITE(FMPOUT%UNIT(3),FMT=TRIM(fmt_string))(BUFF(J,I,1),J=1,NCOL)
      ENDDO
      ELSE
C
C4A2B---WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
C       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
!      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//NUM2STR(NCOL)//'I12)'                           !REMOVED TRIM(ADJUSTL(fmt_number))
      DO I=1,NROW
      WRITE(FMPOUT%UNIT(3),FMT=TRIM(fmt_string)) (IRCH(J,I),J=1,NCOL)
      ENDDO
!      write(fmt_number,'(i10)')NCOL
      fmt_string = '('//NUM2STR(NCOL)//'F12.4)'                        !REMOVED TRIM(ADJUSTL(fmt_number))
      DO I=1,NROW
      WRITE(FMPOUT%UNIT(3),FMT=TRIM(fmt_string)) 
     1                                    (BUFF(J,I,IRCH(J,I)),J=1,NCOL)
      ENDDO
c 
      ENDIF
C
      ENDIF
C
C4B---- PRINT CUMULATIVE FARM NET RECHARGE FLOW RATE FOR EACH FARM TO ASCII FILE
C       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
C       (FOR EACH TIME STEP)
C
C4B1----WRITE HEADER TO ASCII FILE
      IF(IFNRCB.EQ.2) THEN
C      OPEN(UNIT=1003, FILE='FNRCH_LIST.OUT', STATUS='UNKNOWN')                   !Opened in AR seb
      IF(KPER.EQ.1.AND.KSTP.EQ.1) WRITE(FMPOUT%UNIT(4),33) TIMEUNIT
   33 FORMAT
     1(2X,'PER',2X,'STP',A14,4X,'FARM ID         RATE')
C
C4B2----WRITE LIST OF CUMULATIVE FARM NET RECHARGE FOR EACH FARM
        DO NF=1,NFARMS
        FNRECH=0.D0         
          DO IR=1,NROW
          DO IC=1,NCOL
             IF(IFID(IC,IR).EQ.IFA(NF))    THEN
             FNRECH=FNRECH+FNRCH(IC,IR)
             ENDIF
          ENDDO
          ENDDO
        WRITE(FMPOUT%UNIT(4),64) KPER,KSTP,TIME,IFA(NF),FNRECH
   64   FORMAT(2I5,A14,I11,G17.8)     
        ENDDO
      ENDIF      
C
C
C6===== PRINT NET RECHARGE FLOW RATE TO BINARY FILES ========================================================
C
C6A-----PRINT LIST OF CUMULATIVE FARM NET RECHARGE FLOW RATES FOR EACH FARM TO BINARY FILE.
C
C6A1----WRITE HEADERS TO BINARY FILE
C
C6A1A---HEADER FOR TIME SERIES:
C       (TO BUILD TIME SERIES FOR FARMS: PER,STP, AND TIME ARE INCLUDED FOR EACH FARM)
        IF(IFNRCB.EQ.3) THEN
C         OPEN(UNIT=1005, FILE='FNRCH_LIST_BIN.OUT', STATUS='UNKNOWN',                  !File Opened in AR seb
C     1   FORM=FORM)
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,65) IFNRCB,KSTP,KPER
          ENDIF
   65     FORMAT(/,1X,'SAVING ',
     1    '"TIME SERIES OF CUMULATIVE FARM NET RECHARGE FOR EACH FARM"',
     2    ' ON UNIT',I5,' AT TIME STEP',I5,', STRESS PERIOD',I4)      
          IF(KPER.EQ.1.AND.KSTP.EQ.1) THEN
           WRITE(FMPOUT%UNIT(5)) '         PER','         STP',TIMEUNIT,
     1      '         FID','        RATE'
          ENDIF
C
C6A1B---HEADER OF LISTS PRINTED AT TIME STEPS THAT ARE SPECIFIED BY OUTPUT CONTROL:
        ELSEIF(IFNRCB.GT.3.AND.IBD.EQ.1) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,66) IFNRCB,KSTP,KPER
          ENDIF
   66     FORMAT(/,1X,'SAVING ',
     1    '"CUMULATIVE FARM NET RECHARGE FOR EACH FARM" ON UNIT',I5,
     2    ' AT TIME STEP',I5,', STRESS PERIOD',I4)
          WRITE(IFNRCB) KSTP,KPER,TEXT,NCOL,NROW,NLAY   !Scott check why writing to IFNRCB
          WRITE(IFNRCB) DELT,PERTIM,TIME
          WRITE(IFNRCB) NFARMS
        ENDIF
C
C6A2----WRITE LIST OF CUMULATIVE FARM NET RECHARGE FOR EACH FARM
        DO NF=1,NFARMS
        FNRECH=0.D0         
          DO IR=1,NROW
          DO IC=1,NCOL
             IF(IFID(IC,IR).EQ.IFA(NF))    THEN
               FNRECH=FNRECH+FNRCH(IC,IR)
             ENDIF
          ENDDO
          ENDDO
          IF(IFNRCB.EQ.3) THEN
             WRITE(FMPOUT%UNIT(5)) KPER,KSTP,TIME,IFA(NF),FNRECH
          ELSEIF(IFNRCB.GT.3.AND.IBD.EQ.1) THEN
             WRITE(IFNRCB) IFA(NF), FNRECH    !Check why writing to IFNRCB
          ENDIF
        ENDDO
C
C
C6B-----CELL-BY-CELL NET RECHARGE FLOW RATES WILL BE SAVED AS 2-D ARRAY IN A BINARY FILE, IF
C       "COMPACT BUDGET" IS SPECIFIED IN OUTPUT CONTROL.
      IF(IBD.EQ.2) THEN
C
C6B1----WRITE ONE RECORD OF FLOW VALUES IF ONLY ONE LAYER
      IF(NLAY.EQ.1) THEN
        NOPT=1
      ELSE
C
C6B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS RECEIVE NET-RECHARGE.
C       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS FLOW VALUES).
        NOPT=2
      ENDIF
C     
      CALL UBDSV3(KSTP,KPER,TEXT,IFNRCB,BUFF,IRCH,NOPT,
     1            NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C
C7===== UPDATE VOLUMETRIC BUDGET FOR FARM NET RECHARGE ======================================================

C7A-----MOVE TOTAL FARM NET RECHARGE RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=SNGL(RATOUT)
      RIN=SNGL(RATIN)
      VBVL(4,MSUM)=ROUT
      VBVL(3,MSUM)=RIN
C
C7B-----ADD FARM NET RECHARGE FOR TIME STEP TO RECHARGE ACCUMULATOR IN VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
C
C7C-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS_OT.
      VBNM(MSUM)=TEXT
C
C7D-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C8===== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3INTLSTRD(ILIST,LDIM,NLIST,INPACK,IOUT)          
C-----VERSION 2 09/21/09 FMP3INTLSTRD
C     ******************************************************************
C     Read and print a multi-dimensional list of integer data
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER ILIST(LDIM,NLIST),LDIM,NLIST,INPACK,IOUT
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE,FNAME
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
      INTEGER NUNOPN,I,IN,ICLOSE,LLOC,ISTART,ISTOP,IL,ICNSTNT(LDIM),NC, !FORMERLY IMPLICIT INTEGER
     1 N,J,ICOL
      REAL R                                                            !FORMERLY IMPLICIT REAL
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1===== CHECK FOR CONSTANT(S), INTERNAL OR EXTERNAL DATA LIST ===============================================
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE                                               !READ FIRST LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CONSTANT') THEN
         DO IL=2,LDIM
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICNSTNT(IL),R,IOUT,IN)
         ENDDO
         DO NC=1,NLIST
         DO IL=1,LDIM
            IF(IL.EQ.1) ILIST(IL,NC)=NC
            IF(IL.GT.1) ILIST(IL,NC)=ICNSTNT(IL)
         ENDDO
         ENDDO
         GOTO 120
      ELSE IF(LINE(ISTART:ISTOP).EQ.'INTERNAL') THEN
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,111) IN
         ENDIF
  111    FORMAT(/,1X,'Reading list on unit',I5)
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,115) IN,FNAME
         ENDIF
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I5,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      END IF
C
C2===== READ INTEGER DATA LIST ============================================================================== 
      DO J=1,NLIST 
      ICOL=1                                                            !FIRST LINE HAS ALREADY BEEN READ 
      DO IL=1,LDIM
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,ILIST(IL,J),R,IOUT,IN)
      ENDDO
      IF(J.LE.NLIST-1) READ(IN,'(A)') LINE
      ENDDO
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C4===== RETURN ==============================================================================================
  120 RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3LSTRD(RLIST,LDIM,NLIST,INPACK,IOUT,ISCLOC1,ISCLOC2,
     +                                                            NREAD)!seb ADDED NREAD
C-----VERSION 2 09/21/09 FMP3LSTRD
C     ******************************************************************
C     Read and print a multi-dimensional list of real data, and
C     optionally read and apply a scale factor
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER LDIM,NLIST,INPACK,IOUT,ISCLOC1,ISCLOC2,NREAD
      DOUBLE PRECISION RLIST(LDIM,NLIST)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE,FNAME
      DATA NUNOPN/99/
      INTEGER NUNOPN,I,IN,ICLOSE,LLOC,ISTART,ISTOP,IL,N,ICOL,NC         !FORMERLY IMPLICIT INTEGER
      REAL R                                                            !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION CNSTNT(LDIM),SFAC                                !FORMERLY IMPLICIT REAL
C     ------------------------------------------------------------------      
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1===== CHECK FOR CONSTANT(S), INTERNAL OR EXTERNAL DATA LIST ===============================================
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE                                               !READ FIRST LINE
      SFAC=1.D0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CONSTANT') THEN
         DO IL=2,NREAD
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,CNSTNT(IL),IOUT,IN)
         ENDDO
         DO NC=1,NLIST
         DO IL=1,NREAD
            IF(IL.EQ.1) RLIST(IL,NC)=DBLE(NC)
            IF(IL.GT.1) RLIST(IL,NC)=CNSTNT(IL)
         ENDDO
         ENDDO
         GOTO 120 
      ELSE IF(LINE(ISTART:ISTOP).EQ.'INTERNAL') THEN
         READ(IN,'(A)') LINE  
      ELSE IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,111) IN
         ENDIF
  111    FORMAT(/,1X,'Reading list on unit',I5)
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,115) IN,FNAME
         ENDIF
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I5,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      END IF
C
C2===== CHECK FOR SCALE FACTOR ==============================================================================
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SFAC,IOUT,IN)
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,116) SFAC
           ENDIF
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.0.AND.ISCLOC2.EQ.0) THEN
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,*)'THE SCALE FACTOR DOES NOT APPLY TO ANY',
     1            ' FIELD'
              ENDIF
              STOP
           ELSEIF(ISCLOC1.EQ.ISCLOC2) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,113) ISCLOC1
              ENDIF
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,114) ISCLOC1,ISCLOC2
              ENDIF
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1           I2,'-',I2,')')
           END IF
         READ(IN,'(A)') LINE
      END IF
C
C3===== READ DOUBLE PRECISION DATA LIST AND MULTIPLY BY SCALE FACTOR IF AVAILABLE ===========================
C      
      DO NC=1,NLIST 
      ICOL=1                                                            !FIRST LINE HAS ALREADY BEEN READ
      DO IL=1,NREAD                                                     !DIMENSIONED BY LARGEST NUMBER OF COLUMNS IN ANY LIST  seb ONLY READ NREAD COLLUMNS
      CALL URWORDDP(LINE,ICOL,ISTART,ISTOP,4,I,R,RLIST(IL,NC),IOUT,IN)
      IF(IL.GE.ISCLOC1.AND.IL.LE.ISCLOC2)
     1 RLIST(IL,NC)=RLIST(IL,NC)*SFAC
      ENDDO
      IF(NC.LE.NLIST-1) READ(IN,'(A)') LINE
C
      ENDDO
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C4===== RETURN ==============================================================================================
  120 RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3DPLSTRD(DPRLIST,LDIM,NLIST,INPACK,IOUT,ISCLOC1,
     1                       ISCLOC2,NREAD)                             !seb ADDED NREAD WHICH REPRESENTS THE NUMBER OF COLLUMNS TO READ (NREAD<=LDIM)
C-----VERSION 2 09/21/09 FMP3DPLSTRD
C     ******************************************************************
C     Read and print a multi-dimensional list of double precision data, and
C     optionally read and apply a scale factor or an external list of scale factors
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER LDIM,NLIST,INPACK,IOUT,ISCLOC1,ISCLOC2,NREAD
      DOUBLE PRECISION DPRLIST(LDIM,NLIST)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE,FNAME,SFACNAME
      DATA NUNOPN/99/
      INTEGER I,NUNOPN,IN,ICLOSE,LLOC,ISTART,ISTOP,IL,NC,N,ISFACFL,     !FORMERLY IMPLICIT INTEGER
     1 LLOC2,INSFAC,ICOL
      REAL R                                                            !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION D, DPCNSTNT(LDIM)                                !seb CHANGED THROUGHOUT CODE LDIM TO NREAD FOR READING LOOPS
      DOUBLE PRECISION SLIST(2,LDIM),SFAC                               !FORMERLY IMPLICIT REAL
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'      
C     ------------------------------------------------------------------
C
C1===== CHECK FOR CONSTANT(S), INTERNAL OR EXTERNAL DATA LIST ===============================================
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE                                               !READ FIRST LINE
      SFAC=1.D0
      LLOC=1
      CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,1,I,R,D,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CONSTANT') THEN
         DO IL=2,NREAD
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,DPCNSTNT(IL),IOUT,IN)
         ENDDO
         DO NC=1,NLIST
         DO IL=1,NREAD
            IF(IL.EQ.1) DPRLIST(IL,NC)=NC
            IF(IL.GT.1) DPRLIST(IL,NC)=DPCNSTNT(IL)
         ENDDO
         ENDDO
         GOTO 121
      ELSE IF(LINE(ISTART:ISTOP).EQ.'INTERNAL') THEN
         READ(IN,'(A)') LINE 
      ELSE IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,2,I,R,D,IOUT,IN)
         IN=I
         IF(LSTCHK(3)) THEN
           FNAME=''
           INQUIRE(IN,NAME=FNAME)
           WRITE(IOUT,111) IN, TRIM(FNAME)
         ENDIF
  111    FORMAT(/,1X,'READING LIST ON UNIT',I5,' WITH FILE NAME: ',A)
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,0,N,R,D,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
!         IN=NUNOPN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,115) TRIM(FNAME)
         ENDIF
  115    FORMAT(1X,/1X,'OPENING FILE WITH FILENAME: ',A)
         OPEN(NEWUNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,125) IN
         ENDIF
         ICLOSE=1
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      END IF
C
C2===== CHECK FOR SCALE FACTOR OR EXTERNAL SCALE FACTOR LIST ================================================
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      ISFACFL=0
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
C
C2A-----CHECK FOR EXTERNAL LIST OF SCALE FACTORS AND READ NAME OF LIST
         LLOC2=LLOC                                                     !REMEMBER LOCATION AFTER READING 'SFAC'
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
         IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
           ISFACFL=1
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
           INSFAC=I
           IF(LSTCHK(3)) THEN
             FNAME=''
             INQUIRE(IN,NAME=FNAME)
             WRITE(IOUT,120) INSFAC,TRIM(FNAME)
           ENDIF
  120      FORMAT(1X,'READING SCALE FACTOR LIST ON UNIT',I5,
     +               ' ASSOCIATED WITH FILE: ',A)
         ELSEIF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
           ISFACFL=2
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
           SFACNAME=LINE(ISTART:ISTOP)
!           INSFAC=100
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,119)SFACNAME
           ENDIF
  119      FORMAT(1X,/1X,'OPENING SCALE FACTOR LIST WITH FILENAME:',A)
           !
           OPEN(NEWUNIT=INSFAC,FILE=SFACNAME,ACTION=ACTION(1))
           !
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,125) INSFAC
           ENDIF
  125      FORMAT(1X,'AND UNIT NUMBER: 'I4)
         ELSE
C
C2B-----CHECK FOR SCALE FACTOR AND READ IT
           LLOC=LLOC2                                                   !REWIND TO AFTER 'SFAC' TO CHECK FOR SCALE FACTOR SCALAR
           CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SFAC,IOUT,IN)
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,116) SFAC
           ENDIF
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.0.AND.ISCLOC2.EQ.0) THEN
             IF(LSTCHK(1)) THEN
               WRITE(IOUT,*)'THE SCALE FACTOR DOES NOT APPLY TO ANY',
     1            ' FIELD'
             ENDIF
             STOP
           ELSEIF(ISCLOC1.EQ.ISCLOC2) THEN
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,113) ISCLOC1
             ENDIF
  113        FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,114) ISCLOC1,ISCLOC2
             ENDIF
  114        FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1           I4,' -',I4,')')
           END IF
         ENDIF
C
C2C-----READ LIST OF SCALE FACTORS
         IF(ISFACFL.GT.0) THEN
           CALL FMP3LSTRD(SLIST,2,LDIM,INSFAC,IOUT,0,0,2)
           IF(ISFACFL.EQ.2) CLOSE(UNIT=INSFAC)
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,117)
           ENDIF
  117      FORMAT(1X,'FIELD     SCALE FACTOR')
           DO IL=1,NREAD
             IF(LSTCHK(3)) THEN
               WRITE(IOUT,118) IL, SLIST(2,IL)
             ENDIF
  118        FORMAT(1X,I5,1PG17.5)
           ENDDO
         ENDIF               
         READ(IN,'(A)') LINE
      END IF
C
C3===== READ DOUBLE PRECISION DATA LIST AND MULTIPLY BY SCALE FACTOR IF AVAILABLE ===========================
      DO NC=1,NLIST 
      ICOL=1                                                            !FIRST LINE HAS ALREADY BEEN READ
      DO IL=1,NREAD                                                     !DIMENSIONED BY LARGEST NUMBER OF COLUMNS IN ANY LIST seb CHANGED LDIM TO NREAD TO ONLY READ IN CORRECT NUMBER OF COLLUMNS
      CALL URWORDDP(LINE,ICOL,ISTART,ISTOP,4,I,R,DPRLIST(IL,NC),IOUT,IN)
      IF(ISFACFL.GT.0) THEN
      DPRLIST(IL,NC)=DPRLIST(IL,NC)*SLIST(2,IL)
      ELSEIF(ISFACFL.EQ.0.AND.(IL.GE.ISCLOC1.AND.IL.LE.ISCLOC2)) THEN
      DPRLIST(IL,NC)=DPRLIST(IL,NC)*SFAC
      ENDIF
      ENDDO       
      IF(NC.LE.NLIST-1) READ(IN,'(A)') LINE
      ENDDO
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C4===== RETURN ==============================================================================================         
121   RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3DPWDLSTRD(DPRLIST,WORD,LDIM,NLIST,INPACK,IOUT,
     1                         ISCLOC1,ISCLOC2)          
C-----VERSION 2 09/21/09 FMP3DPWDLSTRD
C     ******************************************************************
C     Read and print a multi-dimensional list of double precision data,and
C     optionally read and apply a scale factor,and
C     interrupt reading field if a word replaces numeric fields & read word, and 
C     read word at the end of each list row
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      CHARACTER*20 WORD(NLIST)
      INTEGER LDIM,NLIST,INPACK,IOUT,ISCLOC1,ISCLOC2
      DOUBLE PRECISION DPRLIST(LDIM,NLIST) 
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE,FNAME
      CHARACTER*20 CONSTWORD
      CHARACTER*1 W
      DATA NUNOPN/99/      
      INTEGER NUNOPN,I,IN,ICLOSE,LLOC,ISTART,ISTOP,IL,N,NC,ICOL,ICOLW   !FORMERLY IMPLICIT INTEGER
      REAL R                                                            !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION D, DPCNSTNT(NLIST)
      DOUBLE PRECISION SFAC                                             !FORMERLY IMPLICIT REAL
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
C1===== CHECK FOR CONSTANT(S), INTERNAL OR EXTERNAL DATA LIST ===============================================
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE                                               !READ FIRST LINE
      SFAC=1.D0
      LLOC=1
      CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,1,I,R,D,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'CONSTANT') THEN
         DO IL=2,LDIM
        CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,DPCNSTNT(IL),IOUT,IN)
         ENDDO
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,1,N,R,D,IOUT,IN)
         CONSTWORD=LINE(ISTART:ISTOP)
         DO NC=1,NLIST
         DO IL=1,LDIM
            IF(IL.EQ.1) DPRLIST(IL,NC)=NC
            IF(IL.GT.1) DPRLIST(IL,NC)=DPCNSTNT(IL)
         ENDDO
         WORD(NC)=CONSTWORD
         ENDDO
         GOTO 121
      ELSE IF(LINE(ISTART:ISTOP).EQ.'INTERNAL') THEN
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,2,I,R,D,IOUT,IN)
         IN=I
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,111) IN
         ENDIF
  111    FORMAT(/,1X,'Reading list on unit',I5)
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,0,N,R,D,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,115) IN,FNAME
         ENDIF
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT',I5,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE                                            !GO TO NEXT LINE TO READ DATA
      END IF
C
C2===== CHECK FOR SCALE FACTOR =============================================================================
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SFAC,IOUT,IN)
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,116) SFAC
           ENDIF
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.0.AND.ISCLOC2.EQ.0) THEN
             IF(LSTCHK(1)) THEN
               WRITE(IOUT,*)'THE SCALE FACTOR DOES NOT APPLY TO ANY',
     1           ' FIELD'
             ENDIF
              STOP
           ELSEIF(ISCLOC1.EQ.ISCLOC2) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,113) ISCLOC1
              ENDIF
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,114) ISCLOC1,ISCLOC2
              ENDIF
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1           I2,'-',I2,')')
           END IF
         READ(IN,'(A)') LINE
      END IF
C
C3===== READ LIST OF DOUBLE PRECISION DATA & A WORD, AND MULTIPLY BY SCALE FACTOR IF AVAILABLE ==============
C
C3A-----READ ONE LINE (ONE LIST ROW) OF DATA
      DO NC=1,NLIST 
      ICOL=1                                                            !FIRST LINE HAS ALREADY BEEN READ
      DO IL=1,LDIM                                                      !DIMENSIONED BY LARGEST NUMBER OF COLUMNS IN ANY LIST
      CALL URWORDDP(LINE,ICOL,ISTART,ISTOP,4,I,R,DPRLIST(IL,NC),IOUT,IN)
      IF(IL.GE.ISCLOC1.AND.IL.LE.ISCLOC2)
     1DPRLIST(IL,NC)=DPRLIST(IL,NC)*SFAC
C
C3B-----INTERRUPT READING FIELDS IF A WORD REPLACES NUMERIC FIELDS, AND READ WORD
C       (e.g. used for: replace coefficients of analytical functions with soil-type word)
      W=LINE(ICOL:ICOL)
123   if(il.lt.ldim.and.ichar(w).eq.32.or.ichar(w).eq.9) then           !32=blank, 9=tab
      if(icol.lt.LEN(LINE))then
          icol=icol+1
      else
          goto 124
      endif
      W=LINE(ICOL:ICOL)
      goto 123
      endif
124      IF(ICHAR(W).LT.45.OR.ICHAR(W).GT.57) THEN   
      CALL URWORDDP(LINE,ICOL,ISTART,ISTOP,1,N,R,D,IOUT,IN)
      WORD(NC)=LINE(ISTART:ISTOP) 
      GOTO 122
      ENDIF
      ENDDO
C
C3C-----READ WORD AT THE END OF EACH LIST ROW
      CALL URWORDDP(LINE,ICOLW,ISTART,ISTOP,1,N,R,D,IOUT,IN)
      WORD(NC)=LINE(ISTART:ISTOP)
122   IF(NC.LE.NLIST-1) READ(IN,'(A)') LINE
      ENDDO
C
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C4===== RETURN ==============================================================================================
121   RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3WELRD(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,
     1  IOUT,LABEL,CAUX,NCAUX,NAUX,NCOL,NROW,NLAY,ISCLOC1,ISCLOC2,ITERP,
     2  IUNITMNW1,IUNITMNW2,IUNITNWT)                         !added IUNITNWT for connection of NWT to Farm Wells
C-----VERSION 2 09/21/09 FMP3WELRD (modified ULSTRD)
C     ******************************************************************
C     Read and print a list of farm-well attributes.  
C     NAUX of the values in the list are optional -- auxiliary data.
C     ******************************************************************
      USE FMPMODULE, ONLY:PSIRAMPF,IUNITRAMPF,SATTHK,                   !added IUNITNWT for connection of NWT to Farm Wells  
     +                    MNW2NAM,AUXV                                  !seb added MNW2NAM FOR MNW2 LINK, AUXV TO HOLD FLAGS FOR AUXILARY VARIABLES
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      INTEGER NLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,IOUT,NCAUX,NAUX,NCOL,
     1  NROW,NLAY,ISCLOC1,ISCLOC2,ITERP,IUNITMNW1,IUNITNWT,
     2  IUNITMNW2
      DOUBLE PRECISION RLIST(LDIM,MXLIST) 
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE,FNAME
      DATA NUNOPN/99/
      INTEGER NUNOPN,IN,ICLOSE,LLOC,ISTART,ISTOP,I,N,NREAD2,NREAD1,II,  !FORMERLY IMPLICIT INTEGER
     1 JJ,IDUM,K,J,ILOC,NN,WID
      REAL R                                                            !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION SFAC                                             !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION DDUM                                             !TEMP STORAGE FOR DP READIN
C     ------------------------------------------------------------------
      INCLUDE 'openspec.inc'     
C     ------------------------------------------------------------------
C
      IF (NLIST.EQ.0) RETURN
C
C1===== Check for and decode EXTERNAL and SFAC records. ======================================================
      IN=INPACK
      ICLOSE=0
      READ(IN,'(A)') LINE
      SFAC=1.D0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'INTERNAL') THEN
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN=I
         IF(LSTCHK(3)) THEN
         IF(ITERP.EQ.1)  WRITE(IOUT,111) IN
         ENDIF
  111    FORMAT(/,1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME=LINE(ISTART:ISTOP)
         IN=NUNOPN
         IF(LSTCHK(3)) THEN
         IF(ITERP.EQ.1)  WRITE(IOUT,115) IN,FNAME
         ENDIF
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
         ICLOSE=1
         READ(IN,'(A)') LINE
      END IF
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORDDP(LINE,LLOC,ISTART,ISTOP,4,I,R,SFAC,IOUT,IN)
         IF(ITERP.EQ.1) THEN
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,116) SFAC
           ENDIF
  116      FORMAT(1X,'LIST SCALING FACTOR=',1PG12.5)
           IF(ISCLOC1.EQ.ISCLOC2) THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,113) ISCLOC1
              ENDIF
  113         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
           ELSE
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,114) ISCLOC1,ISCLOC2
              ENDIF
  114         FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',
     1           I2,'-',I2,')')
           END IF
         ENDIF
C
ccrth                                            !Add checking for use of 'Specify' Parameter in Farm Well file after SFAC to read PSIRAMPF and IUNITRAMPF values by rth
      ELSEIF(IUNITNWT.NE.0)THEN
       IF(LINE(ISTART:ISTOP).EQ.'SPECIFY') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMPF,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SATTHK,IOUT,IN)
ccrth        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMPF,R,IOUT,IN)
         IF(PSIRAMPF.LT.1.0E-5) PSIRAMPF=1.0E-5
         IF (SATTHK.LE.0.D0) SATTHK = 0.1
ccrth         IF ( IUNITRAMPF.EQ.0 ) IUNITRAMPF = IOUT
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,*)
         ENDIF
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,9) PSIRAMPF,SATTHK
         ENDIF
       ELSE
         BACKSPACE IN
         IF ( IUNITNWT.GT.0 )THEN
           IUNITRAMPF = IOUT
       IF(LSTCHK(3)) THEN
         WRITE(IOUT,*)' PHIRAMP WILL BE SET TO A DEFAULT VALUE OF 0.05'
       ENDIF
       IF(LSTCHK(3)) THEN
         WRITE(IOUT,*) ' WELLS WITH REDUCED PUMPING WILL BE '
     +                      ,'REPORTED TO THE MAIN LISTING FILE'
       ENDIF
         END IF
       END IF
         READ(IN,'(A)') LINE
      END IF
    9 FORMAT(1X,'PUMPING RATES FOR Single-Aquifer Farm Wells '/
     +    ' WILL BE REDUCED IF HEAD FALLS WITHIN THE INTERVAL '/
     +    ' PSIRAMPF TIMES THE CELL THICKNESS. THE VALUE SPECIFIED',/
     +    ' FOR PHISRAMPF IS ',E12.5,' WELLS WITH REDUCED PUMPING WILL'
     +    ' BE REDUCED IF THE SATURATED THICKNESS IS LESS THAN ',E12.5)
ccrth
C
C2==== Write a label for the list. ==========================================================================
      IF(ITERP.EQ.1) THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,'(1X)')
         CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
         ENDIF
      END IF
C
C3===== Read the list =======================================================================================
      NREAD2=LDIM-IAL
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
      DO 250 II=LSTBEG,N
C
C3A-----Read a line into the buffer.  (The first line has already been read
C       in order to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C3B-----Read the non-optional values from the line (in Farm Process: only Free Format).
      LLOC=1
      DO JJ=1,NREAD1
      CALL
     1 URWORDDP(LINE,LLOC,ISTART,ISTOP,4,IDUM,R,RLIST(JJ,II),IOUT,IN)   !modfication of ULSTRD
      ENDDO
      K=IDINT(RLIST(1,II))                                              !
      I=IDINT(RLIST(2,II))                                              !modfication of ULSTRD
      J=IDINT(RLIST(3,II))                                              !
      WID=IDINT(RLIST(4,II))
      DO ILOC=ISCLOC1,ISCLOC2
      RLIST(ILOC,II)=RLIST(ILOC,II)*SFAC
      ENDDO
C
C3C-----Read the optional MNW2 WELL Names to be Linked to   seb
      IF(RLIST(4,II).LT.0D0 .AND. IUNITMNW2.GT.0) THEN                  !FARM-WELL-ID STORED AS DOUBLE PRECISION
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,R,IOUT,IN)           !USING STANDARD UTIL FOR CONVERTING WORD TO UPPER CASEs
        MNW2NAM(II)=LINE(ISTART:ISTOP)
      ELSEIF(RLIST(4,II).GE.0D0 .AND. IUNITMNW2.GT.0) THEN 
        MNW2NAM(II)='NO-LINK'                                           !BLANK OUT UNUSED MNW2NAM SPACE FOR CLEAN PRINTING
      END IF      
C
C3C-----Read the optional values from the line
      IF(NAUX.GT.0) THEN
         RLIST(NREAD1+1:NREAD2,II)=0D0                                  !seb ZERO OUT PREVIOUSLY EXISTING AUX VALUES
         AUXV(:,II)=0                                                   !ORIGINAL STORAGE OF INTEGERS WAS IN: DO 210 JJ=NREAD1+1,NREAD2; RLIST(JJ,II), SUPERCEDED BY NEW VARIABLE AUXV
         DO 210 JJ=1,NAUX
         IDUM=0                                                         !seb IF THERE IS A FAILED AUX READ IT IS ASSUMED TO BE ZERO
         CALL 
     1   URWORDDP(LINE,LLOC,ISTART,ISTOP,2,IDUM,R,DDUM,-IOUT,IN)
         SELECT CASE (CAUX(JJ))                                         !seb STATIC ASSIGNMENT OF AUX VARIABLES TO FWLAUXORDER
          CASE ("QMAXRESET");AUXV(1,II)=IDUM
          CASE ("NOCIRNOQ"); AUXV(2,II)=IDUM
          CASE ("LGRGRID");  AUXV(3,II)=IDUM
          CASE  DEFAULT
            WRITE(*,*) "ERROR AUX VARIABLE "//TRIM(CAUX(JJ))
     +               //" NOT FOUND IN SOURCE CODE"
            WRITE(IOUT,*) "ERROR AUX VARIABLE "//TRIM(CAUX(JJ))
     +               //" NOT FOUND IN SOURCE CODE"
         END SELECT
210      CONTINUE
      END IF
C
C3D-----Write the values that were read.
      NN=II-LSTBEG+1
      IF(LSTCHK(3)) THEN
        IF(ITERP.EQ.1 .AND. IUNITMNW2.LE.0)   THEN                      ! seb ADD PRINT INFORMATION FOR MNW2 LINKED WELL NAME
          WRITE(IOUT,215) NN,(IDINT(RLIST(JJ,II)),JJ=1,5),RLIST(6,II),   !modfication of ULSTRD
     1                       (AUXV(JJ,II),JJ=1,NAUX)
        ELSEIF(ITERP.EQ.1 .AND. IUNITMNW2.GT.0)   THEN                  ! seb ADD PRINT INFORMATION FOR MNW2 LINKED WELL NAME
          WRITE(IOUT,216) NN,(IDINT(RLIST(JJ,II)),JJ=1,5),RLIST(6,II),   !modfication of ULSTRD
     1                       MNW2NAM(II),(AUXV(JJ,II),JJ=1,NAUX)
        END IF  
      ENDIF
215   FORMAT(1X,4I7,2I11,ES16.4,5(10x,I5))                              !modfication of ULSTRD
216   FORMAT(1X,4I7,2I11,ES16.4,2x,A,5(10x,I5)) 
C
C3E-----Check for illegal grid location
      IF(IUNITMNW1.EQ.0 .AND. IUNITMNW2.EQ.0) THEN
        IF(K.LT.1 .OR. K.GT.NLAY) THEN
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         ENDIF
         CALL USTOP(' ')
        END IF
      ELSEIF(IUNITMNW1.GT.0 .OR. IUNITMNW2.GT.0) THEN                   !seb added check FOR MNW2 
        IF((K.LT.1 .OR. K.GT.NLAY).AND. WID.GT.0) THEN                  !modfication of ULSTRD: ZERO LAYER INDICATES THAT LAYERS ARE DEFINED IN MNW PACKAGE
         IF(LSTCHK(1)) THEN
           WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         ENDIF
         CALL USTOP(' ')
        ELSEIF(WID.LT.0.AND.IUNITMNW1.GT.0)THEN                         !seb USE LAYER NUMBER AS FLAG FOR MNW1 LINK
            K=0
            RLIST(1,II)=0D0
        END IF
      ENDIF
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
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
C4===== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE FMP3WELPARRD(IN,PACK,IOUTU,PTYP,RLIST,LSTVL,LSTDIM,
     1           NREAD,MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX,
     2           IUNITMNW2)                                             !seb ADDED IUNITMNW2
C-----VERSION 2 09/21/09 FMP3WELPARRD (modified UPARLSTSUB)
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE FMPMODULE, ONLY:MNW2NAM,AUXV                                  !seb added FMPMODULE TO ACCESS MNW2NAM FOR MNW2 LINK
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      CHARACTER*(*) PACK,PTYP
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)      
      INTEGER IN,IOUTU,LSTVL,LSTDIM,NREAD,MXLST,NTOT,IPVL1,IPVL2,NCAUX,
     1 NAUX,IUNITMNW2          
      DOUBLE PRECISION RLIST(LSTVL,LSTDIM)      
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*700 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
      INTEGER IOUT,LLOC,ISTART,ISTOP,IDUM,IP,NLST,NUMINST,ILOC,NI,KI,I, !FORMERLY IMPLICIT INTEGER
     1 II,III,J,IPVL,IL,IR,IC,JJ
      REAL RDUM                                                         !FORMERLY IMPLICIT REAL
C     ------------------------------------------------------------------
C
C1------The LIST file unit is the absolute value of IOUTU.  
C1------Read the parameter name.
      IOUT = IABS(IOUTU)
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
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,81) PARNAM(IP),PARTYP(IP),PACK,PTYP
            ENDIF
   81       FORMAT(1X,'Parameter type conflict:',/
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
                WRITE(IOUT,1000)PACK,PARNAM(IP)
              ENDIF
 1000         FORMAT(/,1X,'Blank instance name in the ',A,
     1               ' file for parameter ',A)
              CALL USTOP(' ')
            ENDIF
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,1010) CTMP3
            ENDIF
 1010       FORMAT(3X,'Instance:  ',A)
            CALL UPCASE(CTMP3)
            DO 50 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 55
              ENDIF
   50       CONTINUE
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
            ENDIF
 1020       FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     1             A,'" for parameter ',A)
            CALL USTOP(' ')
   55       CONTINUE
          ENDIF
C
C4------Check that the parameter is not already active.
          IF (IACTIVE(IP).GT.0) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,1030) PARNAM(IP)
            ENDIF
 1030       FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     1          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     1          ' -- STOP EXECUTION (FMP3WELPARRD)')                    !modfication of UPARLSTSUB
            CALL USTOP(' ')
          ENDIF
C
C5------Set the active flag
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
            DO 85 J=1,NREAD
              RLIST(J,II)=RLIST(J,III)
   85       CONTINUE
            IF (IUNITMNW2.GT.0) THEN
              MNW2NAM(II)=MNW2NAM(III)                                  ! seb TRANSFER MNW2 WELL NAME TO FRONT THIS INCLUDES BOTH LINKED WELL NAMES AND EMPTPY STRINGS: ' '
            END IF
            AUXV(:,II)=AUXV(:,III)                                      ! seb TRANSFER AUXILARY VARIABLES FROM PARAMETERS
C
C8A-----Scale the RLIST values from IPVL1 to IPVL2 by the parameter
C8A-----value.  
            DO 86 IPVL=IPVL1,IPVL2
              RLIST(IPVL,II)=RLIST(IPVL,II)*DBLE(B(IP))                 !modfication of UPARLSTSUB
   86       CONTINUE
            IL=IDINT(RLIST(1,II))
            IR=IDINT(RLIST(2,II))
            IC=IDINT(RLIST(3,II))
            IF(LSTCHK(3)) THEN
              IF(IOUTU.GT.0 .AND. IUNITMNW2.LE.0)  THEN                 !modfication of UPARLSTSUB seb ADD PRINT FOR MNW2 LINKED PARAMETERS
                 WRITE(IOUT,88) II,(IDINT(RLIST(JJ,II)),JJ=1,5),
     +                 RLIST(6,II),(AUXV(JJ,II),JJ=1,NAUX)
              ELSEIF(IOUTU.GT.0 .AND. IUNITMNW2.GT.0) THEN
                 WRITE(IOUT,89) II,(IDINT(RLIST(JJ,II)),JJ=1,5),
     +                 RLIST(6,II),MNW2NAM(II),(AUXV(JJ,II),JJ=1,NAUX)
              END IF
            ENDIF
   88       FORMAT(1X,4I7,2I11,ES16.4,5(10x,I5))                           !modfication of UPARLSTSUB
   89       FORMAT(1X,4I7,2I11,ES16.4,2x,A,5(10x,I5))
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
C10==== END ==============================================================================================
      END SUBROUTINE
C
C
      SUBROUTINE UPARLSTALPRTOCH(IN,IOUT,LINE,NP,MXL,ILGR,IGRID)
C     ******************************************************************
C     Setup list parameter definition for a package
C     And allow reading a flag indicating the use of parent list entries
C     For the child model list
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE FMPMODULE, ONLY: FMPDAT
      CHARACTER*(*) LINE
C     ------------------------------------------------------------------
C
C1------Decode PARAMETER definitions if they exist
      NP=0
      MXL=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
         IF(NP.LT.0) NP=0
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
         IF(MXL.LT.0) MXL=0
         IF(ILGR.NE.0.AND.IGRID.GT.1) THEN
           CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
           IF(LINE(ISTART:ISTOP).EQ.'P') THEN
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,30) NP,MXL,FMPDAT(1)%MXPFW
              ENDIF
   30         FORMAT(1X,I15,' Named Parameters     ',/,1x,I15,
     1        ' Child Model Well List entries',/,1x,I15,
     2        ' Parent Model Well List entries',
     3        ' used at Maximum for Child Model Wells'  )
              MXL=MXL+FMPDAT(1)%MXPFW
           ELSE
              IF(LSTCHK(3)) THEN
                WRITE(IOUT,31) NP,MXL
              ENDIF
           ENDIF
         ELSE
           IF(LSTCHK(3)) THEN
             WRITE(IOUT,31) NP,MXL         
           ENDIF
   31      FORMAT(1X,I15,' Named Parameters     ',I15,' List entries')
         ENDIF
         READ(IN,'(A)') LINE
      ELSE
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,'(A)') ' No named parameters'
         ENDIF
      END IF
C
C2------Return.
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE ACREAGE(NCELL,QMXF,ADSW,NRDE,NCON,NOPV,NLSER,
     1NLGER,NLEQR,NROWA,NCOLA,GWSUM,SWSUM,NRSUM,EF,NF,KPER,KSTP)
C-----VERSION 2 10/15/2009 ACREAGE
C     *****************************************************************************************
C     Reset simplex matrix for current iteration.
C     Initialize initial tableau matrix a (at least ncon+2 x nopv+1),
C     where ncon is the number of constraints, and nopv is the number of optimization variables.
C     Both ncon and nopv are already defined in FMP3FM.
C
C     Enter ncon=number of constraints = 3+#cells
C     The number of constraints equals 3 resource constraints for the entire farm (3 equations)
C     plus i area constraints (with i = 1,2,..., number of cells per farm):
C     �   eq. 1: resource function for groundwater irrigated areas only:          Sum(qi*Xigw) <= QMAXF*EF,
C     �   eq. 2: resource function for (semi-) routed sw irrigated areas only:    Sum(qi*Xisw) <= DELact*EF,
C     �   eq. 3: resource function for non-routed sw irrigated areas only:        Sum(qi*Xinr) <= NRD*EF,
C     �   number of cells equations: Xigw + Xisw  >= area-max for each cell.
C
C     Enter nopv=number of optimization-variables =3*#cells
C     The number of optimization-variables nopv equals 3 times the number of farm cells
C     (# of gw-irrigated cells + # of sw-irrigated cells + # of nr-irrigated cells)
C     
C     Enter nlser=number of LHS<=RHS, nlger=number of LHS>=RHS, nleqr=number of LHS=RHS
C     
C     Initialize row 1 = objective function (max Z = Sum[Xigw*Eigw + Xisw*Eisw + Xinr*Eisw],
C     with:
C         �   Eigw    = economic value of groundwater irrigated areas [$/L2] 
C                     = GW-Profit per farm cell
C         �   Eisw    = economic value of surface-water irrigated areas [$/L2]
C                     = profit of (semi-)routed SW per farm cell
C         �   Einr    = economic value of non-routed surface-water irrigated areas [$/L2]
C                     = profit of non-routed SW per farm cell
C
C     Initialize row 2,3,4: resources constraints [gw-, (semi-)routed sw, and non-routed sw].
C
C     Initialize rows 5 and following (in total i times): areas <= a maximum area.
C
C     Call LINOPT subroutine to solve.
C
C     Calculate total reduction factors for each cell# and
C     percentages of groundwater and surface-water irrigation.
C
C     Calculate and print optimized groundwater- and surface-water irrigation-flow rate per farm.
C
C     Print notice, whether the optimized and profitable amount of groundwater or surface-water
C     falls behind the available well capacity, or the available delivery from (semi-)routed
C     surface-water, or the available non-routed delivery.
C     
C     ******************************************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,    ONLY:IOUT,NCOL,NROW
      USE FMPMODULE, ONLY:OPT,IOPFL,ICID,FMPOUT
      USE FMPBLK,    ONLY:FPS
      USE CVT2STR,   ONLY: NUM2STR
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER NCELL,NCON,NOPV,NLSER,NLGER,NLEQR,NROWA,NCOLA,NF,KPER,KSTP
      DOUBLE PRECISION QMXF,ADSW,NRDE,GWSUM,SWSUM,NRSUM,EF
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------            
      CHARACTER*10 fmt_number
      CHARACTER*20 fmt_string
      INTEGER ISOL,ILHV(NCON),IRHV(NOPV),IX,IOUTFILE,I,J
      DOUBLE PRECISION A(NROWA,NCOLA),VALU(NOPV)
C     ------------------------------------------------------------------
C
C1===== PRINT ORIGINAL FLOW RATES / RESOURCE CONSTRAINTS ====================================================
      IF(IOPFL.EQ.-3.OR.IOPFL.EQ.-2.OR.IOPFL.EQ.3.OR.IOPFL.EQ.2) THEN
C        IF(IOPFL.EQ.-3.OR.IOPFL.EQ.-2) IOUTFILE=IOUT                   !Opened in AR seb
C        IF(IOPFL.EQ.3.OR.IOPFL.EQ.2) THEN
C          OPEN(UNIT=1006, FILE='ACR_OPT.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1006
C        ENDIF
        WRITE(FMPOUT%UNIT(6),30) KPER,KSTP,NF
   30   format(//,'ORIGINAL & OPTIMIZED FLOWRATES',7X,
     1  'PERIOD: ',I4,',    STEP:',I4,',    FARM #:',I9,/,84('-'))
C      WRITE(FMPOUT%UNIT(6),31) NCELL,EF,QMXF/EF,ADSW/EF,NRDE/EF,QMXF,ADSW,NRDE
C   31 FORMAT(1X,/,'NO. OF CELLS IN FARM: ',I5,1X,'EFF: 'F8.2,1X,//,1X,
C     1'ORIGINAL AVAILABLE SUPPLY>>      QMAXF:      ',ES9.3,
C     2'  R-SWD-INI:    ',ES9.3,'  NR-SWD-INI:    ',ES9.3,/,1X,
C     3'ORIGINAL RESOURCE CONSTRAINTS>>  QMAXF*EF:   ',ES9.3,
C     4'  R-SWD-INI*EF: ',ES9.3,'  NR-SWD-INI*EF: ',ES9.3)
      WRITE(FMPOUT%UNIT(6),31) NCELL,QMXF,ADSW,NRDE
   31 FORMAT(1X,/,'NO. OF CELLS IN FARM: ',I5,1X,//,1X,
     1'ORIGINAL AVAILABLE SUPPLY>>      QMAXF:      ',ES9.3,
     2'  R-SWD-INI:    ',ES9.3,'  NR-SWD-INI:    ',ES9.3)
      ENDIF
C
C2===== RESET ===============================================================================================
C
C2A-----AVOID BAD INPUT TABLEAU
      IF(QMXF.LT.FPS) QMXF=0D0
      IF(ADSW.LT.FPS) ADSW=0D0    
      IF(NRDE.LT.FPS) NRDE=0D0
C
C2B-----RESET SIMPLEX MATRIX FOR CURRENT ITERATION.
      DO I=1,NROWA
      DO J=1,NCOLA
      A(I,J)=0.0D0
      ENDDO
      ENDDO
C
C3===== INITIALIZE INITIAL TABLEAU MATRIX A (AT LEAST NCON+1 X NOPV+1) ======================================
      DO J=1,NCELL
C
C3A-----INITIALIZE ROW 1 = OBJECTIVE FUNCTION (MAX Z = Sum[Xigw*Eigw + Xisw*Eisw + Xinr*Eisw])
C
C3A1-----ECONOMIC VALUE OF GROUNDWATER IRRIGATED AREAS [$/L2]      
         A(1,J+1)=      OPT(1,J)
C3A2-----ECONOMIC VALUE OF SURFACE-WATER IRRIGATED AREAS [$/L2]   
         A(1,J+1+NCELL)=OPT(2,J)
C3A3-----ECONOMIC VALUE OF NON-ROUTED SURFACE-WATER IRRIGATED AREAS [$/L2]    
         A(1,J+1+2*NCELL)=OPT(3,J)
C
C3B-----INITIALIZE ROW2 & ROW3 & ROW4: GROUNDWATER- & SURFACEW.- & NON-ROUTED-SW.- RESOURCES CONSTRAINT
         A(2,J+1)=        -OPT(4,J)
         A(3,J+1+NCELL)=  -OPT(4,J)
         A(4,J+1+2*NCELL)=-OPT(4,J)
C
C3C-----INITIALIZE ROW5 AND FOLLOWING ROWS: AREAS <= A MAXIMUM AREA      
         A(J+4,1)=OPT(5,J)
         A(J+4,J+1)=-1
         A(J+4,J+1+NCELL)=-1
         A(J+4,J+1+2*NCELL)=-1
      ENDDO
      A(2,1)=QMXF
      A(3,1)=ADSW
      A(4,1)=NRDE
C
C3D-----PRINT TABLEAU MATRIX A
      IF(IOPFL.EQ.-4.OR.IOPFL.EQ.4) THEN
C        IF(IOPFL.EQ.-4) IOUTFILE=IOUT                                  !File opened in AR seb
C        IF(IOPFL.EQ.4) THEN
C          OPEN(UNIT=1006, FILE='ACR_OPT.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1006
C        ENDIF
        WRITE(FMPOUT%UNIT(6),40) KPER,KSTP,NF
   40   FORMAT(1X,/,1X,'ACREAGE OPTIMIZATION - TABLEAU MATRIX:',7X,
     1  'PERIOD: ',I4,',    STEP:',I4,',    FARM #:',I9,/)
!      write(fmt_number,'(i10)')NCELL*3+1
      fmt_string='('//NUM2STR(NCELL*3+1)//'G10.4)'                      !REMOVED TRIM(ADJUSTL(fmt_number))
      DO I=1,NCELL+4
      WRITE(FMPOUT%UNIT(6),FMT=TRIM(fmt_string)) (A(I,J),J=1,NCELL*3+1) 
      ENDDO
      ENDIF
C
C4===== SOLVE FOR OPTIMIZED ACREAGE OF EACH CELL ===========================================================
      CALL LINOPT(A,NCON,NOPV,NROWA,NCOLA,NLSER,NLGER,NLEQR,
     1ISOL,IRHV,ILHV)
C
C5===== CALCULATE AND PRINT FRACTIONS OF OPTIMIZED CELL AREAS ==============================================
      DO I=1,NCON
      IF(ILHV(I).LE.NOPV) THEN
      IX=ILHV(I)
      VALU(IX)=A(I+1,1)
      ENDIF
      ENDDO
      DO J=1,NOPV
      IF(IRHV(J).LE.NOPV) THEN
      IX=IRHV(J)
      VALU(IX)=0.0D0                                                   !IF VALU IS REAL THEN 0.
      ENDIF
      ENDDO
C
C5A-----CALCULATE TOTAL REDUCTION FACTORS FOR EACH CELL# AND PERCENTAGES OF GW AND SW
      DO J=1,NCELL
      OPT(8,J)=(VALU(J)+VALU(J+NCELL)+VALU(J+2*NCELL))/OPT(5,J)
      OPT(9,J)=0.0D0
      OPT(10,J)=0.0D0
      OPT(11,J)=0.0D0
      IF(VALU(J).NE.0.0D0 .OR. VALU(J+NCELL).NE.0.0D0
     1                     .OR. VALU(J+2*NCELL).NE.0.0D0) THEN         !IF VALU IS REAL THEN 0.
      OPT(9,J) =VALU(J)/
     1         (VALU(J)+VALU(J+NCELL)+VALU(J+2*NCELL))
      OPT(10,J)=VALU(J+NCELL)/
     1         (VALU(J)+VALU(J+NCELL)+VALU(J+2*NCELL))
      OPT(11,J)=VALU(J+2*NCELL)/
     1         (VALU(J)+VALU(J+NCELL)+VALU(J+2*NCELL))
      ENDIF
C
      IF(IOPFL.EQ.-3.OR.IOPFL.EQ.3) THEN
C        IF(IOPFL.EQ.-3) IOUTFILE=IOUT                                  !File Opened in AR seb
C        IF(IOPFL.EQ.3) THEN
C          OPEN(UNIT=1006, FILE='ACR_OPT.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1006
C        ENDIF
      if(j.eq.1) then
      write(FMPOUT%UNIT(6),
     1     '(1X,/,"FRACTIONS OF OPTIMIZED CELL AREAS:",/)')
      write(FMPOUT%UNIT(6),60)
   60 FORMAT(22X,'A-tot-opt/  A-gw-opt /  A-sw-opt /  A-nr-opt /',/,
     1'  Row  Col  Crop-ID',3X,
     2           'A-tot-max   A-tot-opt   A-tot-opt   A-tot-opt',/)   
      endif
      IF(LSTCHK(3)) THEN
      WRITE(FMPOUT%UNIT(6),61)
     1IDINT(opt(7,j)),IDINT(opt(6,j)),ICID(IDINT(OPT(6,J)),
     2IDINT(OPT(7,J))),opt(8,j),opt(9,j),opt(10,j),opt(11,j)
      ENDIF
   61 FORMAT(2I5,I9,4F12.7)
      ENDIF
      enddo
C
C5B-----CALCULATE OPTIMIZED GROUNDWATER-, SURFACE-WATER-, AND NON-ROUTED-SW- 
C       IRRIGATION-FLOWRATE PER FARM
      GWSUM=0.0D0
      SWSUM=0.0D0
      NRSUM=0.0D0
      DO J=1,NCELL
      GWSUM=GWSUM+OPT(4,J)*VALU(J)
      SWSUM=SWSUM+OPT(4,J)*VALU(J+NCELL)
      NRSUM=NRSUM+OPT(4,J)*VALU(J+2*NCELL)
      IF(GWSUM.LT.0.0D0) GWSUM=0.0D0                                    !THIS IS JUST NEEDED TO PRINT 0 CORRECTLY,
      IF(SWSUM.LT.0.0D0) SWSUM=0.0D0                                    !BECAUSE FORTRAN PRINTS INACCURATELY VERY SMALL NEG. NUMBERS, 
      IF(NRSUM.LT.0.0D0) NRSUM=0.0D0                                    !WHICH HOWEVER ARE STILL NUMERICALLY 0!
      IF(GWSUM.GT.QMXF) GWSUM=QMXF                                      !ALTHOUGH THE RESULTING GSWUM,SWSUM,NRSUM SHOULD BE ALWAYS < 
      IF(SWSUM.GT.ADSW) SWSUM=ADSW                                      !QMAXF,ADSW,NRDE, FOR VERY SMALL QMAXF,ADSW,NRDE CLOSE TO ZERO,
      IF(NRSUM.GT.NRDE) NRSUM=NRDE                                      !GWSUM,SWSUM,NRSUM CAN BE INACCURATELY SLIGHTLY HIHGER!
      ENDDO
C
C6===== PRINT OPTIMIZED RESOURCE FLOW RATES ===============================================================
C
C6A-----PRINT FLOW RATES / RESOURCE CONSTRAINTS
      IF(IOPFL.EQ.-3.OR.IOPFL.EQ.-2.OR.IOPFL.EQ.3.OR.IOPFL.EQ.2) THEN
C        IF(IOPFL.EQ.-3.OR.IOPFL.EQ.-2) IOUTFILE=IOUT                   !File Opened in AR seb
C        IF(IOPFL.EQ.3.OR.IOPFL.EQ.2) THEN
C          OPEN(UNIT=1006, FILE='ACR_OPT.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1006
C        ENDIF
C      WRITE(FMPOUT%UNIT(6),3) GWSUM,SWSUM,NRSUM,GWSUM/EF,SWSUM/EF,NRSUM/EF
C    3 FORMAT(1X,/,1X,
C     1'OPTIMIZED RESOURCE CONSTRAINTS>> GW-IRR-DEM: ',ES9.3,
C     2'  SW-IRR-DEM:   ',ES9.3,'  NR-IRR-DEM:    ',ES9.3,/,1X,
C     3'OPTIMIZED FINAL DEMAND>>         QREQ-FIN:   ',ES9.3,
C     4'  R-SWD-FIN:    ',ES9.3,'  NR-SWD-FIN:    ',ES9.3)
      WRITE(FMPOUT%UNIT(6),3) GWSUM,SWSUM,NRSUM
    3 FORMAT(1X,/,1X,
     1'OPTIMIZED FINAL DEMAND>>         QREQ-FIN:   ',ES9.3,
     2'  R-SWD-FIN:    ',ES9.3,'  NR-SWD-FIN:    ',ES9.3)     
C 
C6B-----NOTIFY, IF GWSUM OR SWSUM OR NRSUM IS SMALLER THAN THE AVAILABLE QMXF OR ADSW OR NRDE, 
      IF(GWSUM.LT.QMXF*0.995.OR.SWSUM.LT.ADSW*0.995.
     1OR.NRSUM.LT.NRDE*0.995) THEN
      WRITE(FMPOUT%UNIT(6),70)
   70 FORMAT(/,1X,
     1'RESOURCES THAT ARE AVAILABLE AT THE RATE OF A CONSTRAINED SUPPLY'
     2,' BUT ARE NOT (FULLY) PROFITABLE',/,1X,'ARE ONLY SUPPLIED TO THE'
     3,' FARM AT THE RATE OF THE OPTIMIZED DEMAND:',//
     4,'     OPTIMIZED DEMAND          < CONSTRAINED SUPPLY',/
     5,'     ----------------            ------------------')
      ENDIF
      IF(GWSUM.LT.QMXF*0.995)
     1WRITE(FMPOUT%UNIT(6),*)
     2'==> GROUNDWATER PUMPING REQ.  < CUMULATIVE MAXIMUM CAPACITY'
      IF(SWSUM.LT.ADSW*0.995)
     1WRITE(FMPOUT%UNIT(6),*)
     2'==> ROUTED SURFACE-WATER REQ. < AVAILABLE ROUTED SW DELIVERY'
      IF(NRSUM.LT.NRDE*0.995)
     1WRITE(FMPOUT%UNIT(6),*)
     2'==> NON-ROUTED SW. REQ.       < AVAILABLE NON-ROUTED SW DELIVERY'
      WRITE(FMPOUT%UNIT(6),'(/)')
      ENDIF
C
C7===== RETURN =============================================================================================
      RETURN
      END SUBROUTINE
C
C     
      SUBROUTINE LINOPT(A,NCON,NOPV,NROWA,NCOLA,NLSER,NLGER,NLEQR,
     1  ISOL,IRHV,ILHV)
      USE GLOBAL, ONLY:IOUT
      USE FMPBLK, ONLY:OPS
C     ******************************************************************
C     PERFORM SIMPLEX METHOD OF LINEAR OPTIMIZATION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER NCON,NOPV,NROWA,NCOLA,NLSER,NLGER,NLEQR,ISOL,IRHV(NOPV),
     1 ILHV(NCON)
      DOUBLE PRECISION A(NROWA,NCOLA)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      INTEGER I,IL,ILEQR,ILEQRFL,L,LH,LP,LST1(NOPV),LST2(NCON),
     1 LST3(NCON),NLSEGER,NLST1,NLST2
      DOUBLE PRECISION COEFMAX,AUXF
C     ------------------------------------------------------------------
C
      IF(NCON.NE.NLSER+NLGER+NLEQR) THEN
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,*)
     1'NUMBER OF CONSTRAINTS NOT EQUAL TO LHS<=RHS + LHS>=RHS + LHS=RHS'
      ENDIF
      STOP
      ENDIF
      NLST1=NOPV
      DO L=1,NOPV
        LST1(L)=L
        IRHV(L)=L
      ENDDO
      NLST2=NCON
      DO I=1,NCON
        IF(A(I+1,1).LT.0.0D0) THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,*) A(I+1,1),'BAD INPUT TABLEAU IN LINOPT'
        ENDIF
        STOP
        ENDIF
        LST2(I)=I
        ILHV(I)=NOPV+I
      ENDDO
      DO I=1,NLGER
        LST3(I)=1
      ENDDO
      ILEQRFL=0
      IF(NLGER+NLEQR.EQ.0) GOTO 5
      ILEQRFL=1
      DO L=1,NOPV+1
        AUXF=0.0D0
        DO I=NLSER+1,NCON
          AUXF=AUXF+A(I+1,L)
        ENDDO
        A(NCON+2,L)=-AUXF
      ENDDO
    3 CALL OPT1(A,NROWA,NCOLA,NCON+1,LST1,NLST1,0,LP,COEFMAX)
      IF(COEFMAX.LE.OPS.AND.A(NCON+2,1).LT.-OPS)THEN
        ISOL=-1
        RETURN
      ELSE IF(COEFMAX.LE.OPS.AND.A(NCON+2,1).LE.OPS)THEN
        NLSEGER=NLSER+NLGER+1
        IF(NLSEGER.LE.NCON)THEN
          DO ILEQR=NLSEGER,NCON
            IF(ILHV(ILEQR).EQ.ILEQR+NOPV)THEN
              CALL OPT1(A,NROWA,NCOLA,ILEQR,LST1,NLST1,1,LP,COEFMAX)
              IF(COEFMAX.GT.0.0D0) GOTO 1
            ENDIF
          ENDDO
        ENDIF
        ILEQRFL=0
        NLSEGER=NLSEGER-1
        IF(NLSER+1.GT.NLSEGER) GOTO 5
        DO I=NLSER+1,NLSEGER
          IF(LST3(I-NLSER).EQ.1)THEN
            DO L=1,NOPV+1
              A(I+1,L)=-A(I+1,L)
            ENDDO
          ENDIF
        ENDDO
        GOTO 5
      ENDIF
      CALL OPT2(A,NOPV,NROWA,NCOLA,LST2,NLST2,ILEQR,LP)
      IF(ILEQR.EQ.0)THEN
        ISOL=-1
        RETURN
      ENDIF
    1 CALL OPT3(A,NROWA,NCOLA,NCON+1,NOPV,ILEQR,LP)
      IF(ILHV(ILEQR).GE.NOPV+NLSER+NLGER+1)THEN
        DO L=1,NLST1
          IF(LST1(L).EQ.LP) GOTO 2
        ENDDO
    2   NLST1=NLST1-1
        DO IL=L,NLST1
          LST1(IL)=LST1(IL+1)
        ENDDO
      ELSE
        IF(ILHV(ILEQR).LT.NOPV+NLSER+1) GOTO 4
        LH=ILHV(ILEQR)-NLSER-NOPV
        IF(LST3(LH).EQ.0) GOTO 4
        LST3(LH)=0
      ENDIF
      A(NCON+2,LP+1)=A(NCON+2,LP+1)+1.D0
      DO I=1,NCON+2
        A(I,LP+1)=-A(I,LP+1)
      ENDDO
    4 IL=IRHV(LP)
      IRHV(LP)=ILHV(ILEQR)
      ILHV(ILEQR)=IL
      IF(ILEQRFL.NE.0) GOTO 3
    5 CALL OPT1(A,NROWA,NCOLA,0,LST1,NLST1,0,LP,COEFMAX)
      IF(COEFMAX.LE.0.0D0)THEN
        ISOL=0
        RETURN
      ENDIF
      CALL OPT2(A,NOPV,NROWA,NCOLA,LST2,NLST2,ILEQR,LP)
      IF(ILEQR.EQ.0)THEN
        ISOL=1
        RETURN
      ENDIF
      CALL OPT3(A,NROWA,NCOLA,NCON,NOPV,ILEQR,LP)
      GOTO 4
C===== END ==============================================================================================       
      END SUBROUTINE
C
C
      SUBROUTINE OPT1(A,NROWA,NCOLA,MROW,LIST,NLIST,IABSF,LP,COEFMAX)
C     ******************************************************************
C     PERFORM SIMPLEX METHOD OF LINEAR OPTIMIZATION;
C     SUBTASK: DETERMINE MAXIMUM COEFFICIENT OF THE ROW BELOW ROW "MROW"
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER NROWA,NCOLA,MROW,LIST(NCOLA),NLIST,IABSF,LP
      DOUBLE PRECISION A(NROWA,NCOLA),COEFMAX
C     ------------------------------------------------------------------      
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------      
      INTEGER L
      DOUBLE PRECISION TEST
C     ------------------------------------------------------------------ 
      LP=LIST(1)
      COEFMAX=A(MROW+1,LP+1)
      DO L=2,NLIST
        IF(IABSF.EQ.0) THEN
          TEST=A(MROW+1,LIST(L)+1)-COEFMAX
        ELSE
          TEST=DABS(A(MROW+1,LIST(L)+1))-DABS(COEFMAX)
        ENDIF
        IF(TEST.GT.0.0D0) THEN
          COEFMAX=A(MROW+1,LIST(L)+1)
          LP=LIST(L)
        ENDIF
      ENDDO
C===== RETURN ============================================================================================== 
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE OPT2(A,NOPV,NROWA,NCOLA,LST2,NLST2,ILEQR,LP)
      USE FMPBLK, ONLY:OPS
C     ******************************************************************
C     PERFORM SIMPLEX METHOD OF LINEAR OPTIMIZATION;
C     SUBTASK: FIND PIVOT ELEMENT
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER NOPV,NROWA,NCOLA,LST2(NROWA),NLST2,ILEQR,LP
      DOUBLE PRECISION A(NROWA,NCOLA)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      INTEGER I,IL,L
      DOUBLE PRECISION X1,X,X0,XP
C     ------------------------------------------------------------------
      ILEQR=0
      DO I=1,NLST2
        IF(A(LST2(I)+1,LP+1).LT.-OPS) GOTO 1
      ENDDO
      RETURN
1     X1=-A(LST2(I)+1,1)/A(LST2(I)+1,LP+1)
      ILEQR=LST2(I)
      DO I=I+1,NLST2
        IL=LST2(I)
        IF(A(IL+1,LP+1).LT.-OPS) THEN
          X=-A(IL+1,1)/A(IL+1,LP+1)
          IF(X.LT.X1) THEN
            X1=X
            ILEQR=IL
          ELSE IF (X.EQ.X1) THEN
            DO L=1,NOPV
              X0=-A(IL+1,L+1)/A(IL+1,LP+1)
              XP=-A(ILEQR+1,L+1)/A(ILEQR+1,LP+1)
              IF(X0.NE.XP) GOTO 2
            ENDDO
2           IF(X0.LT.XP) ILEQR=IL
          ENDIF
        ENDIF
      ENDDO
C===== RETURN ==============================================================================================       
      RETURN
      END SUBROUTINE
C 
C
      SUBROUTINE OPT3(A,NROWA,NCOLA,ICON,KOPV,ILEQR,LP)
C     ******************************************************************
C     PERFORM SIMPLEX METHOD OF LINEAR OPTIMIZATION;
C     SUBTASK: EXCHANGE LEFT-HAND VARIABLE FOR A RIGHT-HAND VARIABLE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER NROWA,NCOLA,ICON,KOPV,ILEQR,LP
      DOUBLE PRECISION A(NROWA,NCOLA)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      INTEGER I,K
      DOUBLE PRECISION PIV
C     ------------------------------------------------------------------
      PIV=1.D0/A(ILEQR+1,LP+1)
      DO I=1,ICON+1
        IF(I-1.NE.ILEQR)THEN
          A(I,LP+1)=A(I,LP+1)*PIV
          DO K=1,KOPV+1
            IF(K-1.NE.LP)THEN
              A(I,K)=A(I,K)-A(ILEQR+1,K)*A(I,LP+1)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DO K=1,KOPV+1
        IF(K-1.NE.LP)A(ILEQR+1,K)=-A(ILEQR+1,K)*PIV
      ENDDO
      A(ILEQR+1,LP+1)=PIV
C===== RETURN ============================================================================================== 
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE PRIOR(RDEL,FLOWIN,L,NF,KPER,KSTP)
C-----VERSION 2 10/15/2009 PRIOR
      USE FMPMODULE,    ONLY:NFARMS,SFRADD,IFCRID,RDR,FLOWINMIN,QSTRMIN,
     1                       NFSEG,IRDRFL,PCLOSE,IPAPFL,ISTARTFL,FMPOUT
      USE FMPBLK,       ONLY:AR,AC
      USE GLOBAL,       ONLY:IOUT
      USE GWFSFRMODULE, ONLY:STRM,ISTRM,NSTRM,SEG,DVRSFLW,NSEGDIM,
     1                       IDIVAR,SGOTFLW
C     ******************************************************************
C     PRIOR APPROPRIATION: SIMULATION OF WATER-RIGHTS DEPENDENT DELIVERY TO
C     FARM HEAD-GATE REACHES & ASSOCIATED CANAL DIVERSIONS FROM THE RIVER.
C     ------------------------------------------------------------------
C     - UPDATE POTENTIAL AND ACTUAL DIVERSION INTO CURRENT CANAL SEGMENT;
C     - SOLVE FOR REACH DELIVERY TO FARM & FOR ACTUAL DIVERSION INTO CANAL;
C     - EVALUATE MINIMUM CANAL STREAMFLOW AT JUNIOR FARM;
C     - EVALUATE MINIMUM RIVER STREAMFLOW AT DIVERSION INTO UPSTREAM CANAL.
C     ------------------------------------------------------------------
C     SUBROUTINE "PRIOR" APPLIES TO:
C     A CERTAIN FARM (NF),
C     A CERTAIN FARM HEAD-GATE REACH (L), AND
C     A CERTAIN SEGMENT (ISTRM(4,L) ASSOCIATED WITH THAT REACH
C     ******************************************************************
C        SPECIFICATION:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER L,NF,KPER,KSTP
      DOUBLE PRECISION RDEL,FLOWIN
C     ------------------------------------------------------------------
C        COMMON BLOCKS:
C     ------------------------------------------------------------------
      COMMON ITER, ITER2
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER*31 ACTION(3)
      CHARACTER*15 STAGE(3)
      DATA ACTION(1) /'EXIT & APPLY NEW PDIV RATES    '/
      DATA ACTION(2) /'CONVERGENCE: (RDEL-Qin)<=PCLOSE'/
      DATA ACTION(3) /'EXCEEDANCE:  PDIV > ADIV       '/
      DATA STAGE(1) /' INITIAL VALUES'/
      DATA STAGE(2) /'  CUMULATE PDIV'/
      DATA STAGE(3) /'       SOLUTION'/      
      INTEGER IOUTFILE,M,ITER2,ISENDSTR,IFCRIDOLD,N,ITER,ISOL,IJUNUSTR,
     1 IFCRIDALT,ISNUPSG,NFA,JNUPSG
      REAL JNSTROUT,SNSTROUT,FLOWOUT                 
      DOUBLE PRECISION ADIV,PDIV,SIN,SOUT,DVRS,RDELOLD,DEL
C     -----------------------------------------------------------------
C
C1===== PRINT HEADERS =======================================================================================
      IF(IPAPFL.EQ.-1.OR.IPAPFL.EQ.1) THEN
C        IF(IPAPFL.EQ.-1) IOUTFILE=IOUT
C        IF(IPAPFL.EQ.1) THEN
C          OPEN(UNIT=1007, FILE='PRIOR.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1007
C        ENDIF
C
      WRITE(FMPOUT%UNIT(11),3) NF,KPER,KSTP 
    3 FORMAT(/,1X,'PRIOR APPROPRIATION FOR FARM: ',I7,
     1',   PERIOD: ',I7,',    STEP: ',I7,//,1X,
     2'FARM',3X,'HEAD-GATE',5X,'CANAL',5X,'RIVER',/,
     3 12X,'REACH',3X,'SEGMENT',3X,'SEGMENT')
      WRITE(FMPOUT%UNIT(11),4) NF,L,ISTRM(4,L),IDIVAR(1,ISTRM(4,L))
    4 FORMAT(I5,I12,2I10)
C
      WRITE(FMPOUT%UNIT(11),6)
    6 FORMAT(/,1X,'|Budget at Point of Canal-Diversion from River:',10X,
     1'|Budget at Point of Farm Diversion from Canal:',11x,'|',/,1X,
     2'|(at downstream end of river reach)',22X,
     3'|(at upstream end of farm head-gate reach in canal)',6x,'|',/,1X,
     4162('.'),/,1X,'|',4X,'Qstr-in',3x,'Qstr-out',3x,'Qstr-min',7x,
     5'ADIV',7x,'PDIV ','|',3x,'RDEL-req',5x,'Qcn-in',4x,'Qcn-out',4x,
     6'Qcn-min',3x,'DELIVERY |',12X,'STAGE',3X,'RESULT',/,1X,162('.'))
C
      ENDIF
C
C2===== UPDATE POTENTIAL AND ACTUAL DIVERSION INTO CURRENT CANAL SEGMENT ====================================
C
C2A-----UPDATE BUDGET FOR RIVER DIVERSION REACH (DIVERSION FROM RIVER INTO CANAL):
C       (DIVERSION OCCURS AT END OF REACH)
      DO M=1,NSTRM-1
       IF(ISTRM(4,M).EQ.IDIVAR(1,ISTRM(4,L)).AND.
     1    ISTRM(5,M).GT.ISTRM(5,M+1)) THEN
          SIN=DBLE(ANINT(STRM(9,M)*AR))/AC
        ENDIF
      ENDDO
      SOUT=DBLE(ANINT(SGOTFLW(IDIVAR(1,ISTRM(4,L)))*AR))/AC
      DVRS=DBLE(ANINT(DVRSFLW(ISTRM(4,L))*AR))/AC
C
C2B-----UPDATE TERMS NEEDED FOR BUDEGT FOR CANAL DIVERSION REACH (DIV. CAN --> FARM):
C       (DIVERSION OCCURS AT BEGINNING OF REACH
C     FLOWIN IS DEFINED AHEAD OF THE SUBROUTINE, SINCE IT COULD BE CONSTRAINED BY CALLS
      FLOWOUT=STRM(9,L) !seems to be redundant ...remove after verifying
C
C2C-----FOR PRESENT FARM: ACCOUNT FOR MINIMUM RIVER STREAMFLOW (QSTRMIN)
C       THAT IS NEEDED FOR A HIGHER RANK FARM ON A DOWNSTREAM CANAL SEGMENT
      IF(QSTRMIN(NF).GT.PCLOSE) THEN
        ADIV=SOUT+DVRS-QSTRMIN(NF)
      ELSE
        ADIV=DVRS                                                       !actual diversion
      ENDIF               
      PDIV=DBLE(ANINT(SEG(2,ISTRM(4,L))*AR))/AC                         !potential diversion
C
C2D-----ACCOUNT FOR SURPLUS SITUATION AT HEAD GATE REACH (EXCEPTIONS FROM REGULAR "NO-SURPLUS" CASE)
C       (REGULAR CASE:    NO SURPLUS: SUPPLY <= DEMAND;
C        EXCEPTIONS:      SURPLUS:    1) SUPPLY DUE TO NRD > DEMAND,
C                                     2) LIMITING SUPPLY > NEW DEMAND AFTER INVOKING DROUGHT RESPONSE POLICY
C2D1----EXCEPTION 1:
C       Allow PDIV to iterate, if unnecessary non-routed deliveries are specified
C       to be recharged back into the canal as surplus
C       (if INRDU flag = 1 --> the absolute amount available is used;
C        surplus recharged into canal at farm's head gate --> RDEL will be negative).
      IF(RDEL.LT.0D0) PDIV=ADIV
C
C2D2----EXCEPTION 2:
C       Allow PDIV to iterate, if execution of deficiency scenario for an 
C       upstream senior farm leads to: new demand even < formerly constrained supply!
C       Assumption: New surplus (old constrained supply - new demand) reduces the
C                   RDEL, and accordingly less PDIV is required from stream into canal.
c       In this case: The diversion of a smaller PDIV leads to excess stream-flow,
c       which the downstream junior farm will benefit from.
c       [but set pdiv=adiv only for the first time when a smaller PDIV drops below ADIV
c        after the previous iteration, when still insufficiency existed (PDIV > ADIV)].
      IF(iter2.eq.1.and.rdel+flowinmin(l).lt.flowin.and.
     1   pdiv.gt.adiv) pdiv=adiv
C
C2E-----DETERMINE DIVERSION FROM STREAM INTO EACH CANAL ITERATIVELY (EXIT PRIOR AND SOLVE MF2K5)
C
C2E1----CHECK IF THERE ARE SENIOR FARMS DOWNSTREAM THE SAME CANAL AS CURRENT FARM
      ISENDSTR=0
      IFCRIDOLD=0
      DO M=3,NSTRM                                                      !reach loop within reach reach loop of FMP3FM
      DO N=1,NFARMS
        IF(NFSEG(NF).EQ.ISTRM(4,M).AND.M.GT.L.AND.                      !if reach is on same segment as current farm & if reach is downstream the current headgate reach
     1     (IFCRID(N,M).LT.NF .AND. IFCRID(N,M).GT.0 .AND.              !Farm Reach: * is a headgate reach of a farm senior to the current junior rights farm, * is at all a farm reach, * 1st one being different from previous one,
     2     IFCRID(N,M).NE.IFCRID(N,M-1).AND.IFCRID(N,M).NE.             !Farm Reach: * 1st one being different from second previous one, meaning 1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point), but still within the same farm.
     3     IFCRID(N,M-2).AND.IFCRID(N,M).NE.IFCRIDOLD)) THEN
           IFCRIDOLD=IFCRID(N,M)
           ISENDSTR=ISENDSTR+1                                          !number of senior-rights-farms downstream the same canal segment
         ENDIF
      ENDDO
      ENDDO
C
C2E2----IF THERE IS A DOWNSTREAM SENIOR FARM, THEN ADD THE MINIUM CANAL STREAMFLOW REQUIREMENT
C       TO THE REACH DELIVERY REQUIRMENT OF THE CURRENT FARM
      IF(ISENDSTR.EQ.0) FLOWINMIN(L)=0.D0
      RDEL=RDEL+FLOWINMIN(L)
C
C2E3----IF SURPLUS > FLOWINMIN: SOLUTION FOUND (NO PDIV NEEDED)
c       Solution will be: (1) no PDIV is needed (irdrfl=1 --> don't iterate), and 
c                         (2) SEG(2,nseg) in FMP3FM will be set to zero!  --> zero solution!
      IF(RDEL.LT.0D0) THEN
        RDEL=0D0                                                        !__   if surplus > flowinmin then surplus compensates at least for flowinmin and 
        ITER=0                                                          !  |__provides additional excess surplus to be recharged into canal!
        IRDRFL(NF)=1
        GO TO 200
      ENDIF
c
C2E4----ADD 'UNSATISFIED' INCREMENT [RDEL(k)-FLOWIN(k-1)] FROM CURRENT ITERATION TO 
C       CUMULATIVE POTENTIAL DIVERSION OF SENIOR FARMS FROM PREVIOUS ITERATION
C
C2E4A---CUMULATE PDIV BY INCREMENT
C       - PDIV will be added to previous cumulative PDIV (for TF-ND > 0), or
c       - subtracted from prevous cumulative PDIV (for TF-ND < 0 & for surplus < flowinmin).
      ITER=0
      IF(IPAPFL.EQ.-1.OR.IPAPFL.EQ.1) THEN
C        IF(IPAPFL.EQ.-1) IOUTFILE=IOUT
C        IF(IPAPFL.EQ.1) THEN
C          OPEN(UNIT=1007, FILE='PRIOR.OUT', STATUS='UNKNOWN')
C          IOUTFILE=1007
C        ENDIF
      !#################################################################!IS THERE SUPPOSED TO BE 2 FLOWIN VARIABLES
      WRITE(FMPOUT%UNIT(11),7) SIN,SOUT,QSTRMIN(NF),ADIV,PDIV,RDEL,     !IS THERE SUPPOSED TO BE 2 FLOWIN VARIABLES
     1FLOWIN, FLOWIN,FLOWINMIN(L),FLOWIN-FLOWINMIN(L),STAGE(1)
    7 FORMAT(1x,'|',5F11.3,' |',5F11.3,' |',2X,A15)
      ENDIF   
C     
      IF(DABS(RDEL-FLOWIN).GT.PCLOSE.
     1  AND.(ADIV.GT.PDIV.OR.DABS(ADIV-PDIV).LT.PCLOSE).                !PDIV from previous farm on same segment; ADIV >= PDIV (+- inaccuracy)
     2  AND.(QSTRMIN(NF).NE.SOUT.OR.QSTRMIN(NF).EQ.0D0.OR.              !__   Don't allow a spec. case where: NOTIF(QSTRMIN.EQ.SOUT.AND.QSTRMIN.NE.0.AND.SOUT.NE.0.AND.DVRS.EQ.0)
     3       SOUT.EQ.0D0.OR.DVRS.NE.0D0)) THEN                          !  |  expressed as ....................  IF(QSTRMIN.NE.SOUT.OR.QSTRMIN.EQ.0.OR.SOUT.EQ.0.OR.DVRS.NE.0)
        PDIV=PDIV+(RDEL-FLOWIN)                                         !  |__==> immediately solve!
C
        if(iter2.eq.1) THEN
           IF(ISENDSTR.GT.0) RDEL=RDEL-FLOWINMIN(L)
           RDR(L)=RDEL
        ENDIF
C
        IF(QSTRMIN(NF).GT.PCLOSE.AND.PDIV-ADIV.GT.PCLOSE) THEN          !__   if no min streamflow in river need to be maintained for higher rank farms on downstream diverting canals
           SEG(2,ISTRM(4,L))=SNGL(ADIV)                                 !  |__this PDIV now account for current and previous farms on same segment
           DEL=0.D0                                                     !only need for write statement
        ELSE                                                            !if no min streamflow in river need to be maintained for higher rank farms on downstream diverting canals
           SEG(2,ISTRM(4,L))= SNGL(PDIV)
           STRM(12,L)=SNGL(SFRADD(L)-RDR(l))                            !HAS TO BE DONE HERE ARLEADY: EXITING FMP3FM!
           DEL=RDR(L)                                                   !only need for write statement
        ENDIF
C
        ITER=1
        IF(ISTARTFL.EQ.1) ISTARTFL=0
C
C2E4B---PRINT BUDGET AFTER CUMULATING PDIV (BUT BEFORE EXITING FMP3FM AND MF2K5 SOLUTION)
        IF(IPAPFL.EQ.-1.OR.IPAPFL.EQ.1) THEN
C          IF(IPAPFL.EQ.-1) IOUTFILE=IOUT
C          IF(IPAPFL.EQ.1) THEN
C            OPEN(UNIT=1007, FILE='PRIOR.OUT', STATUS='UNKNOWN')
C            IOUTFILE=1007
C          ENDIF
          WRITE(FMPOUT%UNIT(11),9) SIN,SOUT,QSTRMIN(NF),ADIV,PDIV,RDEL,
     1    FLOWIN,FLOWIN-DEL,FLOWINMIN(L),FLOWIN-FLOWINMIN(L),
     2    STAGE(2),ACTION(1)
    9     FORMAT(1x,'|',5F11.3,' |',5F11.3,' |',2X,A15,3X,A31,/)
        ENDIF
C
C2E4C---EXIT SUBROUTINE PRIOR
        GO TO 100
C
C3===== SOLUTION FOR REACH DELIVERY TO FARM AND FOR ACTUAL DIVERSION INTO CANAL FOUND ???? ==================
C
C3A------SOLVE
C       (notice: before: RDEL was "reach delivery requirement (TF-ND + Q-min)" before,
C                but now: variable RDEL is re-used as "reach delivery to current farm")
c       Solution for general cases (for TF-ND > 0, and TF-ND < 0 and surplus < flowinmin):
c       - if |rdel(t,k) - flowin(t,k-1)| <= convergence criterion, or     --> "convergence solution!"
c       - if cumulative pot. diversion (PDIV)
C         exceeds actually possible diversion (ADIV).                     --> "exceedance solution!"
      ELSEIF(DABS(RDEL-FLOWIN).LE.PCLOSE.OR.
     1 (DABS(RDEL-FLOWIN).GT.PCLOSE.AND.PDIV-ADIV.GT.PCLOSE).OR.        !ADIV < PDIV (+- inaccuray)
     2 (QSTRMIN(NF).EQ.SOUT.AND.QSTRMIN(NF).NE.0D0.OR.                  !__   Solve for a spec. case where: IF(QSTRMIN.EQ.SOUT.AND.QSTRMIN.NE.0.AND.SOUT.NE.0.AND.DVRS.EQ.0)
     3  SOUT.NE.0D0.OR.DVRS.EQ.0D0))THEN                                !  |__
        RDELOLD=RDEL
        IF(DABS(RDEL-FLOWIN).LE.PCLOSE) THEN
          ISOL=2
        ELSE
          ISOL=3
        ENDIF
        if(PDIV-ADIV.GT.PCLOSE.AND.FLOWIN-RDEL.GT.PCLOSE) GOTO 300      !jump over next line (next line only for convergence solution).
        RDEL=FLOWIN-FLOWINMIN(L)                                        !STRM(12,L)=SFRADD(L)-RDEL IS DONE IN FMP3FM
C
300     ITER=0
        IRDRFL(NF)=1                                                    !irdrfl(nf)=1: after solution is found for a farm, don't iterate for that farm's RDR anymore!
C
C3B-----PRINT BUDGET AFTER SOLUTION (BUT BEFORE MF2K5 SOLUTION)
        IF(IPAPFL.EQ.-1.OR.IPAPFL.EQ.1) THEN
C          IF(IPAPFL.EQ.-1) IOUTFILE=IOUT
C          IF(IPAPFL.EQ.1) THEN
C            OPEN(UNIT=1007, FILE='PRIOR.OUT', STATUS='UNKNOWN')
C            IOUTFILE=1007
C          ENDIF
          WRITE(FMPOUT%UNIT(11),11) SIN,SOUT,QSTRMIN(NF),ADIV,PDIV,
     1    RDELOLD,FLOWIN,FLOWIN-RDEL,FLOWINMIN(L),RDEL,
     2    STAGE(3),ACTION(ISOL)
   11     FORMAT(1x,'|',5F11.3,' |',5F11.3,' |',2X,A15,3X,A31,/)
        ENDIF
C
      ENDIF
c
C4==== EVALUATE MINIMUM CANAL STREAMFLOW AT JUNIOR FARM =====================================================
C      (required for senior farm downstream the same canal)   
      if(pdiv-adiv.gt.PCLOSE.and.flowin-rdel.gt.PCLOSE) go to 150       !don't calculate flowinmin if adiv restricts upstream rdel, but would be causing still too much flowin for current senior downstream farm. ... before: go to 100
200   IJUNUSTR=0
      IFCRIDALT=0 
      DO M=3,NSTRM                                                      !reach loop within reach loop of FMP3FM
      DO N=1,NFARMS
        IF(NFSEG(NF).EQ.ISTRM(4,M).AND.M.LT.L.AND.                      !if reach is on same segment as current farm & if reach is upstream the current headgate reach
     1     (IFCRID(N,M).GT.NF .AND. IFCRID(N,M).GT.0 .AND.              !Farm Reach: * is a headgate reach of a farm junior to the current junior rights farm, * is at all a farm reach, * 1st one being different from previous one,
     2     IFCRID(N,M).NE.IFCRID(N,M-1).AND.IFCRID(N,M).NE.             !Farm Reach: * 1st one being different from second previous one, meaning 1st previous one could be just a blank (FCRID(NF,L)=0 at farm corner point), but still within the same farm.
     3     IFCRID(N,M-2).AND.IFCRID(N,M).NE.IFCRIDALT)) THEN
           IFCRIDALT=IFCRID(N,M)
           IJUNUSTR=IJUNUSTR+1
           IF(IJUNUSTR.EQ.1) THEN                                       !evaluate minimum streamflow required for current downstream farm NF for next upstream junior rights farm, while not yet taking into account the delivery requirement of that farm (RDR is still zero)
           FLOWINMIN(M)=DBLE(STRM(9,M-1))-RDR(M)                        !store minimum flowin at headgate reach of next upstream junior rights farm, which is necessary to account for the senior rights deliveries downstream. Then, take that "minimum streamflow" requirement into account, when calculating the total Reach Delivery for the next upstream farm with a junior right.
           IF(FLOWINMIN(M).LT.0D0) FLOWINMIN(M)=0.D0                    !(for new time step: strm(9,m-1) comes from 1st iteration with SFR only; rdr(m) comes from last it. of previous time step ==> therefore: FLOWIN could be, but should not be less than zero! )
           ENDIF
         ENDIF
      ENDDO
      ENDDO
c
C5===== EVALUATE MINIMUM RIVER STREAMFLOW AT DIVERSION INTO UPSTREAM CANAL ==================================
c       to account for (1) diversion into downstream canal for senior farm(s), and
c                      (2) conveyance losses between up- and downstream canal diversions. 
150   DO M=2,NSTRM
         IF(ISTRM(4,M).EQ.NFSEG(NF).AND.ISTRM(5,M).EQ.1) THEN
         ISNUPSG=IDIVAR(1,ISTRM(4,M))
         SNSTROUT=SGOTFLW(ISNUPSG)
         ENDIF
      ENDDO      
      DO NFA=1,NFARMS
         IF(NFA.GT.NF.AND.NFSEG(NFA).LT.NFSEG(NF)) THEN
            DO M=2,NSTRM                   
               IF(ISTRM(4,M).EQ.NFSEG(NFA).AND.ISTRM(5,M).EQ.1) THEN
               JNUPSG=IDIVAR(1,ISTRM(4,M))
               JNSTROUT=STRM(9,M-1)-STRM(9,M)                           !SGOTFLW(JNUPSG)?
               QSTRMIN(NFA)=DBLE(JNSTROUT-SNSTROUT)
               ENDIF
            ENDDO   
         ENDIF
      ENDDO
C
C6===== RETURN ============================================================================================== 
100   RETURN
      END SUBROUTINE
C
C
      SUBROUTINE PRIORCOR(TFPC,N,QRPC)
C-----VERSION 2 10/15/2009 PRIORCOR      
      USE FMPMODULE,    ONLY:NFARMS,NFSEG,NRD,IFCRID,NWPERF,QAVF,
     1                       FWELL,NFWELS,NFWLVL,MXFWEL,QDEF,QEXC,RDR
      USE GWFSFRMODULE, ONLY:SEG,NSEGDIM,NSTRM,STRM
C     ******************************************************************
C     CORRECTION OF INACCURACIES RESULTING FROM THE SIMULATION OF A
C     WATER-RIGHTS RANKING DRIVEN PRIOR APPROPRIATION SYSTEM OF FARMS (IALLOTSW=3)
C     ------------------------------------------------------------------
C     correction 1:  Objective: If senior farm on upstream canal experiences QREQ,
C                    then junior farm's RDEL on downstream canal should be 0!
C     correction 2:  Objective: If junior farm on upstream canal experiences QREQ,
C                    then senior farm's QREQ on downstream canal should be 0!
C     ******************************************************************
C        SPECIFICATION:
C     ------------------------------------------------------------------
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION TFPC(NFARMS),QRPC(NFARMS)
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:      
C     ------------------------------------------------------------------
      INTEGER NFA,NFB,M,NX
      DOUBLE PRECISION RDEL,QREQB,QSUMDEF,QSUMEXC,QDEFICIENCY,QEXCESS
C     ------------------------------------------------------------------      
c
      do nfa=1,nfarms 
      do nfb=1,nfarms
c
C1===== Correction 1 ========================================================================================
C
C       Objective:
C
C       If senior farm on upstream canal experiences QREQ,
C       then junior farm's RDEL on downstream canal should be 0!
c       But head-dependent ADIV may change (here is ADIV(k) > ADIV(k-1)), which could lead to:
c
c       Error 1: A small quantity of SW may be available for diversion and as RDEL
c       to a junior farm on an downstream canal, while a senior farm on an upstream canal
c       experiences already a QREQ based on a smaller ADIV(k-1) from the previous iteration.
C
c       correction 1:
c
c       upstream canal: 1. senior farm can divert more from upstream canal (+RDEL of downstream senior farm)
c                       2. upstream canal can divert more from river (+RDEL of downstream senior farm)
c                       3. senior farm's QREQ is smaller (-RDEL of downstream senior farm)
c       downstrm canal: 1. junior farm should divert nothing from downstream canal (RDEL = 0)
c                       2. downstream canal should divert more from river (+RDEL of downstream senior farm)
c                       3. junior farm's QREQ is higher (QREQ = TF-ND-AFDELSW, where AFDELSW = RDEL = 0) 
C
      if(nfb.gt.nfa.and.nfseg(nfb).gt.nfseg(nfa)) then                
      rdel=TFPC(nfb)-nrd(n,nfb)-QRPC(nfb)
      if(rdel.ne.0d0.and.QRPC(nfb).gt.0d0.and.QRPC(nfa).gt.0d0) then
      if(rdel.gt.QRPC(nfa)) rdel=QRPC(nfa)                              !__   If both numbers "QRPC for farm nf" and "rdel for farm nf+1" are very small,
          seg(2,nfseg(nfb))=seg(2,nfseg(nfb))- SNGL(rdel)               !  |  then in rare cases "rdel for farm nf+t" > "QRPC for farm nf."
          seg(2,nfseg(nfa))=seg(2,nfseg(nfa))+ SNGL(rdel)               !  |__In this case, rdel(nf+1) can at most be reduced by QRPC(nf) !!!
          QRPC(nfa)=QRPC(nfa)- rdel 
          QRPC(nfb)=QRPC(nfb)+ rdel 
          do m=1,nstrm
          do nx=1,nfarms
              if(ifcrid(nx,m).eq.nfa) then
              STRM(12,m)=strm(12,m)-SNGL(RDEL)
              goto 123
              endif
          enddo
          enddo
123       do m=1,nstrm
          do nx=1,nfarms
              if(ifcrid(nx,m).eq.nfb) then
              STRM(12,m)=strm(12,m)+SNGL(RDEL)
              goto 124
              endif
          enddo
          enddo
124   IF (NWPERF(NFA).GT.0) THEN
      QAVF(NFA)=QRPC(NFA)/DBLE(NWPERF(NFA))
      ENDIF
      QSUMDEF=0.D0
      QSUMEXC=0.D0
      DO M=1,NFWELS
      IF(IDINT(FWELL(5,M)).EQ.NFA) THEN  
         IF (FWELL(6,M).LE.QAVF(NFA)) THEN
         QDEFICIENCY=QAVF(NFA)-FWELL(6,M)
         QSUMDEF=QSUMDEF+QDEFICIENCY
         ELSE
         QEXCESS=FWELL(6,M)-QAVF(NFA)
         QSUMEXC=QSUMEXC+QEXCESS
         ENDIF                         
      ENDIF
      ENDDO
      QDEF(NFA)=QSUMDEF
      QEXC(NFA)=QSUMEXC
      endif
      endif
C
C2===== Correction 2 ========================================================================================
C
C       Objective:
C
C       If junior farm on upstream canal experiences QREQ,
c       then senior farm's QREQ on downstream canal should be 0!
c       But head-dependent ADIV may change (here is ADIV(k) < ADIV(k-1)), which could lead to:
c       Error 2: A small quantity of QREQ may be required now for senior farm on downstream canal,
c       while at the same time actually the junior farm on upstream canal experiences already 
c       a QREQ based on a higher ADIV(k-1) from the previous iteration.
C
c       correction 2:
c
c       upstream canal: 1. senior farm can divert less from canal (-QREQ of downstream junior farm
c                       2. upstream canal can divert less from river (-QREQ of downstream junior farm)
c                       3. senior farm's QREQ is higher (+QREQ of downstream junior farm)
c       downstrm canal: 1. junior farm should divert more from downstream canal (+QREQ of downstream junior farm)
c                       2. downstream canal should divert more from river (+QREQ of downstream junior farm)
c                       3. junior farm's QREQ is zero (QREQ = 0)
C
      if(nfa.gt.nfb.and.nfseg(nfa).lt.nfseg(nfb)) then
      rdel=TFPC(nfa)-nrd(n,nfa)-QRPC(nfa)
      qreqb=QRPC(nfb)
      if(QRPC(nfa).gt.0d0.and.rdel.ne.0d0.and.QRPC(nfb).gt.0d0) then
      if(QRPC(nfb).gt.rdel) qreqb=rdel
          seg(2,nfseg(nfa))=seg(2,nfseg(nfa))- SNGL(qreqb)              !QRPC(nfb)
          seg(2,nfseg(nfb))=seg(2,nfseg(nfb))+ SNGL(qreqb)              !QRPC(nfb)
          QRPC(nfa)=QRPC(nfa)+  qreqb                                   !QRPC(nfb)
          do m=1,nstrm
          do nx=1,nfarms
              if(ifcrid(nx,m).eq.nfa) then
              STRM(12,m)=strm(12,m)+ SNGL(qreqb)                        !QRPC(nfb)
              goto 125
              endif
          enddo
          enddo
125       do m=1,nstrm
          do nx=1,nfarms
              if(ifcrid(nx,m).eq.nfb) then
              STRM(12,m)=strm(12,m)- SNGL(qreqb)                        !QRPC(nfb)
              goto 126
              endif
          enddo
          enddo
126       if(QRPC(nfb).le.rdel) QRPC(nfb)=0d0
          if(QRPC(nfb).gt.rdel) QRPC(nfb)=QRPC(nfb)- qreqb 
      IF (NWPERF(NFB).GT.0) THEN
      QAVF(NFB)= QRPC(NFB)/DBLE(NWPERF(NFB))
      ENDIF 
      QSUMDEF=0.D0
      QSUMEXC=0.D0
      DO M=1,NFWELS
      IF(IDINT(FWELL(5,M)).EQ.NFB) THEN  
         IF (FWELL(6,M).LE.QAVF(NFB)) THEN
         QDEFICIENCY=QAVF(NFB)-FWELL(6,M)
         QSUMDEF=QSUMDEF+QDEFICIENCY
         ELSE
         QEXCESS=FWELL(6,M)-QAVF(NFB)
         QSUMEXC=QSUMEXC+QEXCESS
         ENDIF                         
      ENDIF
      ENDDO
      QDEF(NFB)=QSUMDEF
      QEXC(NFB)=QSUMEXC
      endif
      endif   
C     
      enddo
      enddo
C
C6===== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE RISE_RUN()
C-----VERSION 2.01 07/28/2010 RISERUN
C     ******************************************************************
C     CALCULATE SLOPE AS RISE-RUN TO RELATE FIESWP AND FIESWI IN FM TO SLOPE.
C     METHOD: THIRD-ORDER FINITE DIFFERENCE ESTIMATOR USING ALL 8 OUTHER 
C     POINTS OF NINE ELEVATION POINTS (3X3 WINDOW) - AFTER HORNE (1981).
C     EDGES: 6 ELEVATION POINTS (3X2 WINDOW).
C     CORNERS: 4 ELEVATION POINTS (2X2 WINDOW).
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,    ONLY:GSURF,RISERUN
      USE GLOBAL,       ONLY:NCOL,NROW,NLAY,DELR,DELC
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
c     ------------------------------------------------------------------
      INTEGER IR,IC,IR1,IR2,IR3,IC1,IC2,IC3,IMX,IMY,ID
      DOUBLE PRECISION DZDX,DZDY
C     ------------------------------------------------------------------
      DO IR=1,NROW
      DO IC=1,NCOL
      IR1=0
      IR2=0
      IR3=0
      IC1=0
      IC3=0
      IC2=0
      IMX=0
      IMY=0
      ID=0 
C1------CORNERS
      if(ir.eq.1.or.ic.eq.1.or.ir.eq.nrow.or.ic.eq.ncol) then
      if(ir.eq.1.and.ic.eq.1) then
      ic1=ic
      ic3=ic+1
      ir1=ir
      ir3=ir+1
      elseif(ir.eq.1.and.ic.eq.ncol) then
      ic1=ic-1
      ic3=ic
      ir1=ir
      ir3=ir+1
      elseif(ir.eq.nrow.and.ic.eq.1) then
      ic1=ic
      ic3=ic+1
      ir1=ir-1
      ir3=ir
      elseif(ir.eq.nrow.and.ic.eq.ncol) then
      ic1=ic-1
      ic3=ic
      ir1=ir-1
      ir3=ir
      endif
      ic2=1
      ir2=1
      imx=0
      imy=0
      id=2
      endif
c2------EDGES
      if(ir.eq.1.and.(ic.ne.1.and.ic.ne.ncol)) then
      ic1=ic-1
      ic2=ic
      ic3=ic+1
      ir1=ir
      ir2=1
      ir3=ir+1
      imx=0
      imy=2
      id=4
      elseif(ir.eq.nrow.and.(ic.ne.1.and.ic.ne.ncol)) then
      ic1=ic-1
      ic2=ic
      ic3=ic+1
      ir1=ir-1
      ir2=1
      ir3=ir
      imx=0
      imy=2
      id=4
      elseif(ic.eq.1.and.(ir.ne.1.and.ir.ne.nrow)) then
      ic1=ic
      ic2=1
      ic3=ic+1
      ir1=ir-1
      ir2=ir
      ir3=ir+1
      imx=2
      imy=0
      id=4
      elseif(ic.eq.ncol.and.(ir.ne.1.and.ir.ne.nrow)) then
      ic1=ic-1
      ic2=1
      ic3=ic
      ir1=ir-1
      ir2=ir
      ir3=ir+1
      imx=2
      imy=0
      id=4
C3------GENERAL
      elseif(ic.ne.1.and.ir.ne.1.and.ic.ne.ncol.and.ir.ne.nrow) then
      ic1=ic-1
      ic2=ic
      ic3=ic+1
      ir1=ir-1
      ir2=ir
      ir3=ir+1
      imx=2
      imy=2
      id=8
      endif
C4------THIRD ORDER FINITE DIFFERENCE ESTIMATOR
      DZDX=GSURF(IC1,IR1)+DBLE(IMX)*GSURF(IC1,IR2)+GSURF(IC1,IR3)
     1   -(GSURF(IC3,IR1)+DBLE(IMX)*GSURF(IC3,IR2)+GSURF(IC3,IR3))
      DZDY=GSURF(IC1,IR1)+DBLE(IMY)*GSURF(IC2,IR1)+GSURF(IC3,IR1)
     1   -(GSURF(IC1,IR3)+DBLE(IMY)*GSURF(IC2,IR3)+GSURF(IC3,IR3))
      DZDX=DZDX/(DBLE(ID)*DBLE(DELR(IC)))
      DZDY=DZDY/(DBLE(ID)*DBLE(DELC(IR)))
      RISERUN(IC,IR)=DSQRT(DZDX**2+DZDY**2)                             !RISE-RUN SLOPE IN PERCENT
      ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE
C
      SUBROUTINE RETURNFLOW(NF,IROUT)
C-----VERSION 2.01 07//28/2010 LOWESTCELL_NEARESTREACH
C     ******************************************************************
C     IF DRAIN IS NOT LOCATED WITHIN A FARM AND IF NO RETURNFLOW REACH IS SPECIFIED,
C     DETERMINE CELL WITH LOWEST ELEVATION IN A FARM, AND
C     CLOSEST DRAIN REACH (IRRFL.EQ.1) OR CLOSEST "ANY-TYPE" REACH (IRRFL=-1) FROM THAT CELL,
C     AND DEFINE THAT REACH TO BE A "FARM DRAIN REACH."
C     (RECHARGE CUMULATIVE RUNOFF INTO THAT DRAIN REACH IN FMP3FM-C10B1)
C     ******************************************************************
C        SPECIFICATIONS:
C     -------------------------------------------------------------------
      USE FMPMODULE, ONLY:IRTPFL,FDSEGL,ISRRFL,IRRFL,ISRR,IFID,IFDRID,
     1                    SFRADD,FDSEGL,GSURF,LFID
      USE GLOBAL, ONLY:NCOL,NROW,DELR,DELC
      USE GWFSFRMODULE, ONLY: NSTRM,IDIVAR,ISTRM,STRM,SEG
      IMPLICIT NONE
C     -------------------------------------------------------------------
C        ARGUMENTS:
C     -------------------------------------------------------------------
      INTEGER NF,IROUT
C     -------------------------------------------------------------------
C        LOCAL VARIABLES:
      INTEGER LOLD2,NRCH,ICNT,L,IR,IC,NR,NC,N,IA,IB
      DOUBLE PRECISION FDRAINSEGL,GSURFOLD,COLD,COUNT,ASUM,BSUM,C
C     -------------------------------------------------------------------
C8D4----DETERMINE FULLY-ROUTED OR SEMI-ROUTED SURFACE-WATER RUNOFF-RETURNFLOWS:
C       - DETERMINE DRAIN SEGEMENT LENGTH WITHIN A FARM.
C         (STRM(12,L)=RUNOFF PER REACH, SEG(3,ISTRM(4,L))=RUNOFF PER SEGMENT, SEG(1,ISTRM(4,L))=SEGMENT LENGTH)
C       - DEFINE REACHES WITHIN A FARM TO BE FARM DRAIN REACHES.
C       - DETERMINE PREEXISTING RUNOFF PER DRAIN REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
C       - LOCK PREEXISTING RUNOFF PER DRAIN REACH, SO THAT IT CAN'T BE OVERWRITTEN.
C       - WRITE LIST OF FOUND DRAIN REACHES AND FARM DRAIN SEGMENT LENGTH TO LIST FILE.
      IF(IRTPFL.NE.0) THEN
      WRITE(IROUT,
     +           '(/," RETURNFLOWS:",/,2X,"FULLY-ROUTED RETURNFLOWS:")')
      ENDIF
      FDRAINSEGL=0.D0
      IF(ISRRFL.EQ.0.OR.(ISRRFL.GT.0.AND.
     1  ISRR(2,NF).EQ.0.AND.ISRR(3,NF).EQ.0.AND.
     2  ISRR(4,NF).EQ.0.AND.ISRR(5,NF).EQ.0) ) THEN
        IF(IRTPFL.NE.0) THEN
        IF(IRRFL.EQ.1) THEN
        WRITE(IROUT,'(3X,"ACTIVATED SEARCH FOR REACHES OF ",
     1               "NON-DIVERSION SEGMENTS THAT ARE WITHIN A FARM.")')
        ELSEIF(IRRFL.EQ.-1) THEN
        WRITE(IROUT,'(3X,"ACTIVATED SEARCH FOR REACHES OF ANY STREAM",
     1  " SEGMENTS THAT ARE WITHIN A FARM.")')       
        ENDIF
        ENDIF
        LOLD2=0
        DO L=1,NSTRM
          DO IR=1,NROW
          DO IC=1,NCOL
            IF(IFID(IC,IR).EQ.NF.and.LFID(NF)) THEN                     !include test for returnflow only in active farm id's--rth
             IF(( (IRRFL.EQ.1.AND.IDIVAR(1,ISTRM(4,L)).EQ.0).OR.
     1            (IRRFL.EQ.-1) ).AND.
     2             ISTRM(2,L).EQ.IR.AND.ISTRM(3,L).EQ.IC ) THEN
              IF(L.NE.LOLD2) THEN                                       !don't count reaches twice if they are intercalated into an "inner" corner of the farm boundary!
              FDRAINSEGL=FDRAINSEGL+DBLE(STRM(1,L))
              IFDRID(NF,L)=NF                                           !CREATE ID OF REACHES WITHIN A FARM EQUAL TO ID OF FARM
              STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L)))!RUNOFF PER REACH MUST BE RESET: OTHERWISE FOR ITMP<0 IN SFR (DATA REUSED FOR NEW STRESS PERIODS), THE RUNOFF PER REACH WOULD NOT BE OVERWRITTEN, AND THE LAST 'RUNOFF MINUS REACH DEL.'-VALUE WOULD FALSLY REPRESENT THE RUNOFF PER REACH VALUE FOR THE NEW STRESS PERIOD!
              SFRADD(L)=DBLE(STRM(12,L))                                !-------> !RENAME, SO THAT FM AT SECOND AND FOLLOWING ITERATIONS DOESN'T START UP WITH STRM(12,L)=STRM(12,L)+RDR(L), BUT WITH STRM(12,L)=RUNOFF(L)+RDR(L)
              IF(IRTPFL.NE.0) THEN
              IF(LOLD2.EQ.0) WRITE(IROUT,220)                           !However note that reuse stress period (-1) in input file won't work. Each stress period's inflow must be specified separately, since if -1 is specified the last stored value (i.e. seg(2,n)) will be re-used.
  220         FORMAT(3X,'FULLY ROUTED RUNOFF-RETURNFLOW PRORATED ',
     1        'OVER REACHES WITHIN THE FARM AT:
     2        ',/6X,'ROW',2X,'COLUMN',2X,'SEGMENT_NO.',2X,'REACH_NO.',
     3        2X,'REACH_LENGTH')
              WRITE(IROUT,221)ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),
     1        ISTRM(5,L),STRM(1,L)
  221         FORMAT(3X,I6,I8,I12,I11,3X,G15.8)
              ENDIF
              ENDIF
              LOLD2=L
             ENDIF
            ENDIF
          ENDDO
          ENDDO
        ENDDO
      ELSE
        IF(IRTPFL.NE.0) THEN
        IF(IRRFL.EQ.1) THEN
        WRITE(IROUT,'(3X,"DEACTIVATED SEARCH FOR REACHES OF ",
     1  "NON-DIVERSION SEGMENTS THAT ARE WITHIN A FARM.")')
        ELSEIF(IRRFL.EQ.-1) THEN
        WRITE(IROUT,'(3X,"DEACTIVATED SEARCH FOR REACHES OF ANY STREAM "
     1             ,"SEGMENTS THAT ARE WITHIN A FARM.")')        
        ENDIF
        ENDIF
      ENDIF      
      FDSEGL(NF)=FDRAINSEGL      
      IF(IRTPFL.NE.0) THEN
      IF(FDSEGL(NF).GT.0.D0) THEN
        WRITE(IROUT,'
     1 (3X,"ACTIVE FARM DRAIN SEGMENT LENGTH: ",G15.8)') FDSEGL(NF)
      ELSE
        WRITE(IROUT,'
     1  (3X,"NO ACTIVE FARM DRAIN REACHES ARE WITHIN ",
     2  "THE FARM: NO FULLY-ROUTED RETURNFLOW POSSIBLE.")')
      ENDIF
      ENDIF
C
C8D5----IF DRAIN IS NOT LOCATED WITHIN A FARM AND IF NO RETURNFLOW REACH IS SPECIFIED,
C       DETERMINE CELL WITH LOWEST ELEVATION IN A FARM, AND
C       CLOSEST DRAIN REACH (IRRFL.EQ.1) OR CLOSEST "ANY-TYPE" REACH (IRRFL=-1) FROM THAT CELL,
C       AND DEFINE THAT REACH TO BE A "FARM DRAIN REACH."
C       (RECHARGE CUMULATIVE RUNOFF INTO THAT DRAIN REACH IN FMP3FM-C10B1)
C       (FMP3RP-C8D5 WAS FORMERLY (PRIOR TO 07/02/08) LOCATED IN FMP3FM-C10B2)
      IF(IRTPFL.NE.0) THEN
      WRITE(IROUT,'(/,2X,"SEMI-ROUTED RETURNFLOWS:")')
      ENDIF
      NRCH=0
      IF(FDSEGL(NF).EQ.0 .AND.(ISRRFL.EQ.0.OR.(ISRRFL.GT.0.AND.
     1  ISRR(2,NF).EQ.0.AND.ISRR(3,NF).EQ.0.AND.
     2  ISRR(4,NF).EQ.0.AND.ISRR(5,NF).EQ.0)))THEN 
        ICNT=0
        GSURFOLD=0.D0
        COLD=0.D0
        DO IR=1,NROW
        DO IC=1,NCOL
        IF(FDSEGL(NF).EQ.0.AND.IFID(IC,IR).EQ.NF.and.LFID(NF)) THEN     !include test for returnflow only in active farm id's--rth
         ICNT=ICNT+1
        IF(ICNT.EQ.1.OR.(ICNT.GT.1.AND.GSURF(IC,IR).LE.GSURFOLD))
     1   GOTO 200
         IF(GSURF(IC,IR).GT.GSURFOLD) GO TO 210
200      COUNT=0.D0                     
         DO L=1,NSTRM
           IF( (IRRFL.EQ.1.AND.IDIVAR(1,ISTRM(4,L)).EQ.0.
     1          AND.ISTRM(4,L).NE.1) .OR.  IRRFL.EQ.-1  ) THEN
           COUNT=COUNT+1
           ASUM=DBLE(DELC(ISTRM(2,L))/2.+DELC(IR)/2.)
           BSUM=DBLE(DELR(ISTRM(3,L))/2.+DELR(IC)/2.)
           NR=IABS(ISTRM(2,L)-IR)                                        !NUMBER OF ROWS BETWEEN STREAM REACH AND CELL
           NC=IABS(ISTRM(3,L)-IC)                                        !NUMBER OF COLUMNS BETWEEN STREAM REACH AND CELL
             IF(NR.EQ.0) ASUM=0.D0
             IF(NR.GT.1) THEN
             DO N=2,NR
               IF(ISTRM(2,L).LT.IR) IA=ISTRM(2,L)+(N-1)                 !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH!
               IF(ISTRM(2,L).GT.IR) IA=IR+(N-1)                         !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
               ASUM=ASUM+DBLE(DELC(IA))
             ENDDO
             ENDIF
             IF(NC.EQ.0) BSUM=0.D0
             IF(NC.GT.1) THEN
             DO N=2,NC  
               IF(ISTRM(3,L).LT.IC) IB=ISTRM(3,L)+(N-1)                 !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM STREAM REACH!
               IF(ISTRM(3,L).GT.IC) IB=IC+(N-1)                         !ROW NUMBER, WHOSE DELC WILL BE ADDED UP, STARTING FROM CELL!
               BSUM=BSUM+DBLE(DELR(IB))
             ENDDO
             ENDIF
           C=DSQRT(ASUM**2+BSUM**2)
             IF(COUNT.EQ.1 .OR. (COUNT.GT.1 .AND. C.LT.COLD)) THEN
             COLD=C
             NRCH=L
             ENDIF
           ENDIF
         ENDDO
         GSURFOLD=GSURF(IC,IR)
         ENDIF
210     ENDDO
        ENDDO
        IF(LFID(NF).and.NRCH.gt.0)IFDRID(NF,NRCH)=NF                    !include test for returnflow only in active farm id's--rth
        IF(IRTPFL.NE.0) THEN
        WRITE(IROUT,118)
     1  ISTRM(2,NRCH),ISTRM(3,NRCH),ISTRM(4,NRCH),ISTRM(5,NRCH)
  118   FORMAT(3X,'SEMI-ROUTED RUNOFF-RETURNFLOW TO A STREAM REACH',
     1  ' FOUND NEAREST TO THE LOWEST FARM ELEVATION AT:',/,6X,
     2  'ROW',2X,'COLUMN',2X,'SEGMENT NO.',2X,'REACH NO.',/,
     3  3X,I6,I8,I12,I11)
        ENDIF
      ENDIF
C
C8D6---FOR SEMI-ROUTED RUNOFF-RETURNFLOW TO SPECIFIED REACH:
C       - DEFINE A SPECIFIED REACH (BY COORDINATES) TO BE A "REMOTE" FARM 'DRAIN' REACH
C         (NOTE: UNLIKE 'DRAIN REACHES' INSIDE A FARM, THIS RETURNFLOW REACH CAN BE ON ANY TYPE OF SEGMENT)
C       - DETERMINE RUNOFF INTO "REMOTE" FARM RETURNFLOW REACH FROM PRESPECIFIED RUNOFF PER SEGMENT.
C       - LOCK RUNOFF INTO "REMOTE" FARM RETURNFLOW REACH, SO THAT IT CAN'T BE OVERWRITTEN.
      IF(ISRRFL.GT.0.AND.((ISRR(2,NF).GT.0.AND.ISRR(3,NF).GT.0).OR.
     1                    (ISRR(4,NF).GT.0.AND.ISRR(5,NF).GT.0))) THEN
        DO L=1,NSTRM                                                    !  IF POINT UNIQUELY IDENTIFIED BY:
         IF((ISRR(2,NF).EQ.ISTRM(2,L).AND.ISRR(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL SEG REACH | FULL SET OF INFO (MORE INFO THAN REQUIRED)
     1       ISRR(4,NF).EQ.ISTRM(4,L).AND.ISRR(5,NF).EQ.ISTRM(5,L)).OR. !  |                  |
     2      (ISRR(2,NF).EQ.ISTRM(2,L).AND.ISRR(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL SEG 0/-   | IF >=1 SEG PASS THROUGH CELL (USER PREFERS IDENTIFICATION OF LOCATION WITH COORDINATES)
     3       ISRR(4,NF).EQ.ISTRM(4,L).AND.ISRR(5,NF).EQ.0)         .OR. !  |                  |
     4      (ISRR(2,NF).EQ.ISTRM(2,L).AND.ISRR(3,NF).EQ.ISTRM(3,L).AND. !  |ROW COL 0/- 0/-   | IF JUST 1 SEG PASSES THROUGH CELL (USER PREFERS IDENTIFICATION OF LOCATION WITH COORDINATES)
     5       ISRR(4,NF).EQ.0.AND.ISRR(5,NF).EQ.0)                  .OR. !  |                  |
     6      (ISRR(2,NF).EQ.0.AND.ISRR(3,NF).EQ.0.AND.                   !  |0   0   SEG REACH | IF >=1 SEG PASS THROUGH CELL (USER PREFERS IDENFIFICATION OF LOCATION BY REACH NO.)
     7       ISRR(4,NF).EQ.ISTRM(4,L).AND.ISRR(5,NF).EQ.ISTRM(5,L)))THEN
            IFDRID(NF,L)=NF
            STRM(12,L)=SEG(3,ISTRM(4,L))*(STRM(1,L)/SEG(1,ISTRM(4,L)))
            SFRADD(L)=DBLE(STRM(12,L))
            IF(IRTPFL.NE.0) THEN
            WRITE(IROUT,119) ISTRM(2,L),ISTRM(3,L),ISTRM(4,L),ISTRM(5,L)
  119       FORMAT(3X,'SEMI-ROUTED RUNOFF-RETURNFLOW TO A SPECIFED ',
     1      'STREAM REACH AT:',/,6X,'ROW',2X,'COLUMN',2X,'SEGMENT NO.',
     2      2X,'REACH NO.',/,3X,I6,I8,I12,I11)
            ENDIF
          ENDIF
        ENDDO
      ELSEIF(NRCH.EQ.0) THEN 
      IF(IRTPFL.NE.0) THEN
      WRITE(IROUT,'
     1  (3X,"NO POINT OF RECHARGE FOR SEMI-ROUTED RETURNFLOW SPECIFIED:"
     2  ," NO SEMI-ROUTED RETURNFLOW POSSIBLE.")')
      ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE
C
C
C     ******************************************************************
      SUBROUTINE FMP3PRTOCH(ID)
C     ******************************************************************

C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,   ONLY:IFID,ICID,ISID,FMPDAT,NFARMS,NSOILS,NCROPS,
     1                      IFA,ISA,ICA
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,GLOBALDAT
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NPLEND,NCPP,NPL,
     1                      NCPPL,IBFLG,ICBOUND
      integer itest
C     ------------------------------------------------------------------
C
      ICEND=NCPP
      JCEND=NCPP     
      KCLAY=1
      DO KPLAY = 1,NPL
        KCEND=NCPPL(KPLAY)       
        ICR=1
        DO IPR = NPRBEG,NPREND
          JCC=1
          DO JPC = NPCBEG,NPCEND
            DO K=1,KCEND
              KM1=K-1
              DO I=1,ICEND
                IM1=I-1
                DO J=1,JCEND
                  JM1=J-1
                  IF(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .GT. 0 .OR.
     1            ABS(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1)).EQ.IBFLG)THEN
               IF(ID.EQ.1) IFID(JCC+JM1,ICR+IM1)=FMPDAT(1)%IFID(JPC,IPR)
               IF(ID.EQ.2) ISID(JCC+JM1,ICR+IM1)=FMPDAT(1)%ISID(JPC,IPR)
               IF(ID.EQ.3) ICID(JCC+JM1,ICR+IM1)=FMPDAT(1)%ICID(JPC,IPR)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
            JCC=JCC+JCEND
          ENDDO 
          ICR=ICR+ICEND
        ENDDO
        KCLAY=KCLAY+KCEND
      ENDDO
C
      IF(ID.EQ.1) THEN
      NCF=0
      DO NPF=1,FMPDAT(1)%NFARMS
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPF.EQ.IFID(IC,IR).AND.NPF.GT.NCF) THEN
          IFA(NPF)=IFID(IC,IR)
          NCF=NPF
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C      
      IF(ID.EQ.2) THEN
      NCS=0
      DO NPS=1,FMPDAT(1)%NSOILS
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPS.EQ.ISID(IC,IR).AND.NPS.GT.NCS) THEN
          ISA(NPS)=ISID(IC,IR)
          NCS=NPS
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C      
      IF(ID.EQ.3) THEN
      NCC=0
      DO NPC=1,FMPDAT(1)%NCROPS
      DO IR=1,NROW
      DO IC=1,NCOL
        IF(NPC.EQ.ICID(IC,IR).AND.NPC.GT.NCC) THEN
          ICA(NPC)=ICID(IC,IR)
          NCC=NPC
        ENDIF
      ENDDO
      ENDDO
      ENDDO
      ENDIF
C      
C      DO IR=1,NROW
C        IF(ID.EQ.1) WRITE(1256,'(12I7)') (IFID(IC,IR),IC=1,NCOL)
C        IF(ID.EQ.2) WRITE(1256,'(12I7)') (ISID(IC,IR),IC=1,NCOL)      
C        IF(ID.EQ.3) WRITE(1256,'(12I7)') (ICID(IC,IR),IC=1,NCOL)      
C      ENDDO
C
      return
      END SUBROUTINE
c      
C     ******************************************************************
      SUBROUTINE FMP3PWELTOCWEL(ID,MM,IBEG,IPFWEL,IUNITMNW2)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,   ONLY:MXPFW,NNPFWL,FWELL,FMPDAT,NFWLVL,MXFWEL,NAUX
     1                       ,FWLAUX,FWLAUXORDER,AUXV,MNW2NAM
      USE GLOBAL,      ONLY:IOUT
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NPLEND,NCPP,NPL,
     1                      NCPPL,IBFLG,ICBOUND
C     ------------------------------------------------------------------
C
      IF(ID.EQ.1) NPFWELS=FMPDAT(1)%NNPFWL
      IF(ID.EQ.2) NPFWELS=FMPDAT(1)%MXFWEL      
      M=MM
      DO L=1,NPFWELS       
      ICEND=NCPP
      JCEND=NCPP     
      KCLAY=0
      DO KPLAY = 1,NPL
        KCEND=NCPPL(KPLAY)       
        ICR=0
        DO IPR = NPRBEG,NPREND
          JCC=0
          DO JPC = NPCBEG,NPCEND
C            DO K=1,KCEND
              KM1=INT(KCEND/2)+1
C              DO I=1,ICEND
                IM1=INT(ICEND/2)+1
C                DO J=1,JCEND
                  JM1=INT(JCEND/2)+1
                  IF(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1) .GT. 0.OR.
     +            ABS(ICBOUND(JCC+JM1,ICR+IM1,KCLAY+KM1)).EQ.IBFLG)THEN
                     IF(FMPDAT(1)%FWELL(1,L).EQ.KPLAY.AND.
     1                  FMPDAT(1)%FWELL(2,L).EQ.IPR.AND.
     2                  FMPDAT(1)%FWELL(3,L).EQ.JPC) THEN
C
C                       EXCLUDE PARENT WELLS FROM BEING USED FOR A CHILD MODEL FARM, IF EVEN ONE WELL IS SPECIFIED FOR THAT CHILD MODEL FARM
                        DO N=IBEG,MM-1
                         IF(FMPDAT(1)%FWELL(5,L).EQ.FWELL(5,N)) GOTO 206
                        ENDDO
C                        
                        FWELL(1,M)=KCLAY+KM1
                        FWELL(2,M)=ICR+IM1
                        FWELL(3,M)=JCC+JM1
                        DO N=4,FMPDAT(1)%NFWLVL
                          FWELL(N,M)=FMPDAT(1)%FWELL(N,L)
C                          WRITE(1234,*) FWELL(N,M),FMPDAT(1)%FWELL(N,L)
                        ENDDO
                        AUXV(:,M)=FMPDAT(1)%AUXV(:,M)                   !TRANSFER AUX VARIABLES
                        IF(IUNITMNW2.GT.0)THEN
                          MNW2NAM(M)=FMPDAT(1)%MNW2NAM(M)               !TRANSFER MNW2 LINKED NAMES
                        END IF
                        !
                        IF(FWLAUXORDER(3).EQ."LGRGRID") THEN
                          FWELL(9,M)=1                                  !LGR FLAG ON, SET FWELL(9,M) TO 1 SCOTT THIS SEEMS TO DO NOTHING
                          AUXV(3,M)=1
                        END IF
C                        
C                       FOR NON-PARAMETER WELLS PULLED FROM NON-PAR. PARENT WELLS OR PARAMETER WELLS PULLED FORM PARAMETER PARENT WELLS,
C                       APPEND INFO TO LIST IN LIST FILE
                        IF(ID.EQ.1) NN=M
                        IF(ID.EQ.2) NN=M-MM+2
                        IF(LSTCHK(3)) THEN
                         IF(IPFWEL.EQ.1 .AND. IUNITMNW2.LE.0)    THEN    !ADDED IPFWEL AND MNW2 UNIT CHECKS FOR PRINT OUT seb
                           WRITE(IOUT,204) NN,(IDINT(FWELL(N,M)),N=1,5),
     1                      FWELL(6,M),(AUXV(N,M),N=1,NAUX)
                         ELSEIF(IPFWEL.EQ.1 .AND. IUNITMNW2.GT.0)THEN
                           WRITE(IOUT,205) NN,(IDINT(FWELL(N,M)),N=1,5),
     1                      FWELL(6,M),MNW2NAM(M),(AUXV(N,M),N=1,NAUX)
                         ENDIF
 204                     FORMAT(1X,4I7,2I11,ES16.4,1x,5I5)
 205                     FORMAT(1X,4I7,2I11,ES16.4,2x,A,1x,5I5) 
                        END IF
                     M=M+1
                     ENDIF
                  ENDIF
C                ENDDO
C              ENDDO
C            ENDDO
            JCC=JCC+JCEND
 206        ENDDO 
          ICR=ICR+ICEND
        ENDDO
        KCLAY=KCLAY+KCEND
      ENDDO
      ENDDO
      MM=M
C      DO M=1,MXFWEL
C       WRITE(1234,'(10F10.3)') (FWELL(N,M),N=1,NFWLVL)
C      ENDDO
C
      RETURN
      END SUBROUTINE
      
C     ******************************************************************
      SUBROUTINE FMP3PRTOCH_BILINEAR(P,F)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,   ONLY:FMPDAT
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,GLOBALDAT
      USE LGRMODULE,   ONLY:NPCBEG,NPRBEG,NPCEND,NPREND,NPLEND,NCPP,NPL,
     1                      NCPPL,IBFLG,ICBOUND
      INTEGER:: X,Y                                                     !seb ADDED EXPLICIT DECLARATION BECAUSE VARIABLES WERE ORIGINALLY TREATED AS REAL
      DOUBLE PRECISION R,F(NCOL,NROW),
     1 P(GLOBALDAT(1)%NCOL,GLOBALDAT(1)%NROW)
      ALLOCATABLE R(:,:)
C     ------------------------------------------------------------------
C-----IF PARENT ARRAY IS CONSTANT, RETURN CONSTANT FOR CHILD ARRAY
      IF(MAXVAL(P).EQ.MINVAL(P)) THEN
        F=MAXVAL(P)
        GOTO 100
      ENDIF
      ICEND=NCPP
      JCEND=NCPP     
      NROWEXT=NROW+2*ICEND
      NCOLEXT=NCOL+2*JCEND
      ALLOCATE(R(NCOLEXT,NROWEXT))
      R=0D0
      F=0D0
      KCLAY=0
      DO KPLAY = 1,NPL
        KCEND=NCPPL(KPLAY)       
        ICR=0
        DO IPR = NPRBEG-1,NPREND+1
          JCC=0
          DO JPC = NPCBEG-1,NPCEND+1
            KM1=INT(KCEND/2)+1
            IM1=INT(ICEND/2)+1
            JM1=INT(JCEND/2)+1
            DO Y=1,ICEND
            DO X=1,JCEND
              IF(JCC+X.EQ.JCC+JM1.AND.ICR+Y.EQ.ICR+IM1) THEN
                R(JCC+X,ICR+Y)=P(JPC,IPR)
              ENDIF
              IF(JCC+X.EQ.JCC+JM1) THEN
                IF(ICR+Y.LT.ICR+IM1) THEN
                  R(JCC+X,ICR+Y)=
     1          (P(JPC,IPR)-P(JPC,IPR-1))/ICEND*(IM1-1+Y)+P(JPC,IPR-1)
                ENDIF
                IF(ICR+Y.GT.ICR+IM1) THEN
                  R(JCC+X,ICR+Y)=
     1          (P(JPC,IPR+1)-P(JPC,IPR))/ICEND*(Y-IM1)+P(JPC,IPR)
                ENDIF
              ENDIF
              IF(ICR+Y.EQ.ICR+IM1) THEN
                IF(JCC+X.LT.JCC+JM1) THEN
                  R(JCC+X,ICR+Y)=
     1          (P(JPC,IPR)-P(JPC-1,IPR))/JCEND*(JM1-1+X)+P(JPC-1,IPR)
                ENDIF
                IF(JCC+X.GT.JCC+JM1) THEN
                  R(JCC+X,ICR+Y)=
     1          (P(JPC+1,IPR)-P(JPC,IPR))/JCEND*(X-JM1)+P(JPC,IPR)
                ENDIF
              ENDIF
            ENDDO
            ENDDO
            JCC=JCC+JCEND
          ENDDO 
          ICR=ICR+ICEND
        ENDDO
        KCLAY=KCLAY+KCEND
      ENDDO
C
C      DO IR=1,NROWEXT
C      WRITE(1239,'(<NCOLEXT>F15.7)') (R(IC,IR),IC=1,NCOLEXT)
C      ENDDO
C      WRITE(1239,'(/)')      
C
      ICR=0
      DO WHILE (ICR.LE.NROWEXT)
        JCC=0
        DO WHILE (JCC.LE.NCOLEXT)
          IM1=INT(ICEND/2)+1
          JM1=INT(JCEND/2)+1
            IF(JCC.GE.JCEND.AND.ICR.GE.ICEND.AND.
     1         JCC.LT.NCOLEXT-JCEND.AND.ICR.LT.NROWEXT-ICEND) THEN
            DO Y=1,ICEND
            DO X=1,JCEND
              IF(JCC+X.LT.JCC+JM1.OR.JCC+X.GT.JCC+JM1) THEN
                IF(ICR+Y.LT.ICR+IM1) THEN                
                  R(JCC+X,ICR+Y)=
     1           (R(JCC+X,ICR+IM1)-R(JCC+X,ICR-ICEND+IM1))/
     2            ICEND*(IM1-1+Y)+R(JCC+X,ICR-ICEND+IM1)
                ENDIF
                IF(ICR+Y.GT.ICR+IM1) THEN
                  R(JCC+X,ICR+Y)=
     1           (R(JCC+X,ICR+ICEND+IM1)-R(JCC+X,ICR+IM1))/
     2            ICEND*(Y-IM1)+R(JCC+X,ICR+IM1)
                ENDIF
              ENDIF
            ENDDO
            ENDDO
            ENDIF
          JCC=JCC+JCEND
        ENDDO 
        ICR=ICR+ICEND
      ENDDO
C
C      DO IR=1,NROWEXT
C      WRITE(1239,'(<NCOLEXT>F15.7)') (R(IC,IR),IC=1,NCOLEXT)
C      ENDDO
C      WRITE(1239,'(/)')      
C            
      DO IR=1,NROWEXT
      DO IC=1,NCOLEXT
      IF(IC.GT.JCEND.AND.IR.GT.ICEND.AND.
     1   IC.LE.NCOLEXT-JCEND.AND.IR.LE.NROWEXT-ICEND) THEN
         F(IC-JCEND,IR-ICEND)=R(IC,IR)
      ENDIF
      ENDDO
      ENDDO
C
C      DO IR=1,NROW
C      WRITE(1239,'(<NCOL>F15.7)') (F(IC,IR),IC=1,NCOL)
C      ENDDO
C      WRITE(1239,'(/)') 
C
      DEALLOCATE(R)
 100  RETURN
      END SUBROUTINE
C=================================================================================
      SUBROUTINE FMP3ETPRT(KSTP,KPER,IGRID)                             !seb NEW PRINT OUTPUT SUBROUTINE
C-----VERSION 1 01/27/13 FMP3FNRBD
C     ******************************************************************
C     PRINT TOTAL EVAPORATION AND TRANSPIRATION FOR MODEL CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE FMPMODULE,    ONLY:NFARMS,IFID,IFA,FMPOUT,ETOT,TTOT,IETPFL,
     +                       SFMP3PNT
      USE FMPBLK,       ONLY:TPL,TPU,ZERO,ZER
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,ITMUNI, 
     1                       HNEW, BOTM
      USE GWFBASMODULE, ONLY:DELT,ICBCFL,PERTIM,TOTIM
      IMPLICIT NONE
C     ------------------------------------------------------------------
C        ARGUMENTS:
C     ------------------------------------------------------------------
      INTEGER:: KSTP,KPER,IGRID
C     ------------------------------------------------------------------
C        LOCAL VARIABLES:
C     ------------------------------------------------------------------
      CHARACTER(12):: FMT
      CHARACTER(39):: TEXTET,TEXTETS
      CHARACTER(14):: TIMEUNIT
      CHARACTER(14):: TIME
      DATA TEXTET  /'EVAPORATION AND TRANSIPIRATION TOTALS  '/
      DATA TEXTETS /'EVAPORATION AND TRANSIPIRATION COMBINED'/
      INTEGER:: IRCH(NCOL,NROW)
      INTEGER:: I,IL,IR,IC,IBD,J,NF,NOPT                                  !FORMERLY IMPLICIT REAL
      DOUBLE PRECISION:: EVAP,TRAN
C     ------------------------------------------------------------------
      FMT='(*(G15.6))'                                                  !SET NUMBER FORMAT  *(Gw.d) means format as many columns as required  CHANGED FROM G12 TO G15 TO PREVENT OVERWRITING
C     ------------------------------------------------------------------
      CALL SFMP3PNT(IGRID)
C OUTPUT NOT REQUESTED RETURN TO MAIN FILE
      IF (IETPFL.EQ.0) RETURN
C
C1===== DEFINE TIME UNIT LABEL FOR "LABEL HEADER" ==========================================================
      IF(ITMUNI.EQ.1) TIMEUNIT='       SECONDS'
      IF(ITMUNI.EQ.2) TIMEUNIT='       MINUTES'
      IF(ITMUNI.EQ.3) TIMEUNIT='         HOURS'
      IF(ITMUNI.EQ.4) TIMEUNIT='          DAYS'
      IF(ITMUNI.EQ.5) TIMEUNIT='         YEARS'
      IF(TOTIM.GE.TPU.OR.TOTIM.GT.TPL) THEN
         WRITE(TIME,'(1PE14.7)') TOTIM
      ELSE
         WRITE(TIME,'(F14.2)') TOTIM
      END IF
C
      IBD=0
      IF(IETPFL.EQ.-1 .AND. ICBCFL.NE.0 .OR. IETPFL.EQ.1) IBD=1
      IF(IETPFL.EQ.-2 .AND. ICBCFL.NE.0 .OR. IETPFL.EQ.2) IBD=2
      IF(IETPFL.EQ.-3 .AND. ICBCFL.NE.0 .OR. IETPFL.EQ.3) IBD=3
      IF(IETPFL.EQ.4) IBD=4
      

C
C2B-----LOOK FOR HIGHEST LAYER THAT ETOT AND TTOT ORIGINATE FROM
      IF (NLAY.GT.1)THEN    !if only 1 layer no need to find top layer
      ROW_LP: DO IR=1,NROW
      COL_LP: DO IC=1,NCOL
C
C2B1----LOOP THROUGH CELLS IN A VERTICAL COLUMN TO FIND WHERE TO PLACE FARM NET RECHARGE.
      IRCH(IC,IR)=1
      LAY_LP: DO IL=1,NLAY
C
C2B2----IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
      IF(IBOUND(IC,IR,IL).LT.0) EXIT LAY_LP
C
C2B3----IF CELL IS INACTIVE MOVE DOWN TO NEXT CELL.
      IF(IBOUND(IC,IR,IL).EQ.0 .OR. HNEW(IC,IR,IL).LE.BOTM(IC,IR,IL)) 
     1                                                    CYCLE LAY_LP
      IRCH(IC,IR)=IL
      EXIT LAY_LP
      END DO LAY_LP
      END DO COL_LP
      END DO ROW_LP
      END IF
C
C
C3===== PRINT 2D-ARRAY OF CELL-BY-CELL COMBINED EVAPORATION AND TRANSPIRATION  ===============================
C
C3A-----WRITE HEADER
      IF(IBD.EQ.1 .AND. LSTCHK(3) ) THEN
      WRITE(FMPOUT%UNIT(12),1) TEXTETS,KPER,KSTP,
     1                         NCOL,NROW,NLAY,TIME,TIMEUNIT
    1 FORMAT(1X,//,1X,A,'   PERIOD',I5,'   STEP',I5,',',I5,' COLUMNS,',
     1I5,' ROWS,',I3,' LAYERS',7X,'ELAPSED TIME',A14,1X,A14,/)
C
C3B-----WRITE DATA AS TWO OR THREE RECORDS CONTAINING ONE VALUE PER LAYER.
      IF(NLAY.EQ.1) THEN
C
C-------WRITE ONE RECORD OF EVAPORATION+TRANSPIRATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(ETOT(J,I)+TTOT(J,I),J=1,NCOL)
      END DO
C
      ELSE
C
C3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS HAVE ET.
C       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS EVAPORATION+TRANSPIRATION).
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(IRCH(J,I),J=1,NCOL)
      END DO
C-------WRITE ONE RECORD OF EVAPORATION+TRANSPIRATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(ETOT(J,I)+TTOT(J,I),J=1,NCOL)
      END DO
c
      END IF
      END IF
C3===== PRINT 2D-ARRAY OF CELL-BY-CELL SEPARATE EVAPORATION AND TRANSPIRATION  ===============================
C
C3A-----WRITE HEADER
      IF(IBD.EQ.2 .OR. IBD.EQ.4 .AND. LSTCHK(3)) THEN
      WRITE(FMPOUT%UNIT(12),1) TEXTET,KPER,KSTP,
     1                         NCOL,NROW,NLAY,TIME,TIMEUNIT
C
C3B-----WRITE DATA AS TWO OR THREE RECORDS CONTAINING ONE VALUE PER LAYER.
      IF(NLAY.EQ.1) THEN
C
C-------WRITE ONE RECORD OF EVAPORATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(ETOT(J,I),J=1,NCOL)
      END DO
C------WRITE ONE RECORD OF TRANSIPIRATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(TTOT(J,I),J=1,NCOL)
      END DO
      ELSE
C
C3B2----WRITE TWO RECORDS WHEN MULTIPLE LAYERS HAVE ET.
C       (FIRST RECORD CONTAINS LAYER NUMBERS; SECOND RECORD CONTAINS EVAPORATION; THIRD RECORD HAS TRANSPIRATION).
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(IRCH(J,I),J=1,NCOL)
      END DO
C-------WRITE ONE RECORD OF EVAPORATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(ETOT(J,I),J=1,NCOL)
      END DO
C------WRITE ONE RECORD OF TRANSIPIRATION VALUES IF ONLY ONE LAYER
      DO I=1,NROW
        WRITE(FMPOUT%UNIT(12),FMT)(TTOT(J,I),J=1,NCOL)
      END DO
      END IF
      END IF
C
C4B---- PRINT CUMULATIVE EVAPOTRANSPIRATION FLOW RATE FOR EACH FARM TO ASCII FILE
C       (TO BUILD TIME SERIES FOR FARM WELLS: PER,STP,AND TIME ARE INCLUDED FOR EACH FARM WELL)
C       (FOR EACH TIME STEP)
C
C4B1----WRITE HEADER TO ASCII FILE
      IF(IBD.EQ.3 .OR. IBD.EQ.4 .AND. LSTCHK(3)) THEN
      IF(KPER.EQ.1.AND.KSTP.EQ.1) WRITE(FMPOUT%UNIT(13),33) TIMEUNIT
   33 FORMAT(2X,'PER',2X,'STP',A14,4X,
     1      'FARM ID           EVAPOARATION          TRANSPIRATION',
     2      '     EVAPOTRANSPIRATION')
C
C4B2----WRITE LIST OF CUMULATIVE EVAPORATION AND TRANSPIRATION FOR EACH FARM
        DO NF=1,NFARMS
          EVAP=0D0
          TRAN=0D0        
          DO IR=1,NROW
          DO IC=1,NCOL
             IF(IFID(IC,IR).EQ.IFA(NF))    THEN
             EVAP=EVAP+ETOT(IC,IR)
             TRAN=TRAN+TTOT(IC,IR)           
             ENDIF
          ENDDO
          ENDDO
        WRITE(FMPOUT%UNIT(13),64) KPER,KSTP,TIME,IFA(NF),
     1                           EVAP,TRAN,EVAP+TRAN   
        END DO
   64 FORMAT(2I5,A14,I11,3G23.8)  
      END IF      
C8===== RETURN ==============================================================================================
      RETURN
      END SUBROUTINE
      !
      END MODULE FMPMAIN