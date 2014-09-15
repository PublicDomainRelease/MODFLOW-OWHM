      MODULE GWFDRTMODULE
        INTEGER,SAVE,POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER,SAVE,POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        INTEGER,SAVE,POINTER   ::PRTFIL                                 !seb FLAG TO PRINT TO EXTERNAL FILE
        REAL,         SAVE, DIMENSION(:,:), POINTER,CONTIGUOUS::DRTF
        CHARACTER*16, SAVE, DIMENSION(:),   POINTER,CONTIGUOUS::DRTAUX
      TYPE GWFDRTTYPE
        INTEGER, POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER, POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        INTEGER,POINTER    ::PRTFIL                                     !seb FLAG TO PRINT TO EXTERNAL FILE
        REAL,         DIMENSION(:,:), POINTER,CONTIGUOUS::DRTF
        CHARACTER*16, DIMENSION(:),   POINTER,CONTIGUOUS::DRTAUX
      END TYPE
      TYPE(GWFDRTTYPE), SAVE :: GWFDRTDAT(10)
      END MODULE GWFDRTMODULE



      SUBROUTINE GWF2DRT7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ PARAMETERS FOR DRAINS AND
C     RETURN FLOWS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNIT           !seb ADDED IUNIT TO CHECK FOR FMP, SWR, and SFR PACKAGES BEING AVAILIBLE
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX,
     2                       PRTFIL
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
      INTEGER:: IUNITFMP,IUNITSWR,IUNITSFR                              !seb ADDED INUIT VARIABLES FOR FMP AND SWR LINK
      
      
      ALLOCATE(NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB)
      ALLOCATE(NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT)
      ALLOCATE(PRTFIL)
      PRTFIL=0
C
C1------IDENTIFY PACKAGE AND INITIALIZE NDRTCL.
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,1)IN
      ENDIF
    1 FORMAT(1X,/
     &1X,'DRT7 -- DRAIN RETURN PACKAGE, VERSION 7, 2/28/2006',/,
     &' INPUT READ FROM UNIT ',I4)
      NDRTCL=0
      NDRTNP=0
      IDRTFL=0
C
C2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
C     READ COMMENTS (ITEM 0)
      CALL URDCOM(IN,IOUT,LINE)
C     READ ITEM 1
      IF (IFREFM.EQ.0) THEN
        READ(LINE,'(4I10)') MXADRT,IDRTCB,NPDRT,MXL
        LLOC=41
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXADRT,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDRTCB,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPDRT,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
      ENDIF
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,3) MXADRT
      ENDIF
    3 FORMAT(1X,'MAXIMUM OF ',I6,
     &' ACTIVE DRAINS WITH RETURN FLOW AT ONE TIME')
      IF(LSTCHK(3)) THEN
        IF (IDRTCB.LT.0) WRITE(IOUT,7)
      ENDIF
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(LSTCHK(3)) THEN
        IF (IDRTCB.GT.0) WRITE(IOUT,8) IDRTCB
      ENDIF
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
      IF (NPDRT.GT.0) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,9) NPDRT,MXL
        ENDIF
    9   FORMAT(1X,I5,' Named Parameters     ',I5,' List entries')
      ELSE
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,'(A)') ' No named parameters'
        ENDIF
      END IF

C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      NAUX=0
      NOPRDT=0
      ALLOCATE (DRTAUX(20))
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     &       LINE(ISTART:ISTOP).EQ.'AUX') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF (NAUX.LT.20) THEN
          NAUX=NAUX+1
          DRTAUX(NAUX)=LINE(ISTART:ISTOP)
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,12) DRTAUX(NAUX)
          ENDIF
   12     FORMAT(1X,'AUXILIARY DRAIN-RETURN FLOW VARIABLE: ',A)
        ENDIF
        GOTO 10
      ELSEIF (LINE(ISTART:ISTOP).EQ.'RETURNFLOW') THEN
        IDRTFL=4
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,13)
        ENDIF
        GOTO 10
   13   FORMAT(1X,'RETURN FLOW OPTION IS SELECTED')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         IF(LSTCHK(3)) THEN
           WRITE(IOUT,14)
         ENDIF
   14    FORMAT(1X,'LISTS OF DRAIN-RETURN CELLS WILL NOT BE PRINTED')
         NOPRDT = 1
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'PRINTFILE') THEN                   !seb ADDED FEATURE TO PRINT FLOW RATES TO A SEPARATE FILE
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,PRTFIL,R,IOUT,IN)
         IF(LSTCHK(3)) WRITE(IOUT,15) PRTFIL
   15    FORMAT(1X, 'LISTS OF DRAIN-RETURN CELLS WILL BE PRINTED TO ',
     +              'EXTERNAL FILE ON UNIT NUMBER: ',I6)
         GO TO 10
      ENDIF
      NDRTVL=5+NAUX+2+IDRTFL
C
C4------ALLOCATE SPACE FOR THE DRTF ARRAY.
      IDRTPB=MXADRT+1
      MXDRT=MXADRT+MXL
      ALLOCATE (DRTF(NDRTVL,MXDRT))
      DRTF=0.0                                                          !seb ADDED ZERO INITIALIATION
C
C5------READ NAMED PARAMETERS.
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,500) NPDRT
      ENDIF
  500 FORMAT(1X,//1X,I5,' Drain-return parameters')
      IF (NPDRT.GT.0) THEN
        NAUX=NDRTVL-5-2-IDRTFL
        LSTSUM=IDRTPB
        ITERPU = 1
        IF (NOPRDT .EQ.1) ITERPU = 99
        DO 100 K=1,NPDRT
          LSTBEG=LSTSUM
C5A-----READ ITEM 2
          CALL UPARLSTRP(LSTSUM,MXDRT,IN,IOUT,IP,'DRT','DRT',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C5B-----ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 50 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,1)
            ENDIF
C5C-----READ ITEM 3
            CALL SGWF2DRT7LR(NLST,DRTF,LB,NDRTVL,MXDRT,IN,IOUT,
     &                       DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                       IDRTFL)
            LB = LB+NLST
   50     CONTINUE
  100   CONTINUE
      ENDIF
C
      IF(ANY(DRTF(6,:)<0.))THEN                                          !seb CHECK TO SEE IF LINKED PACKAGES ARE ACTIVE OR FLAG AS ERROR
       IUNITFMP=IUNIT(61)
       IUNITSWR=IUNIT(64)
       IUNITSFR=IUNIT(44)
      DO I=1,MXDRT
       KR=DRTF(6,I)
       IF (    KR.EQ.-1 .AND. IUNITFMP.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO FMP REQUIRES FMP TO BE ACTIVE'
        END IF
        CALL USTOP(' ')
       ELSEIF (KR.EQ.-2 .AND. IUNITSWR.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO SWR REQUIRES SWR TO BE ACTIVE'
        END IF
        CALL USTOP(' ')
       ELSEIF (KR.EQ.-3 .AND. IUNITSFR.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO SFR REQUIRES SFR TO BE ACTIVE'
        END IF
        CALL USTOP(' ')
       END IF
      END DO
      END IF
C
C6------RETURN
      CALL SGWF2DRT7PSV(IGRID)
      RETURN
      END SUBROUTINE
C-------SUBROUTINE GWF2DRT7AD
      SUBROUTINE GWF2DRT7AD(KSTP,Igrid)
C     *****************************************************************
C     APPLY EFFECTS OF SUBSIDENCE ON DRAIN FLOWS
C     VERSION  7.2.00: July 17, 2012 rth
C     *****************************************************************
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF,IDRTFL
      USE GLOBAL,       ONLY: IBOUND,SUBLNK
      USE GWFSUBMODULE, ONLY: DVZ,LPFLNK,DVZC
      IMPLICIT NONE
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER KSTP,Igrid,L,IL,IR,IC
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
C
      IF(SUBLNK) THEN
        DO 100 L=1,NDRTCL
C       
C1--------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
          IL=DRTF(1,L)
          IR=DRTF(2,L)
          IC=DRTF(3,L)
C       
C2---------IF THE CELL IS EXTERNAL SKIP IT.
          IF (IBOUND(IC,IR,IL).LE.0) GOTO 100
C       
C3---------IF THE CELL IS INTERNAL GET THE DRAIN DATA and subtract the displacement from that cell.
          DRTF(4,L)=DRTF(4,L) - DVZ(IC,IR,IL)
100     CONTINUE 
      END IF
      RETURN
      END SUBROUTINE
C--------------------------------------------------------------------------
      SUBROUTINE GWF2DRT7RP(IN,IGRID)
C     ******************************************************************
C     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION.  IF THE
C     RETURNFLOW OPTION IS SELECTED, READ RECIPIENT CELL AND PROPORTION.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNIT           !seb ADDED IUNIT
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX
C     ------------------------------------------------------------------
      INTEGER:: IUNITFMP,IUNITSWR,IUNITSFR                              !seb ADDED INUIT VARIABLES FOR FMP AND SWR LINK

      CALL SGWF2DRT7PNT(IGRID)
C
C1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF (NPDRT.GT.0) THEN
        IF (IFREFM.EQ.0) THEN
          READ(IN,'(2I10)') ITMP,NP
        ELSE
          READ(IN,*) ITMP,NP
        ENDIF
      ELSE
        NP=0
        IF (IFREFM.EQ.0) THEN
          READ(IN,'(I10)') ITMP
        ELSE
          READ(IN,*) ITMP
        ENDIF
      ENDIF
C
C------CALCULATE SOME CONSTANTS
      NAUX=NDRTVL-5-2-IDRTFL
      ITERPU = 1
      IOUTU = IOUT
      IF (NOPRDT.EQ.1) THEN
        ITERPU = 99
        IOUTU = -IOUT
      ENDIF
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER DRAIN-RETURN CELLS.
      IF (ITMP.LT.0) THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,7)
        ENDIF
    7   FORMAT(1X,/,
     &' REUSING NON-PARAMETER DRAIN-RETURN CELLS FROM',
     &' LAST STRESS PERIOD')
      ELSE
        NDRTNP=ITMP
      ENDIF
C
C3------IF THERE ARE NEW NON-PARAMETER DRAIN-RETURN CELLS, READ THEM.
      MXADRT=IDRTPB-1
      IF (ITMP.GT.0) THEN
        IF (NDRTNP.GT.MXADRT) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,500) NDRTNP,MXADRT
          ENDIF
  500     FORMAT(1X,/1X,'THE NUMBER OF ACTIVE DRT DRAINS (',I6,
     &           ') IS GREATER THAN MXADRT(',I6,')')
          CALL USTOP(' ')
        ENDIF
        CALL SGWF2DRT7LR(NDRTNP,DRTF,1,NDRTVL,MXDRT,IN,IOUT,
     &                   DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                   IDRTFL)
      ENDIF
      NDRTCL=NDRTNP
C
C1C-----IF THERE ARE ACTIVE DRT PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRT')
      IF (NP.GT.0) THEN
        NREAD=NDRTVL-2
        DO 30 N=1,NP
          CALL SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     &                     NDRTCL,DRTAUX,20,NAUX,IDRTFL)
   30   CONTINUE
      ENDIF
C
C     COUNT NUMBER OF DRAIN-RETURN CELLS THAT CAN HAVE RETURN FLOW
      NRFLOW = 0
      IF (IDRTFL.GT.0) THEN
        DO 40 I=1,NDRTCL
          IF (DRTF(6,I) .GT. 0.0) NRFLOW = NRFLOW + 1
   40   CONTINUE
      ENDIF
C
C3------PRINT NUMBER OF DRAIN-RETURN CELLS IN CURRENT STRESS PERIOD.
      IF(LSTCHK(3)) THEN
        WRITE (IOUT,510) NDRTCL
      ENDIF
  510 FORMAT(1X,/1X,I6,' DRAIN-RETURN CELLS')
C
Cseb CHECK TO SEE IF LINKED PACKAGES ARE ACTIVE OR FLAG AS ERROR
      IF (IDRTFL.GT.0) THEN
       IUNITFMP=IUNIT(61)
       IUNITSWR=IUNIT(64)
       IUNITSFR=IUNIT(44)
       IF(ANY(NINT(DRTF(6,:)).EQ.-1).AND.IUNITFMP.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO FMP REQUIRES FMP TO BE ACTIVE'
        END IF
        CALL USTOP(' ')
       END IF
       IF(ANY(NINT(DRTF(6,:)).EQ.-2).AND.IUNITSWR.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO SWR REQUIRES SWR TO BE ACTIVE'
        END IF
        CALL USTOP(' ')      
       END IF
       IF(ANY(NINT(DRTF(6,:)).EQ.-3).AND.IUNITSFR.EQ.0)THEN
        IF(LSTCHK(1)) THEN
         WRITE(IOUT,*) 'ERROR DRT LINK TO SFR REQUIRES SFR TO BE ACTIVE'
        END IF
        CALL USTOP(' ')      
       END IF
      END IF
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRT7FM(IGRID)
C     ******************************************************************
C     ADD DRAIN-RETURN FLOW TO SOURCE TERMS FOR BOTH DRAIN-RETURN CELLS
C     AND RECIPIENT CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:HNEW,HCOF,RHS,IBOUND,LSTCHK
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF,IDRTFL
      USE FMPMODULE,    ONLY:IFID,LFID,DRTFLOW
C
      INTEGER::FID,I
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------IF NDRTCL<=0 THERE ARE NO DRAINS. RETURN.
      IF (NDRTCL.LE.0) RETURN
C seb CHECK TO SEE IF THERE ARE ANY LINKS TO FMP AND INITIALIZE THE FLOWS TO ZERO
      IF(ANY(NINT(DRTF(6,:)).EQ.-1))  DRTFLOW%FLO=0D0
C
C2------PROCESS EACH CELL IN THE DRAIN-RETURN CELL LIST.
      DO 100 L=1,NDRTCL
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
        IL=DRTF(1,L)
        IR=DRTF(2,L)
        IC=DRTF(3,L)
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
        IF (IBOUND(IC,IR,IL).LE.0) GOTO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
        EL=DRTF(4,L)
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
        IF (HNEW(IC,IR,IL).LE.DBLE(EL)) GOTO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
        C=DRTF(5,L)
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL
        IF (IDRTFL.GT.0) THEN
          ILR = DRTF(6,L)
          IRR = DRTF(7,L)                                               !NOTE THAT IF FLOW IS ROUTED TO SWR THE REACH IS STORED IN IRR seb
          ICR = DRTF(8,L)
          RFPROP = DRTF(9,L)
          H = HNEW(IC,IR,IL)
          IF (ILR.GT.0) THEN
            IF (IBOUND(ICR,IRR,ILR) .GT. 0) THEN
              RHS(ICR,IRR,ILR) = RHS(ICR,IRR,ILR)
     +                           - RFPROP*C*(H-EL)
            END IF
          ELSEIF (ILR.EQ.-1) THEN  !SEND WATER TO FMP
            IF(IRR.EQ.0)THEN                                            !seb USE OVER LYING FARM
             FID=IFID(IC,IR)
             IF(FID.NE.0)THEN
              IF(LFID(FID))THEN
                DO I=1, NFARMS                                          !I DOES NOT REPRSENT FARM TO LOOK FOR, IT IS ONLY AN INDEX TO SEARCH FOR FARM FN, ie DRTFLOW(1:NFARM) and %FID is the FARM ID
                  IF(DRTFLOW(I)%FID.EQ.NF)THEN
                 DRTFLOW(I)%FLO=DRTFLOW(I)%FLO+DBLE(RFPROP*C*(H-EL))    !seb FLOW THAT IS SENT TO FMP
                    EXIT
                  END IF
                  IF(DRTFLOW(I)%FID.EQ.0) THEN
                    IF(LSTCHK(1)) WRITE(IOUT,'(3(A,I5),/A)')
     +        'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        'COULD NOT FIND FARM ID ',FID,
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
                    WRITE(*,'(3(A,I5),/A)')
     +        'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        'COULD NOT FIND FARM ID ',FID,
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
                    EXIT                                                !NO FARMS WITH A DRT FOUND
                  END IF
                END DO
              ELSE
               WRITE(*,'(3(A,I5),A,/A)')
     +        'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        'ROUTED WATER TO FARM ID ',FID,' THAT IS NOT ACTIVE.',
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
               IF(LSTCHK(1)) WRITE(IOUT,'(3(A,I5),A,/A)')
     +        ' WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        ' ROUTED WATER TO FARM ID ',FID,' THAT IS NOT ACTIVE.',
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
              END IF
             ELSE
              WRITE(*,'(2(A,I5),A,2(A,I5),/A)')
     +        'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        ' CAN NOT ROUTE WATER TO OVERLAYING FARM', 
     +        ' BECAUSE THERE IS NO FARM LOCATED AT ROW',IR,' COL',IC,
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
               IF(LSTCHK(1)) WRITE(*,'(2(A,I5),A,2(A,I5),/A)')
     +        'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +        ' CAN NOT ROUTE WATER TO OVERLAYING FARM', 
     +        ' BECAUSE THERE IS NO FARM LOCATED AT ROW',IR,' COL',IC,
     +        'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
             END IF
            ELSE                                                        !FARM LINK IS SPECIFIED IN IRR
             FID=IRR
             IF(LFID(FID))THEN
               DO I=1, NFARMS                                           !I DOES NOT REPRSENT FARM TO LOOK FOR, IT IS ONLY AN INDEX TO SEARCH FOR FARM FN, ie DRTFLOW(1:NFARM) and %FID is the FARM ID
                IF(DRTFLOW(I)%FID.EQ.NF)THEN
                DRTFLOW(I)%FLO=DRTFLOW(I)%FLO+DBLE(RFPROP*C*(H-EL))     !seb FLOW THAT IS SENT TO FMP
                  EXIT
                END IF
                IF(DRTFLOW(I)%FID.EQ.0) THEN
                  IF(LSTCHK(1)) WRITE(IOUT,'(3(A,I5),/A)')
     +       'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +       'COULD NOT FIND FARM ID ',FID,
     +       'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
                  WRITE(*,'(3(A,I5),/A)')
     +       'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +       'COULD NOT FIND FARM ID ',FID,
     +       'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
                  EXIT  !NO FARMS WITH A DRT FOUND
                END IF
               END DO
             ELSE
              WRITE(*,'(3(A,I5),A,/A)')
     +       'WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +       'ROUTED WATER TO FARM ID ',FID,' THAT IS NOT ACTIVE.',
     +       'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
              IF(LSTCHK(1)) WRITE(IOUT,'(3(A,I5),A,/A)')
     +       ' WARNING DRAIN LOCATED AT ROW',IR,' COL ',IC,
     +       ' ROUTED WATER TO FARM ID ',FID,' THAT IS NOT ACTIVE.',
     +       'WATER IS SENT OUT OF THE MODELED SYSTEM/LOST/NOT ROUTED'
             END IF
            END IF
          ELSEIF (ILR.EQ.-2) THEN                                       !SWR LINK
           IFLAG=100                                                    !VALUE MAY NEED TO BE CHANGED TO FIT SWR NEEDS
           CALL GWF2SWR7EX_V(IGRID,IFLAG,IRR, RFPROP*C*(H-EL) )         !ADD FLOW TO SWR REACH IRR
          ELSEIF (ILR.EQ.-3) THEN                                       !SFR LINK NOT SUPPORTED YET
        WRITE(*,*)'SFR LINK NOT SUPPORTED YET WATER IS LOST/NOT ROUTED'
        IF(LSTCHK(3))
     +WRITE(IOUT,*)'SFR LINK NOT SUPPORTED YET WATER IS LOST/NOT ROUTED'
           CYCLE
          ENDIF
        ENDIF
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAIN-RETURN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL ,      ONLY: IOUT,HNEW,IBOUND,BUFF,NCOL,NROW,NLAY
      USE GLOBAL,       ONLY:LSTCHK
      USE GWFBASMODULE, ONLY: MSUM,VBNM,VBVL,PERTIM,TOTIM,DELT,ICBCFL,
     1                        IAUXSV
      USE GWFDRTMODULE, ONLY: DRTF,NDRTCL,MXDRT,IDRTCB,NDRTVL,IDRTFL,
     1                        NRFLOW,DRTAUX,PRTFIL
      USE FMPMODULE,    ONLY:IFID                                       !seb ADDED CONNECTION TO FMP FOR OUTPUT WITH PRINTFILE OPTION
C
      DOUBLE PRECISION HHNEW,EEL,CC,CEL,RATIN,RATOUT,QQ
      CHARACTER*16 TEXT
      CHARACTER*7  FORMTEST                                             !seb ADDED VARIABLE TO CHECK IF FILE IS BINARY
      DATA TEXT /'    DRAINS (DRT)'/
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF (IDRTCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF (IDRTCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF (IBD.EQ.2) THEN
        NAUX = NDRTVL - 5 - 2 - IDRTFL
        IF (IAUXSV.EQ.0) NAUX = 0
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,DRTAUX,IDRTCB,NCOL,NROW,NLAY,
     &              NDRTCL+NRFLOW,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C
C3------CLEAR THE BUFFER.
      DO 30 IL=1,NLAY
        DO 20 IR=1,NROW
          DO 10 IC=1,NCOL
            BUFF(IC,IR,IL)=ZERO
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C4------IF THERE ARE NO DRAIN-RETURN CELLS THEN DO NOT ACCUMULATE FLOW.
      IF (NDRTCL.LE.0) GOTO 200
C
C5------LOOP THROUGH EACH DRAIN-RETURN CELL, CALCULATING FLOW.
      DO 100 L=1,NDRTCL
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING DRAIN.
        IL=DRTF(1,L)
        IR=DRTF(2,L)
        IC=DRTF(3,L)
        Q=ZERO
        ILR=0
        QIN=ZERO                                                        !MOVED QIN TO OUTSIDE OF LOOP TO ENSURE PROPER PRINTING
        IF (IDRTFL.GT.0) THEN
          !QIN=ZERO
          ILR = DRTF(6,L)
          IF (ILR.NE.0) THEN  ! Condition added 8/26/08 ERB
            IRR = DRTF(7,L)
            ICR = DRTF(8,L)
            IF ( IBOUND(ICR,IRR,ILR) .LE. 0 ) ILR = 0
            IF ( ILR.EQ.-1  .OR.  ILR.EQ.-2 ) ILR = 0                   !WATER IS SENT TO FMP OR SWR, DO NOT ACCOUNT FOR FLOW RETURNED TO SYSTEM. THOSE PACKAGES WILL HANDEL IT.
          ENDIF
        END IF
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
        IF (IBOUND(IC,IR,IL).LE.0) GOTO 99
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN-RETURN LIST.
        EL=DRTF(4,L)
        EEL=EL
        C=DRTF(5,L)
        HHNEW=HNEW(IC,IR,IL)
C
C5D-----IF HEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
        IF (HHNEW.GT.EEL) THEN
          CC=C
          CEL=C*EL
          QQ=CEL - CC*HHNEW                                             !Q=C(EL-HHNEW)
          Q=QQ
          RATOUT=RATOUT-QQ
          IF (IDRTFL.GT.0) THEN
            IF (ILR.GT.0) THEN
              RFPROP = DRTF(9,L)
              QQIN = RFPROP*(CC*HHNEW-CEL)
              QIN = QQIN
              RATIN = RATIN + QQIN
            ENDIF
          ENDIF
        ENDIF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRTCB<0).
        IF (IBD.LT.0) THEN
          IF(LSTCHK(3)) THEN
            IF (IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
          ENDIF
   61     FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,62) L,IL,IR,IC,Q
          ENDIF
   62     FORMAT(1X,'DRAIN ',I6,'   LAYER ',I3,'   ROW ',I5,
     &       '   COL ',I5,'   RATE ',1PG15.6)
          IF (ILR.NE.0) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,550) L,ILR,IRR,ICR,QIN
            ENDIF
  550       FORMAT(1X,'DRAIN ',I6,' RETURN:  LAYER ',I3,'   ROW ',I5,
     &       '   COL ',I5,'   RATE ',1PG15.6)
          ENDIF
          IF(PRTFIL.EQ.0) IBDLBL=1                                      !seb IF PRINTFILE NOT USED THEN NO LONGER PRINT HEADER
        ENDIF
Cseb ADDED OPTION TO PRINT FLOWS TO FILE WITH KEYWORD "PRINTFILE"
        IF(PRTFIL.NE.0) THEN
          INQUIRE(UNIT=PRTFIL,FORMATTED=FORMTEST)
          IF(TRIM(ADJUSTL(FORMTEST)).EQ.'YES')THEN
           IF (IBDLBL.EQ.0) WRITE(PRTFIL,63) TEXT,NDRTCL,KPER,KSTP,
     +                                       PERTIM,TOTIM,DELT
   63      FORMAT(1X,A,' NDRAINS ',I6,'   PERIOD ',I4,'   STEP ',I3,
     +            ' PERTIM ',1PG16.7,' TOTIM ',1PG16.7,' DELT ',1PG16.7)
           IF     (ILR.GT.0)  THEN
                WRITE(PRTFIL,64) L,IL,IR,IC,Q,' DRT ',ILR,IRR,ICR,QIN
           ELSEIF (ILR.EQ.0)  THEN
                WRITE(PRTFIL,64) L,IL,IR,IC,Q,' DRT ',0,0,0,0.0
           ELSEIF (ILR.EQ.-1) THEN
                IF(IRR.EQ.0)IRR=IFID(IC,IR)
                WRITE(PRTFIL,64) L,IL,IR,IC,Q,' FMP ',ILR,IRR,0,QIN
           ELSEIF (ILR.EQ.-2) THEN
                WRITE(PRTFIL,64) L,IL,IR,IC,Q,' SWR ',ILR,IRR,0,QIN
           END IF
   64      FORMAT(1X,I6,3I5,1PG15.6,A,3I5,1PG15.6)
          ELSE                                                          !UNFORMATTED/BINARY WRITE
           IF (IBDLBL.EQ.0) WRITE(PRTFIL) TEXT,KPER,KSTP,NDRTCL
           IF     (ILR.GT.0)  THEN
             WRITE(PRTFIL) L,IL,IR,IC,Q,' DRT ',ILR,IRR,ICR,QIN
           ELSEIF (ILR.EQ.0)  THEN
             WRITE(PRTFIL) L,IL,IR,IC,Q,' DRT ',0,0,0,0.0
           ELSEIF (ILR.EQ.-1) THEN
             IF(IRR.EQ.0)IRR=IFID(IC,IR)
             WRITE(PRTFIL) L,IL,IR,IC,Q,' FMP ',ILR,IRR,0,QIN
           ELSEIF (ILR.EQ.-2) THEN
             WRITE(PRTFIL) L,IL,IR,IC,Q,' SWR ',ILR,IRR,0,QIN
           END IF
          END IF
          IBDLBL=1
        END IF
C
C5F-----ADD Q TO BUFFER.
        BUFF(IC,IR,IL) = BUFF(IC,IR,IL) + Q
        IF (IDRTFL.GT.0 .AND. ILR.GT.0)
     &      BUFF(ICR,IRR,ILR) = BUFF(ICR,IRR,ILR) + QIN
C
C5G-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  OR IF
C5G-----RETURNING THE FLOW IN THE DRTF ARRAY, COPY FLOW TO DRTF.
   99   IF (IBD.EQ.2) THEN
          CALL UBDSVB(IDRTCB,NCOL,NROW,IC,IR,IL,Q,DRTF(:,L),NDRTVL,NAUX,
     &                10,IBOUND,NLAY)
          IF (IDRTFL.NE.0 .AND. ILR.GT.0)
     &      CALL UBDSVB(IDRTCB,NCOL,NROW,ICR,IRR,ILR,QIN,DRTF(:,L),
     &                  NDRTVL,NAUX,10,IBOUND,NLAY)
        ENDIF
        DRTF(NDRTVL,L) = Q
        DRTF(NDRTVL-1,L) = QIN
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF (IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDRTCB,BUFF,NCOL,NROW,
     &                          NLAY,IOUT)
C
C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 CONTINUE
      RIN = RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2DRT7LR(NLIST,DRTF,LSTBEG,NDRTVL,MXDRT,
     &                       INPACK,IOUT,DRTAUX,NCAUX,NAUX,IFREFM,
     &                       NCOL,NROW,NLAY,ITERP,IDRTFL)
C     ******************************************************************
C     Read and print a list of drain and optional associated
C     return-flow recipient cells.  NAUX of the values in the list are
C     optional -- auxiliary data.
C     ******************************************************************
      USE GLOBAL,      ONLY:LSTCHK
      CHARACTER*57 LABEL1, LABEL2, LABEL3
      CHARACTER*16 DRTAUX(NCAUX)
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER*700 FNAME
      CHARACTER*700 LINE
      DATA NUNOPN/99/
C     ------------------------------------------------------------------
C
      IERR = 0
      ISCLOC1 = 5
      ISCLOC2 = 5
      IN=INPACK
      ICLOSE=0
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR'
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
C
C  Check for and decode EXTERNAL and SFAC records.
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
        IN=I
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,510) IN
        ENDIF
  510   FORMAT(1X,'Reading list on unit ',I4)
        READ(IN,'(A)') LINE
      ELSEIF (LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        FNAME=LINE(ISTART:ISTOP)
        IN=NUNOPN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,520) IN,FNAME
        ENDIF
  520   FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
        OPEN(UNIT=IN,FILE=FNAME)
        ICLOSE=1
        READ(IN,'(A)') LINE
      ENDIF
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'SFAC') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,530) SFAC
        ENDIF
  530   FORMAT(1X,'LIST SCALING FACTOR= ',1PG12.5)
        IF (ISCLOC1.EQ.ISCLOC2) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,540) ISCLOC1
          ENDIF
  540     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD ',I2,')')
        ELSE
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,550) ISCLOC1,ISCLOC2
          ENDIF
  550     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS ',
     &           I2,'-',I2,')')
        ENDIF
        READ(IN,'(A)') LINE
      ENDIF
C
C  Write a label for the list.
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,'(1X)')
      ENDIF
      CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Read the list
      NREAD2=NDRTVL-2
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
      DO 100 II=LSTBEG,N
C  Read a line into the buffer.  (The first line has already been read
C  in order to scan for EXTERNAL and SFAC records.)
        IF (II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C  Read the non-optional values from the line.
        IF (IDRTFL.EQ.0) THEN
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,9F10.0)') K,I,J,(DRTF(JJ,II),JJ=4,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            DO 10 JJ=4,NREAD1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                    IOUT,IN)
   10       CONTINUE
          ENDIF
        ELSE
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,2F10.0,3I10,9F10.0)') K,I,J,
     &          (DRTF(JJ,II),JJ=4,5),KR,IR,JR,(DRTF(JJ,II),JJ=9,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(4,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(5,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KR,R,IOUT,IN)
            IF (KR.EQ.0 .AND. NREAD1.EQ.9 .AND. NAUX.EQ.0) THEN
              IR = 0
              JR = 0
              DRTF(9,II) = 0.0
            ELSE
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IR,R,IOUT,IN)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JR,R,IOUT,IN)
              DO 20 JJ=9,NREAD1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                      IOUT,IN)
   20         CONTINUE
            ENDIF
          ENDIF
          DRTF(6,II) = KR
          DRTF(7,II) = IR
          DRTF(8,II) = JR
        ENDIF
        DRTF(1,II)=K
        DRTF(2,II)=I
        DRTF(3,II)=J
        DO 50 ILOC=ISCLOC1,ISCLOC2
          DRTF(ILOC,II)=DRTF(ILOC,II)*SFAC
   50   CONTINUE
C
C  Read the optional values from the line
        IF (NAUX.GT.0) THEN
          DO 60 JJ=NREAD1+1,NREAD2
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),IOUT,
     &                  IN)
   60     CONTINUE
        ENDIF
C
C  Write the values that were read and that are not related to
C  return flow.
        NN=II-LSTBEG+1
        IF (IDRTFL.EQ.0) THEN
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,NREAD2)
          ENDIF
  570     FORMAT(1X,I6,I7,I7,I7,14G16.4)
        ELSE
          IF (NREAD2.GE.10) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5),
     &                      (DRTF(JJ,II),JJ=10,NREAD2)
            ENDIF
          ELSE
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5)
            ENDIF
          ENDIF
        ENDIF
C
C  Check for illegal grid location
        IF (K.LT.1 .OR. K.GT.NLAY) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,*) ' ERROR: Layer number is outside of the grid'
          ENDIF
          IERR = 1
        ENDIF
        IF (I.LT.1 .OR. I.GT.NROW) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,*) ' ERROR: Row number is outside of the grid'
          ENDIF
          IERR = 1
        ENDIF
        IF (J.LT.1 .OR. J.GT.NCOL) THEN
          IF(LSTCHK(1)) THEN
            WRITE(IOUT,*) ' ERROR: Column number is outside of the grid'
          ENDIF
          IERR = 1
        ENDIF
        IF (IERR.NE.0) CALL USTOP(' ')
  100 CONTINUE
C
C     Check and write data related to return-flow recipient cells
      IF (IDRTFL.GT.0) THEN
        IF(LSTCHK(3)) THEN
         IF(ANY(DRTF(6,:)<0))THEN                                       !seb CHECK TO SEE IF ANY LayR<0 WHICH INDICATES A LINKAGE TO FMP, SWR, or SFR
          WRITE(IOUT,'(/,1X,A,/,1X,2A)') LABEL2,LABEL3,' LINKED PACKAGE'
         ELSE
          WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
         END IF
        ENDIF
        NN = 0
        DO 110 II=LSTBEG,N
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
          IF(LSTCHK(3)) THEN
           IF(    KR.EQ.-1)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' FMP LINK'
           ELSEIF(KR.EQ.-2)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SWR LINK'
           ELSEIF(KR.EQ.-3)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SFR LINK'
           ELSE
            WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
           END IF
          ENDIF
  600     FORMAT(1X,I6,3I7,3I7,2X,F8.6)
  601     FORMAT(1X,I6,3I7,3I7,2X,F8.6,A)
C
C  Check for illegal grid location
          IF (KR.NE.0) THEN
            IF (KR.LT.-3 .OR. KR.GT.NLAY) THEN                          !seb CHANGED FROM IF (KR.LT.-0  TO PREVENT ERRORS ON USING NEW FLAGS
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,*) ' ERROR: Layer number is outside of the',
     &                      ' grid'
              ENDIF
              IERR = 1
            ENDIF
            IF ((IR.LT.1 .OR. IR.GT.NROW) .AND. KR.GT.0) THEN
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,*)' ERROR: Row number is outside of the grid'
              ENDIF
              IERR = 1
            ENDIF
            IF ((JR.LT.1 .OR. JR.GT.NCOL) .AND. KR.GT.0) THEN
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,*) ' ERROR: Column number is outside of the',
     &                      ' grid'
              ENDIF
              IERR = 1
            ENDIF
C
C  Check for invalid return-flow proportion
            IF (RFP.LT.0.0 .OR. RFP.GT.1.0) THEN
              IF(LSTCHK(1)) THEN
                WRITE(IOUT,590)
              ENDIF
  590         FORMAT(' ERROR: Proportion must be between 0.0 and 1.0')
              IERR = 1
            ENDIF
          ENDIF
C
C  If the proportion = 0 or KR = 0, set all indices and proportion to 0
          IF (KR.EQ.0 .OR. RFP.EQ.0.0) THEN
            DRTF(6,II) = 0.0
            DRTF(7,II) = 0.0
            DRTF(8,II) = 0.0
            DRTF(9,II) = 0.0
          ENDIF
          IF (IERR.NE.0) CALL USTOP(' ')
  110   CONTINUE
      ENDIF
C
      IF (ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     &                       NDRTCL,DRTAUX,NCAUX,NAUX,IDRTFL)
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GLOBAL,     ONLY:LSTCHK
      CHARACTER*3 PACK, PTYP
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER*57 LABEL1, LABEL2, LABEL3
      CHARACTER*16 DRTAUX(NCAUX)
      CHARACTER*700 LINE
      CHARACTER*10 CTMP1, CTMP2, CTMP3, CTMP4
C     ------------------------------------------------------------------
  500 FORMAT(/,' Parameter:  ',A)
  510 FORMAT(1X,'Parameter type conflict:',/
     &       1X,'Named parameter:',A,' was defined as type:',A,/
     &       1X,'However, this parameter is used in the ',A,
     &       ' file, so it should be type:',A)
  512 FORMAT(/,1X,'Blank instance name in the ',A,
     &       ' file for parameter ',A)
  514 FORMAT(3X,'Instance:  ',A)
  516 FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &       A,'" for parameter ',A)
  520 FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     &       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
  530 FORMAT(1X,I6,I7,I7,I7,14G16.4)
  550 FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &'" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &' -- STOP EXECUTION (SGWF2DRT7LS)')
  600 FORMAT(1X,I6,3I7,3I7,2X,F8.6)
  601 FORMAT(1X,I6,3I7,3I7,2X,F8.6,A)
C
      PACK = 'DRT'
      PTYP = 'DRT'
      IPVL1 = 5
      IPVL2 = 5
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE  '
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
      IOUT = ABS(IOUTU)
C
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,500) LINE(ISTART:ISTOP)
      ENDIF
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        IF(LSTCHK(1)) THEN
          WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        ENDIF
        CALL USTOP(' ')
      END IF
C
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,510) PARNAM(IP),PARTYP(IP),PACK,PTYP
            ENDIF
            CALL USTOP(' ')
          ENDIF
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
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
                WRITE(IOUT,512)PACK,PARNAM(IP)
              ENDIF
              CALL USTOP(' ')
            ENDIF
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,514) CTMP3
            ENDIF
            CALL UPCASE(CTMP3)
            DO 10 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 15
              ENDIF
   10       CONTINUE
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,516) PACK,CTMP3,PARNAM(IP)
            ENDIF
            CALL USTOP(' ')
   15       CONTINUE
          ENDIF
C
          IF (IACTIVE(IP).GT.0) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,550) PARNAM(IP)
            ENDIF
            CALL USTOP(' ')
          ENDIF
C
          IACTIVE(IP)=NI
C
          NDRTCL=NDRTCL+NLST
          IF(NDRTCL.GT.MXADRT) THEN
            IF(LSTCHK(1)) THEN
              WRITE(IOUT,520) NDRTCL,MXADRT
            ENDIF
            CALL USTOP(' ')
          ENDIF
C
C  Write label for list values
          IF (IOUTU.GT.0) CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Substitute values
          DO 60 I=1,NLST
            II=NDRTCL-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            DO 20 J=1,NREAD
              DRTF(J,II)=DRTF(J,III)
   20       CONTINUE
            DO 40 IPVL=IPVL1,IPVL2
              DRTF(IPVL,II)=DRTF(IPVL,II)*B(IP)
   40       CONTINUE
            IL=DRTF(1,II)
            IR=DRTF(2,II)
            IC=DRTF(3,II)
            IF (IOUTU.GT.0) THEN
              IF (IDRTFL.EQ.0) THEN
                IF(LSTCHK(3)) THEN
                  WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,NREAD)
                ENDIF
              ELSE
                IF (NREAD.GE.10) THEN
                  IF(LSTCHK(3)) THEN
                    WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5),
     &                            (DRTF(JJ,II),JJ=10,NREAD)
                  ENDIF
                ELSE
                  IF(LSTCHK(3)) THEN
                    WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
   60     CONTINUE
          GOTO 120
        ENDIF
  100 CONTINUE
C
      IF(LSTCHK(1)) THEN
        WRITE(IOUT,*) ' The ',PACK,
     &   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      ENDIF
      CALL USTOP(' ')
C
  120 CONTINUE
C
C     WRITE DATA RELATED TO RETURN-FLOW RECIPIENT CELLS
      IF (IDRTFL.GT.0 .AND. IOUTU.GT.0) THEN
        IF(LSTCHK(3)) THEN
         IF(ANY(DRTF(6,:)<0.))THEN                                       !seb CHECK TO SEE IF ANY LayR<0 WHICH INDICATES A LINKAGE TO FMP, SWR, or SFR
          WRITE(IOUT,'(/,1X,A,/,1X,2A)') LABEL2,LABEL3,' LINKED PACKAGE'
         ELSE
          WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
         END IF
        ENDIF
        NN = 0
        DO 140 II=NDRTCL-NLST+1,NDRTCL
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
          IF(LSTCHK(3)) THEN
           IF(    KR.EQ.-1)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' FMP LINK'
           ELSEIF(KR.EQ.-2)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SWR LINK'
           ELSEIF(KR.EQ.-3)THEN
             WRITE(IOUT,601) NN,K,I,J,KR,IR,JR,RFP,' SFR LINK'
           ELSE
            WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
           END IF
          ENDIF
  140   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE GWF2DRT7DA(IGRID)
C  Deallocate DRT MEMORY
      USE GWFDRTMODULE
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTCL)
      DEALLOCATE(GWFDRTDAT(IGRID)%MXDRT )
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTVL)
      DEALLOCATE(GWFDRTDAT(IGRID)%NDRTNP)
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTCB)
      DEALLOCATE(GWFDRTDAT(IGRID)%NPDRT )
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTPB)
      DEALLOCATE(GWFDRTDAT(IGRID)%IDRTFL)
      DEALLOCATE(GWFDRTDAT(IGRID)%NRFLOW)
      DEALLOCATE(GWFDRTDAT(IGRID)%NOPRDT)
      DEALLOCATE(GWFDRTDAT(IGRID)%DRTF  )
      DEALLOCATE(GWFDRTDAT(IGRID)%DRTAUX)
      DEALLOCATE(GWFDRTDAT(IGRID)%PRTFIL)
C
C NULLIFY THE LOCAL POINTERS
      IF (IGRID.EQ.1) THEN
        NDRTCL=>NULL()
        MXDRT =>NULL()
        NDRTVL=>NULL()
        NDRTNP=>NULL()
        IDRTCB=>NULL()
        NPDRT =>NULL()
        IDRTPB=>NULL()
        IDRTFL=>NULL()
        NRFLOW=>NULL()
        NOPRDT=>NULL()
        DRTF  =>NULL()
        DRTAUX=>NULL()
        PRTFIL=>NULL()  
      END IF
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PNT(IGRID)
C  Change DRT data to a different grid.
      USE GWFDRTMODULE
C
        NDRTCL=>GWFDRTDAT(IGRID)%NDRTCL
        MXDRT=>GWFDRTDAT(IGRID)%MXDRT
        NDRTVL=>GWFDRTDAT(IGRID)%NDRTVL
        NDRTNP=>GWFDRTDAT(IGRID)%NDRTNP
        IDRTCB=>GWFDRTDAT(IGRID)%IDRTCB
        NPDRT=>GWFDRTDAT(IGRID)%NPDRT
        IDRTPB=>GWFDRTDAT(IGRID)%IDRTPB
        IDRTFL=>GWFDRTDAT(IGRID)%IDRTFL
        NRFLOW=>GWFDRTDAT(IGRID)%NRFLOW
        NOPRDT=>GWFDRTDAT(IGRID)%NOPRDT
        DRTF=>GWFDRTDAT(IGRID)%DRTF
        DRTAUX=>GWFDRTDAT(IGRID)%DRTAUX
        PRTFIL=>GWFDRTDAT(IGRID)%PRTFIL
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PSV(IGRID)
C  Save DRT data for a grid.
      USE GWFDRTMODULE
C
        GWFDRTDAT(IGRID)%NDRTCL=>NDRTCL
        GWFDRTDAT(IGRID)%MXDRT=>MXDRT
        GWFDRTDAT(IGRID)%NDRTVL=>NDRTVL
        GWFDRTDAT(IGRID)%NDRTNP=>NDRTNP
        GWFDRTDAT(IGRID)%IDRTCB=>IDRTCB
        GWFDRTDAT(IGRID)%NPDRT=>NPDRT
        GWFDRTDAT(IGRID)%IDRTPB=>IDRTPB
        GWFDRTDAT(IGRID)%IDRTFL=>IDRTFL
        GWFDRTDAT(IGRID)%NRFLOW=>NRFLOW
        GWFDRTDAT(IGRID)%NOPRDT=>NOPRDT
        GWFDRTDAT(IGRID)%DRTF=>DRTF
        GWFDRTDAT(IGRID)%DRTAUX=>DRTAUX
        GWFDRTDAT(IGRID)%PRTFIL=>PRTFIL
C
      RETURN
      END
