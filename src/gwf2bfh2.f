      MODULE GWFBFHMODULE
         INTEGER,          SAVE, POINTER ::ISCHILD,IBOTFLG,IBFLG,NCPP 
         INTEGER,          SAVE, POINTER ::NPBNODES,NCBNODES,NTIMES
         INTEGER,          SAVE, POINTER ::NLAYP,NROWP,NCOLP
         INTEGER,          SAVE, POINTER ::IUBC,NGRIDS
         CHARACTER(LEN=17),SAVE, POINTER ::BTEXT
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPLBEG
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPRBEG
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPCBEG
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPLEND
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPREND
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPCEND
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::IBPFLG
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::IBB
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::NPINDX
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::KLAY
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::IROW
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::JCOL 
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::IFACEGN
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::KPLAY 
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::IPROW 
         INTEGER,       SAVE, POINTER, DIMENSION(:),CONTIGUOUS::JPCOL 
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::BFLUX
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::BFLUXCHK
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::BHEAD
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::BCOND
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::BHEADCHK
         REAL,          SAVE, POINTER, DIMENSION(:),CONTIGUOUS::VCB
        TYPE BFHTYPE
         INTEGER,           POINTER ::ISCHILD,IBOTFLG,IBFLG,NCPP 
         INTEGER,           POINTER ::NPBNODES,NCBNODES,NTIMES
         INTEGER,           POINTER ::NLAYP,NROWP,NCOLP
         INTEGER,           POINTER ::IUBC,NGRIDS
         CHARACTER(LEN=17), POINTER ::BTEXT
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPLBEG
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPRBEG
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPCBEG
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPLEND
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPREND
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPCEND
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::IBPFLG
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::IBB
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::NPINDX
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::KLAY
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::IROW
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::JCOL
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::IFACEGN
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::KPLAY 
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::IPROW 
         INTEGER,           POINTER, DIMENSION(:),CONTIGUOUS::JPCOL 
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::BFLUX
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::BFLUXCHK
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::BHEAD
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::BCOND
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::BHEADCHK
         REAL,              POINTER, DIMENSION(:),CONTIGUOUS::VCB
        END TYPE
        TYPE(BFHTYPE), SAVE  ::BFHDAT(10)
      END MODULE GWFBFHMODULE

C-----VERSION 1.2 10JUNE2009 GWF2BFH2AR      
      SUBROUTINE GWF2BFH2AR(IN,ILGR,IGRID)
C     ******************************************************************
C     ALLOCATE SPACE FOR BOUNDARY FLOW AND HEAD PACKAGE AND READ DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY
      USE GLOBAL,      ONLY:LSTCHK
      USE GWFBFHMODULE,ONLY:ISCHILD,IBOTFLG,IBFLG,NPBNODES,NCBNODES,
     1                      NTIMES,NLAYP,NROWP,NCOLP,IUBC,NGRIDS,NCPP,
     2                      BTEXT,NPLBEG,NPRBEG,NPCBEG,NPLEND,NPREND,
     3                      NPCEND,IBPFLG,IBB,NPINDX,KLAY,IROW,JCOL,
     4                      IFACEGN,KPLAY,IPROW,JPCOL,BFLUX,BHEAD,BCOND,
     5                      BFLUXCHK,BHEADCHK,VCB     
C
      LOGICAL             ::LOP
      CHARACTER*700 LINE
      CHARACTER(LEN=17)   ::CTEXTH,PTEXTF
      DATA CTEXTH /'  GHOST-NODE HEAD'/
      DATA PTEXTF /'  GHOST-NODE FLUX'/
C     ------------------------------------------------------------------
      ALLOCATE(ISCHILD,IBOTFLG,IBFLG,NPBNODES,NCBNODES,NTIMES,NLAYP,
     &         NROWP,NCOLP,IUBC,NGRIDS,NCPP,BTEXT,VCB(4))
      
C1------PRINT A MESSAGE IDENTIFYING BFH PACKAGE
      IF(LSTCHK(3)) THEN
        WRITE(IOUT,500)IN
      ENDIF
  500 FORMAT(1X,/1X,'BFH -- BOUNDARY FLOW AND HEAD PACKAGE',
     &    ', VERSION 2.0, 06/22/2009',/8X,'INPUT READ FROM UNIT ',I4)

C2------CHECK IF LGR IS ACTIVE, IF SO STOP.
      IF(ILGR .NE. 0)THEN 
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,*)
     1       'THE BFH PACKAGE CANNOT BE USED WHEN RUNNING LGR' 
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,*)
     1       'RUN LGR FIRST TO PRODUCE THE FILES NEEDED BY BFH'
        ENDIF
        CALL USTOP(' ')
      ENDIF 
C
C3------READ IN HEADER INFO - BTEXT,NTIMES,NBODES,AND ISCHILD AND PRINT COMMENTS
C3------NOTE THAT WE ARE SKIPPING NLAY,NROW,NCOL BECAUSE WE HAVE IT ALREADY
      IC=1
      NEWFLG=0
      NGRIDS = 2
      CALL URDCOM(IN,IOUT,LINE)
      CALL URWORD(LINE,IC,ISTART1,ISTOP1,1,N,R,0,0)
      IF(LINE(ISTART1:ISTOP1) .EQ. 'SINGLE' .OR. 
     &   LINE(ISTART1:ISTOP1) .EQ. 'MULTIPLE')THEN
        NEWFLG=1
        CALL URWORD(LINE,IC,ISTART2,ISTOP2,1,N,R,0,0)
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,NGRIDS,R,0,0)
      ENDIF
      IF(NEWFLG .EQ. 1)THEN
        IC=1
        CALL URDCOM(IN,IOUT,LINE)
        CALL URWORD(LINE,IC,ISTART1,ISTOP1,1,N,R,0,0)
      ENDIF
      CALL URWORD(LINE,IC,ISTART2,ISTOP2,1,N,R,0,0)
      BTEXT=LINE(ISTART1:ISTOP1+1)//LINE(ISTART2:ISTOP2+1)
      BTEXT=TRIM(ADJUSTL(BTEXT))
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,ISCHILD,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip nlay
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip nrow
      CALL URWORD(LINE,IC,ISTART,ISTOP,0,N,R,0,0)   !skip ncol
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NTIMES,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NPBNODES,R,0,0)
      CALL URWORD(LINE,IC,ISTART,ISTOP,2,NCBNODES,R,0,0)
      IF(ISCHILD .GE. 0)THEN
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,NCPP,R,0,0)
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,IBOTFLG,R,0,0)
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,IBFLG,R,0,0)
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,IUBC,R,0,0)
      ENDIF
      IF(ISCHILD .LE. 0 .AND. NEWFLG .EQ. 0) THEN
        CALL URDCOM(IN,IOUT,LINE)
        READ (LINE,*)  NPLBEG,NPRBEG,NPCBEG
        CALL URDCOM(IN,IOUT,LINE)
        READ (LINE,*)  NPLEND,NPREND,NPCEND  
      ELSEIF(ISCHILD .LE. 0 .AND. NEWFLG .EQ. 1)THEN
        ALLOCATE(NPLBEG(NGRIDS-1),NPRBEG(NGRIDS-1),NPCBEG(NGRIDS-1),
     1  NPLEND(NGRIDS-1),NPREND(NGRIDS-1),NPCEND(NGRIDS-1), 
     2  IBPFLG(NGRIDS-1),IBB(NCBNODES))
        CALL URWORD(LINE,IC,ISTART,ISTOP,2,IUBC,R,0,0)
        READ(IN,*) (IBPFLG(LG),NPLBEG(LG),NPRBEG(LG),NPCBEG(LG),
     1              NPLEND(LG),NPREND(LG),NPCEND(LG),LG=1,NGRIDS-1)
      ENDIF
C
C3A-----PRINT COMMENTS
      IF(ISCHILD .EQ. -1 .AND. BTEXT .EQ. TRIM(ADJUSTL(PTEXTF)))THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,410) BTEXT,NCBNODES
        ENDIF
      ELSEIF(ISCHILD .EQ. 1 .AND. BTEXT .EQ. TRIM(ADJUSTL(CTEXTH)))THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,411) BTEXT,NCBNODES
        ENDIF
      ELSE
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,413) 
        ENDIF
        STOP
      ENDIF
C       
C3B-----CHECK TO SEE IF COMPLEMENTARY BOUNDARY CONDITIONS WERE SAVED.
C3B-----IF SO, CHECK TO SEE IF FILE IS OPENED.  IF SO, THEN READ IN
C3B-----HEADER INFO. FOR CHILD AND RUN CHECK. SKIP PARENT HEADER. 
C3B-----IF B.C.'s WERE SAVED, BUT FILE IS NOT OPENED, DO NOT RUN CHECK.
      IF(IUBC .NE. 0) THEN
        INQUIRE(IUBC,OPENED=LOP)
        IF(LOP)THEN
          IF(ISCHILD .GT. 0) THEN
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,420) IUBC
            ENDIF
            IC=1
            CALL URDCOM(IUBC,0,LINE)
            CALL URWORD(LINE,IC,ISTART,ISTOP,1,N,R,0,0)   !skip text
            CALL URWORD(LINE,IC,ISTART,ISTOP,1,N,R,0,0)   !skip text
            CALL URWORD(LINE,IC,ISTART,ISTOP,1,N,R,0,0)   !skip text
            CALL URWORD(LINE,IC,ISTART,ISTOP,2,N,R,0,0)   !skip ischild
            CALL URWORD(LINE,IC,ISTART,ISTOP,2,NLAYP,R,0,0)
            CALL URWORD(LINE,IC,ISTART,ISTOP,2,NROWP,R,0,0)
            CALL URWORD(LINE,IC,ISTART,ISTOP,2,NCOLP,R,0,0)
          ELSE
            IF(LSTCHK(3)) THEN
              WRITE(IOUT,421) IUBC
            ENDIF
            CALL URDCOM(IUBC,0,LINE)              !skip parent header 1
            CALL URDCOM(IUBC,0,LINE)              !skip parent header 2
            CALL URDCOM(IUBC,0,LINE)              !skip parent IBFLG
          ENDIF
        ELSE
          IUBC=0
        ENDIF
      ENDIF
C
  410 FORMAT (1X,A,/,1X,'RUNNING PARENT MODEL WITH ',I5, ' INTERIOR ',
     &               'BOUNDARY FLUXES')
  411 FORMAT (1X,A,/,1X,'RUNNING CHILD MODEL WITH ',I5,' SPECIFIED ',
     &               'HEAD BOUNDARY NODES')
  413 FORMAT (1X,'INVALID INPUT IN BFH2 FILE:',/, 
     &           'ISCHILD AND BTEXT ARE NOT COMPATIBLE')
C
  420 FORMAT (1X,'CHECKING AGAINST FLUX BOUNDARY CONDITIONS ON UNIT',I5)
  421 FORMAT (1X,'CHECKING AGAINST HEAD BOUNDARY CONDITIONS ON UNIT',I5)

C
C4------ALLOCATE BOUNDARY CONDITION ARRARYS. 
      ALLOCATE(KLAY(NCBNODES),IROW(NCBNODES),JCOL(NCBNODES),
     &         IFACEGN(NCBNODES),BFLUX(NCBNODES)) 
      IF(ISCHILD .GE. 0)THEN
     &         
        ALLOCATE(BHEAD(NCBNODES),BCOND(NCBNODES),
     &           KPLAY(NCBNODES),IPROW(NCBNODES),JPCOL(NCBNODES))
        IF(IUBC .GT. 0) ALLOCATE(BFLUXCHK(NCBNODES),NPINDX(NCBNODES))
      ELSEIF(IUBC .GT. 0)THEN
        ALLOCATE(KPLAY(NPBNODES),IPROW(NPBNODES),JPCOL(NPBNODES),
     &           BHEADCHK(NPBNODES))
      ENDIF
C
C6-----SAVE POINTER DATA TO ARRARYS
      CALL SGWF2BFH2PSV(IGRID)
C7
      RETURN
      END 
C***********************************************************************
      SUBROUTINE GWF2BFH2DA(IGRID)
C     ******************************************************************
C     DEALLOCATE BFH DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      DEALLOCATE(BFHDAT(IGRID)%IBOTFLG)
      DEALLOCATE(BFHDAT(IGRID)%IBFLG)
      DEALLOCATE(BFHDAT(IGRID)%NCPP)
      DEALLOCATE(BFHDAT(IGRID)%NPBNODES)
      DEALLOCATE(BFHDAT(IGRID)%NCBNODES)
      DEALLOCATE(BFHDAT(IGRID)%NTIMES)
      DEALLOCATE(BFHDAT(IGRID)%NLAYP)
      DEALLOCATE(BFHDAT(IGRID)%NROWP)
      DEALLOCATE(BFHDAT(IGRID)%NCOLP)
      DEALLOCATE(BFHDAT(IGRID)%NGRIDS)
      DEALLOCATE(BFHDAT(IGRID)%BTEXT)
      DEALLOCATE(BFHDAT(IGRID)%KLAY)
      DEALLOCATE(BFHDAT(IGRID)%IROW)
      DEALLOCATE(BFHDAT(IGRID)%JCOL)
      DEALLOCATE(BFHDAT(IGRID)%IFACEGN)
      DEALLOCATE(BFHDAT(IGRID)%BFLUX)
      DEALLOCATE(BFHDAT(IGRID)%VCB)
      IF(BFHDAT(IGRID)%ISCHILD .GE. 0)THEN
        DEALLOCATE(BFHDAT(IGRID)%BHEAD)
        DEALLOCATE(BFHDAT(IGRID)%BCOND)
        DEALLOCATE(BFHDAT(IGRID)%KPLAY)
        DEALLOCATE(BFHDAT(IGRID)%IPROW)
        DEALLOCATE(BFHDAT(IGRID)%JPCOL)
        IF(BFHDAT(IGRID)%IUBC .GT. 0) DEALLOCATE(BFHDAT(IGRID)%BFLUXCHK,
     &                                           BFHDAT(IGRID)%NPINDX)
      ELSE
        DEALLOCATE(BFHDAT(IGRID)%IBPFLG,BFHDAT(IGRID)%IBB)
        DEALLOCATE(BFHDAT(IGRID)%NPLBEG)
        DEALLOCATE(BFHDAT(IGRID)%NPRBEG)
        DEALLOCATE(BFHDAT(IGRID)%NPCBEG)
        DEALLOCATE(BFHDAT(IGRID)%NPLEND)
        DEALLOCATE(BFHDAT(IGRID)%NPREND)
        DEALLOCATE(BFHDAT(IGRID)%NPCEND)
        IF(BFHDAT(IGRID)%IUBC .GT. 0) THEN
          DEALLOCATE(BFHDAT(IGRID)%BHEADCHK)
        ENDIF
      ENDIF
      DEALLOCATE(BFHDAT(IGRID)%IUBC)
      DEALLOCATE(BFHDAT(IGRID)%ISCHILD)
C
C NULLIFY LOCAL POINTERS
      IF (IGRID.EQ.1)THEN
        IBOTFLG=>NULL()
        IBFLG=>NULL()
        NCPP=>NULL()
        NPBNODES=>NULL()
        NCBNODES=>NULL()
        NTIMES=>NULL()
        NPLBEG=>NULL()
        NPRBEG=>NULL()
        NPCBEG=>NULL()
        NPLEND=>NULL()
        NPREND=>NULL()
        NPCEND=>NULL()
        NLAYP=>NULL()
        NROWP=>NULL()
        NCOLP=>NULL()
        NGRIDS=>NULL()
        BTEXT=>NULL()
        KLAY=>NULL()
        IROW=>NULL()
        JCOL=>NULL()
        IFACEGN=>NULL()
        BFLUX=>NULL()
        VCB=>NULL()
        BHEAD=>NULL()
        BCOND=>NULL()
        KPLAY=>NULL()
        IPROW=>NULL()
        JPCOL=>NULL()
        BFLUXCHK=>NULL()
        NPINDX=>NULL()
        IBPFLG=>NULL()
        IBB=>NULL()
        BHEADCHK=>NULL()
        IUBC=>NULL()
        ISCHILD=>NULL()   
      END IF
      !
      RETURN
      END 
C***********************************************************************
      SUBROUTINE SGWF2BFH2PNT(IGRID)
C     ******************************************************************
C     CHANGE POINTERS FOR BFH DATA TO A DIFFERENT GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      ISCHILD=>BFHDAT(IGRID)%ISCHILD
      IBOTFLG=>BFHDAT(IGRID)%IBOTFLG
      IBFLG=>BFHDAT(IGRID)%IBFLG
      NCPP=>BFHDAT(IGRID)%NCPP
      NPBNODES=>BFHDAT(IGRID)%NPBNODES
      NCBNODES=>BFHDAT(IGRID)%NCBNODES
      NTIMES=>BFHDAT(IGRID)%NTIMES
      NPLBEG=>BFHDAT(IGRID)%NPLBEG
      NPCBEG=>BFHDAT(IGRID)%NPRBEG
      NPRBEG=>BFHDAT(IGRID)%NPCBEG
      NPLEND=>BFHDAT(IGRID)%NPLEND
      NPREND=>BFHDAT(IGRID)%NPREND
      NPCEND=>BFHDAT(IGRID)%NPCEND
      NLAYP=>BFHDAT(IGRID)%NLAYP
      NROWP=>BFHDAT(IGRID)%NROWP
      NCOLP=>BFHDAT(IGRID)%NCOLP
      IUBC=>BFHDAT(IGRID)%IUBC
      NGRIDS=>BFHDAT(IGRID)%NGRIDS
      BTEXT=>BFHDAT(IGRID)%BTEXT
      IBPFLG=>BFHDAT(IGRID)%IBPFLG
      IBB=>BFHDAT(IGRID)%IBB
      NPINDX=>BFHDAT(IGRID)%NPINDX
      KLAY=>BFHDAT(IGRID)%KLAY
      IROW=>BFHDAT(IGRID)%IROW
      JCOL=>BFHDAT(IGRID)%JCOL
      IFACEGN=>BFHDAT(IGRID)%IFACEGN
      KPLAY=>BFHDAT(IGRID)%KPLAY
      IPROW=>BFHDAT(IGRID)%IPROW
      JPCOL=>BFHDAT(IGRID)%JPCOL
      BFLUX=>BFHDAT(IGRID)%BFLUX
      BFLUXCHK=>BFHDAT(IGRID)%BFLUXCHK
      BHEAD=>BFHDAT(IGRID)%BHEAD
      BCOND=>BFHDAT(IGRID)%BCOND
      BHEADCHK=>BFHDAT(IGRID)%BHEADCHK
      VCB=>BFHDAT(IGRID)%VCB
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE SGWF2BFH2PSV(IGRID)
C     ******************************************************************
C     SAVE POINTERS ARRAYS FOR BFH DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE
C     ------------------------------------------------------------------
C
      BFHDAT(IGRID)%ISCHILD=>ISCHILD
      BFHDAT(IGRID)%IBOTFLG=>IBOTFLG
      BFHDAT(IGRID)%IBFLG=>IBFLG
      BFHDAT(IGRID)%NCPP=>NCPP
      BFHDAT(IGRID)%NPBNODES=>NPBNODES
      BFHDAT(IGRID)%NCBNODES=>NCBNODES
      BFHDAT(IGRID)%NTIMES=>NTIMES
      BFHDAT(IGRID)%NPLBEG=>NPLBEG 
      BFHDAT(IGRID)%NPRBEG=>NPCBEG
      BFHDAT(IGRID)%NPCBEG=>NPRBEG
      BFHDAT(IGRID)%NPLEND=>NPLEND
      BFHDAT(IGRID)%NPREND=>NPREND
      BFHDAT(IGRID)%NPCEND=>NPCEND
      BFHDAT(IGRID)%NLAYP=>NLAYP
      BFHDAT(IGRID)%NROWP=>NROWP
      BFHDAT(IGRID)%NCOLP=>NCOLP
      BFHDAT(IGRID)%IUBC=>IUBC
      BFHDAT(IGRID)%NGRIDS=>NGRIDS
      BFHDAT(IGRID)%BTEXT=>BTEXT
      BFHDAT(IGRID)%IBPFLG=>IBPFLG
      BFHDAT(IGRID)%IBB=>IBB
      BFHDAT(IGRID)%NPINDX=>NPINDX
      BFHDAT(IGRID)%KLAY=>KLAY 
      BFHDAT(IGRID)%IROW=>IROW
      BFHDAT(IGRID)%JCOL=>JCOL
      BFHDAT(IGRID)%IFACEGN=>IFACEGN
      BFHDAT(IGRID)%KPLAY=>KPLAY 
      BFHDAT(IGRID)%IPROW=>IPROW
      BFHDAT(IGRID)%JPCOL=>JPCOL
      BFHDAT(IGRID)%BFLUX=>BFLUX
      BFHDAT(IGRID)%BFLUXCHK=>BFLUXCHK
      BFHDAT(IGRID)%BHEAD=>BHEAD
      BFHDAT(IGRID)%BCOND=>BCOND
      BFHDAT(IGRID)%BHEADCHK=>BHEADCHK
      BFHDAT(IGRID)%VCB=>VCB
C
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE2009 GWF2BFH2RP
      SUBROUTINE GWF2BFH2RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ IN THE LAYER, ROW, AND COLUMN DATA FOR WHERE THE BOUNDARY 
C     CONDITIONS WILL BE APPLIED. 
C     THIS ROUTINE ALSO ZEROS OUT THE INTERIOR OF THE PARENT GRID WHERE 
C     THE CHILD WILL BE.
C     IF COMPLIMENTARY BOUNDARY CONDITIONS WERE SAVED, READ IN THE  
C     HEADER INFO.
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,CC,CR,CV
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFBFHMODULE,ONLY:ISCHILD,NPBNODES,NCBNODES,NPLBEG,NPRBEG,
     &                      NPCBEG,NPLEND,NPREND,NPCEND,NLAYP,NROWP,
     &                      NCOLP,KLAY,IROW,JCOL,KPLAY,IPROW,JPCOL,IUBC,
     &                      NGRIDS,IBPFLG,IBB,IFACEGN 
      CHARACTER*700 LINE
      DOUBLE PRECISION  HNF
C     ------------------------------------------------------------------
C
C1-----IF NOT ON THE FIRST STRESS PERIOD, THEN RETURN
      IF(KPER .GT. 1)RETURN

      CALL SGWF2BFH2PNT(IGRID)
      ZERO = 0.
      HNF = HNOFLO
C
C2-3----IF ON THE FIRST STRESS PERIOD, READ IN THE LAY, ROW, AND COL OF 
C2-3----THE INTERFACE CELLS WHERE BOUNDARY CONDITIONS WILL BE APPLIED.
C2A-----IF IT IS A CHILD GRID READ THE INDEX THAT MAPS CHILD->PARENT 
C3A-----IF IT IS A PARENT GRID, THEN ZERO OUT INTERIOR. 
C
      I=1
      IF(ISCHILD .GE. 0)THEN
        DO N=1,NCBNODES
          READ(IN,*) KLAY(N),IROW(N),JCOL(N),IFACEGN(N),KPLAY(N),
     &               IPROW(N),JPCOL(N),IDUM
        ENDDO
C
C2------END CHILD GRID READING 
C
      ELSE
C3------READ IN AND SET PARENT INTERFACE BOUNDARY
        DO N=1,NCBNODES
          READ(IN,*) KLAY(N),IROW(N),JCOL(N),IDUM,IDUM,IDUM,IFACEGN(N), 
     &               IBB(N)
        ENDDO
C
C3A-----ZERO OUT THE INTERIOR BECAUSE THIS IS A PARENT SIMULATION
C3A-----LOOP THROUGH GRIDS. MOVE ACROSS COLUMNS, FIND THE LOCATION OF
C3A-----THE INTERIOR CELLS BETWEEN INTERFACE CELLS AND ZERO THEM OUT.
        !NOTE: WILL NOT WORK FOR IRREGULAR SHAPES
        DO LG = 1,NGRIDS-1
          DO K = NPLBEG(LG),NPLEND(LG)
            DO I = NPRBEG(LG),NPREND(LG)
              DO J = NPCBEG(LG),NPCEND(LG)
C3B-----ZERO OUT INTERIOR
                IBOUND(J,I,K)=0
                HNEW(J,I,K) = HNF
                IF(K.NE.NLAY) CV(J,I,K)=ZERO
                IF(K.NE.1) CV(J,I,K-1)=ZERO
                CC(J,I,K)=ZERO
                IF(I.NE.1) CC(J,I-1,K)=ZERO
                CR(J,I,K)=ZERO
                IF(J.NE.1) CR(J-1,I,K)=ZERO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
C-------END PARENT GRID READING AND ADJUSTMENTS
      ENDIF  
C
C4------IF COMPLIMENTARY BOUNDARY CONDITION FILE WAS SAVED THEN READ
C4------(AND IGNORE IF CHILD) THE CELL INDEX INFO.
      IF(IUBC .NE. 0)THEN
        IF(ISCHILD .GE. 0) THEN
          DO N=1,NCBNODES
            READ(IUBC,*) IDUM
          ENDDO  
        ELSE
          DO N=1,NPBNODES
            READ(IUBC,*) KPLAY(N), IPROW(N), JPCOL(N)
          ENDDO  
        ENDIF
      ENDIF
C5
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE2009 GWF2BFH2AD
      SUBROUTINE GWF2BFH2AD(IN,IGRID)
C     ******************************************************************
C     READ IN BOUNDARY CONDITION DATA FOR THE CURRENT TIME STEP
C     IF THIS IS A CHILD GRID, THEN APPLY SPECIFIED HEADS FOR THE   
C     CURRENT TIME STEP.      
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE,ONLY:ISCHILD,NPBNODES,NCBNODES,IUBC,BFLUX,
     1                      BFLUXCHK,BHEAD,BCOND,BHEADCHK 
      CHARACTER*700 LINE
C     ------------------------------------------------------------------
      CALL SGWF2BFH2PNT(IGRID)

C1
C1------FIRST READ (AND IGNORE) TIME INFO.
      CALL URDCOM(IN,0,LINE)
C
C1------READ IN THE BOUNDARY CONDITION DATA FOR CHILD AND PARENT GRIDS.  
C1------APPLY BOUNDARY HEADS TO HNEW.
C1------CHECK IF COMPLEMENTARY BOUNDARY CONDITIONS ARE BEING CHECKED.  
C1------IF SO, READ THOSE TOO.
      IF(ISCHILD .GE. 0)THEN
        DO N=1,NCBNODES
          READ(IN,*) BHEAD(N),BCOND(N)
        ENDDO
C2------READ COMPLEMENTARY BOUNDARY CONDITION DATA
        IF(IUBC .GT. 0)THEN
          CALL URDCOM(IUBC,0,LINE)
          DO N=1,NCBNODES
            READ(IUBC,*) BFLUXCHK(N) 
          ENDDO
        ENDIF

      ELSE
C3------PARENT GRID DATA
        DO N=1,NCBNODES
          READ(IN,*) BFLUX(N)
        ENDDO
C4------READ COMPLEMENTARY BOUNDARY CONDITION DATA
        IF(IUBC .GT. 0)THEN
          CALL URDCOM(IUBC,0,LINE)
          DO N=1,NPBNODES
            READ(IUBC,*) BHEADCHK(N)
          ENDDO
        ENDIF
      ENDIF
C5
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 22OCTOBER GWF2BFH2FM
      SUBROUTINE GWF2BFH2FM(KPER,KSTP,KITER,IGRID)
C     ******************************************************************
C     ADD GHOST-NODE CONDUCTANCE TO HCOF AND CONDUCTANCT*BHEAD TO RHS
C     ADD SPECIFIED FLUX CONTRIBUTION TO RHS (PARENT GRID ONLY)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBFHMODULE,ONLY:ISCHILD
C     ------------------------------------------------------------------
      CALL SGWF2BFH2PNT(IGRID)
C1
      IF(ISCHILD .GE. 0)THEN
C2------ACCUMULATE CONDUCTANCE TO HCOF AND COND*BHEAD TO RHS
        CALL SGWF2BFH2FMCBF()
      ELSE
C2------ACCUMULATE FLUX TO RHS
        CALL SGWF2BFH2FMPBF()
      ENDIF
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE2009 SGWF2BFH2FMCBF      
      SUBROUTINE SGWF2BFH2FMCBF()
C
C     ******************************************************************
C     ACCUMULATE CONDUCTANCE TO HCOF AND COND*BHEAD TO RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,HCOF,RHS
      USE GWFBFHMODULE,ONLY:NCBNODES,JCOL,IROW,KLAY,BHEAD,BCOND
C     ------------------------------------------------------------------
C
C1-----LOOP THROUGH ACTIVE BOUNDARY CELLS AND SUBTRACT COND FROM RHS AND
C1-----HEAD*COND FROM RHS
      DO N=1,NCBNODES
        IF(IBOUND(JCOL(N),IROW(N),KLAY(N)) .EQ. 0) CYCLE
        HCOF(JCOL(N),IROW(N),KLAY(N))=HCOF(JCOL(N),IROW(N),KLAY(N)) 
     &                               - BCOND(N)
        RHS(JCOL(N),IROW(N),KLAY(N))=RHS(JCOL(N),IROW(N),KLAY(N))
     &                              - BCOND(N)*BHEAD(N)
      ENDDO     
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE2009 SGWF2BFH2FMPBF      
      SUBROUTINE SGWF2BFH2FMPBF()
C
C     ******************************************************************
C     ACCUMULATE BOUNDARY FLUX CONTRIBUTION ON RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,RHS
      USE GWFBFHMODULE,ONLY:NCBNODES,JCOL,IROW,KLAY,BFLUX
C     ------------------------------------------------------------------
C
C1-----LOOP THROUGH BOUNDARY CELLS AND SUBTRACT BOUNDARY FLUX FROM RHS
      DO N=1,NCBNODES
        IF(IBOUND(JCOL(N),IROW(N),KLAY(N)) .EQ. 0) CYCLE
        RHS(JCOL(N),IROW(N),KLAY(N))=RHS(JCOL(N),IROW(N),KLAY(N))
     &                              - BFLUX(N)
      ENDDO     
C2
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE2009 SGWF2BFH2CBF      
      SUBROUTINE SGWF2BFH2CBF(KKPER,NBNODES,BFLUX)
C     ******************************************************************
C     CALCULATE THE CHILD INTERFACE FLUXES FOR CHECKING AGAINST LGR 
C     BOUNDARY FLUXES.  ALSO USED FOR GLOBAL BUDGET FLUX FROM 
C     SPECIFIED HEAD BOUNDARIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,HNEW 
      USE GWFBFHMODULE,ONLY:JCOL,IROW,KLAY,BHEAD,BCOND
      DOUBLE PRECISION HD, CD, HDIFF 
      DIMENSION BFLUX(NBNODES)
C     ------------------------------------------------------------------
C1------LOOP THROUGH ALL CHILD BOUNDARY NODES AND CALCULATE BOUNDARY
C1------FLUXES 
      DO N=1,NBNODES
C-------SET CHILD CELL INDICES
        K=KLAY(N)
        I=IROW(N)
        J=JCOL(N)
C-------CHECK IF CELL IS ACTIVE.  IF SO, CALCULATE FLUX
        IF(IBOUND(J,I,K) .NE. 0) THEN
          HD=BHEAD(N)
          CD=BCOND(N)
          HDIFF=HD - HNEW(J,I,K)
          RATE=CD*HDIFF
          BFLUX(N) = RATE
        ENDIF
      ENDDO
C2------RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 16JANUARY GWF2BFH2BD
      SUBROUTINE GWF2BFH2BD(KSTP,KPER,IUBCF,IULPF,IUHUF,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR GHOST-NODE FLUX AND GHOST-NODE
C     HEAD
C     THIS ROUTINE IS ADAPTED FROM GWF2LGR2BD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IBOUND,IOUT,NCOL,NROW,NLAY
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,DELT,PERTIM,TOTIM,VBVL,VBNM
      USE GWFBFHMODULE,ONLY:ISCHILD,BFLUX,NCBNODES,NPBNODES,KLAY,IROW,
     1                      JCOL,IFACEGN
      USE GWFBCFMODULE,ONLY:IBCFCB
      USE GWFLPFMODULE,ONLY:ILPFCB
      USE GWFHUFMODULE,ONLY:IHUFCB
      CHARACTER(LEN=16):: TEXT, LGRAUX(1)
      DOUBLE PRECISION RATIN,RATOUT,QQ,DZERO
      DIMENSION GNFACE(1)
      DATA TEXT /' GHOST-NODE FLUX'/
      DATA LGRAUX /'           IFACE'/
C     ------------------------------------------------------------------
C
      CALL SGWF2BFH2PNT(IGRID)
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS.  SET FLOW FILE UNIT NUMBER
      DZERO=0.D0
      RATIN=DZERO
      RATOUT=DZERO
      VBNM(MSUM)=TEXT
      IF(IUBCF .NE. 0) IBFHCB=IBCFCB
      IF(IULPF .NE. 0) IBFHCB=ILPFCB
      IF(IUHUF .NE. 0) IBFHCB=IHUFCB
C
C2------IF CHILD GRID THEN FIND FLUX THROUGH GHOST-NODES.  SET NBNODES
      IF(ISCHILD .GE. 0)THEN 
        CALL SGWF2BFH2CBF(KPER,NCBNODES,BFLUX)
        NBNODES=NCBNODES
      ELSE
        NBNODES=NPBNODES
      ENDIF
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IBD=ICBCFL
      IF(IBD.EQ.2) THEN
        NAUX=1   
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,LGRAUX,IBFHCB,NCOL,NROW,
     1              NLAY,NBNODES,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------LOOP THROUGH FLUX B.C. CALCULATING FLOW FOR ALL ACTIVE CELLS.
C3------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C3------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
C3------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
C3------IF CELL IS INACTIVE, SET FLUX TO 0.
      DO N=1,NCBNODES
        IF(IBOUND(JCOL(N),IROW(N),KLAY(N)) .NE. 0) THEN
          Q=BFLUX(N)
          QQ=Q
          IF(Q .GT. 0) RATIN=RATIN+QQ
          IF(Q .LT. 0) RATOUT=RATOUT-QQ
        ELSE
          BFLUX(N) = 0.
        ENDIF
      ENDDO
C
C4------CHECK IF WRITING COMPACT BUDGETS.  
C4------NOTE: THE COMPACT BUDGET NORMALLY SKIPS CELLS WITH 0 VALUES,
C4------(ie INACTIVE CELLS) HOWEVER, WITH DRYING/WETTING THIS REQUIRES
C4------KEEPING TRACK OF THE NUMBER OF ACTIVE CONNECTIONS.  FOR NOW, 
C4------ALL INTERFACE CELLS ARE WRITTEN.  THIS WORKS BECAUSE BFLUX
C4------IS ZEROED ABOVE FOR INACTIVE CELLS.
      IF(IBD.EQ.2) THEN 
C4A-----IF CHILD, LOOP THROUGH ALL BOUNDARY CELLS, SET VALUES AND CALL 
C4A-----ROUTINE TO WRITE BUDGET.
        IF(ISCHILD .GE. 0)THEN 
          DO N=1,NBNODES
            Q=BFLUX(N)
            GNFACE = IFACEGN(N)
            CALL UBDSVB(IBFHCB,NCOL,NROW,JCOL(N),IROW(N),KLAY(N),Q,
     1                  GNFACE,1,NAUX,1,IBOUND,NLAY)
          ENDDO
        ELSE
C4B-----IF PARENT, LOOP THROUGH ALL CELLS AND SUM UP ALL GHOST-NODE 
C4B-----FLUXES MATCHING THE BFH CELL INDEX.  FIND FIRST MATCH FOR 
C4B-----SETTING IFACE AND CALL ROUTINE TO WRITE BUDGET.
          Q = 0.
          DO K = 1, NLAY
            DO I = 1, NROW
              DO J = 1, NCOL
                Q =SUM(BFLUX, KLAY .EQ. K .AND. IROW .EQ. I  
     1                       .AND. JCOL .EQ. J)  
                DO N=1,NCBNODES
                  IF(KLAY(N) .EQ. K .AND. IROW(N) .EQ. I
     1               .AND. JCOL(N) .EQ. J)THEN
                    GNFACE = IFACEGN(N)
                    CALL UBDSVB(IBFHCB,NCOL,NROW,J,I,K,Q,
     1                        GNFACE,1,NAUX,1,IBOUND,NLAY)
                    Q = 0.
C-------MATCH FOUND. SKIP TO NEXT J INDEX TO AVOID REPEATS
                    EXIT 
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF 
      ENDIF 
C
C5------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C
C6------INCREMENT BUDGET TERM COUNTER(MSUM) 
      MSUM=MSUM+1
C
C7------RETURN
      RETURN
      END
C***********************************************************************
C-----VERSION 1.0 15FEBRUARY GWF2BFH2OT
      SUBROUTINE GWF2BFH2OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE BOUNDARY HEAD OR FLUX DIFFERENCES AGAINST THOSE SAVED
C     FROM THE LGR AND REPORT WHERE LARGEST DISCREPANCY IS FOUND.  
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,HNEW
      USE GLOBAL,      ONLY:LSTCHK
      USE GWFBASMODULE,ONLY:IBUDFL
      USE GWFBFHMODULE,ONLY:ISCHILD,IUBC,NPBNODES,NCBNODES,KLAY,IROW,
     1                      JCOL,KPLAY,IPROW,JPCOL,NPINDX,BFLUX,
     2                      BFLUXCHK,BHEADCHK
C     ------------------------------------------------------------------
C
C1------IF BUDGET IS BEING SAVED THEN PRINT FLUX FROM BFH SPECIFIED
C1------HEAD BOUNDARIES.
        IF(ISCHILD .GE. 0) CALL SGWF2BFH2CBD(KPER,KSTP,IBUDFL,NCBNODES,
     &                                       BFLUX)
C 
C2------CHECK IF COMPLEMENTARY BOUNDARY CONDITION FILES WERE SAVED 
C2------AND IF BUDGET IS BEING SAVED.  IF SO, THEN LOOP THROUGH
C2------BOUNDARY CONDITIONS AND FIND MAXIUMUM DIFFERENCE
      IF(IUBC .NE. 0 .AND. IBUDFL .NE. 0)THEN
        SUMNEW = 0.
        SUMOLD = 0.
        DIFFMAX = 0.
        DIFFMEAN = 0.
        NMAX = 1
        IF(ISCHILD .GE. 0)THEN 
          DO N=1,NCBNODES
            SUMNEW = SUMNEW + BFLUX(N)
            SUMOLD = SUMOLD + BFLUXCHK(N) 
            DIFF = BFLUX(N) - BFLUXCHK(N) 
            DIFFMEAN=DIFFMEAN + ABS(DIFF)
            IF(ABS(DIFF) .GT. ABS(DIFFMAX))THEN
              DIFFMAX = DIFF
              NMAX = N
            ENDIF
          ENDDO
C
C2A-----WRITE OUTPUT
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,300) SUMNEW,SUMOLD,DIFFMEAN/NCBNODES,DIFFMAX,
     &                    KPLAY(NMAX),IPROW(NMAX),JPCOL(NMAX),
     &                    BFLUX(NMAX),BFLUXCHK(NMAX)
          ENDIF
C2
C2B-----PARENT SIMULATION: FIND MAX HEAD DIFFERENCE
        ELSE
          DO N=1,NPBNODES
            DIFF = HNEW(JPCOL(N),IPROW(N),KPLAY(N)) - BHEADCHK(N)
            DIFFMEAN=DIFFMEAN + ABS(DIFF)
            IF(ABS(DIFF) .GT. ABS(DIFFMAX))THEN
              DIFFMAX = DIFF
              NMAX = N
              HDMAX=HNEW(JPCOL(N),IPROW(N),KPLAY(N))
            ENDIF
          ENDDO
C2D
C2D-----WRITE OUTPUT
          IF(LSTCHK(3)) THEN
            WRITE(IOUT,400) DIFFMEAN/NPBNODES,DIFFMAX,KLAY(NMAX),
     &                    IROW(NMAX),JCOL(NMAX),HDMAX,
     &                    BHEADCHK(NMAX)         
          ENDIF
        ENDIF

      ENDIF            
C
 300  FORMAT(1X,/,'BFH2: BOUNDARY FLUX COMPARISON',
     &       1X,/,30('-'),
     &       1X,/,'NEW TOTAL BOUNDARY FLUX = ',G16.9,
     &       1X,/,'OLD TOTAL BOUNDARY FLUX = ',G16.9,
     &       1X,/,'AVERAGE ABSOLUTE FLUX DIFFERENCE = ',G16.9,
     &       1X,/,'MAXIMUM ABSOLUTE FLUX DIFFERENCE OF ',G16.9,
     &       1X,/,'OCCURS AT PARENT LAYER ',I0,' ROW ',I0,' COLUMN ',I0,
     &       1X,/,'NEW FLUX AT THIS NODE = ',G16.9,
     &       1X,/,'OLD FLUX AT THIS NODE = ',G16.9,/)

 400  FORMAT(1X,/,'BFH2: BOUNDARY HEAD COMPARISON',
     &       1X,/,30('-'),
     &       1X,/,'AVERAGE ABSOLUTE HEAD DIFFERENCE = ',G16.9,
     &       1X,/,'MAXIMUM ABSOLUTE HEAD DIFFERENCE OF ',G16.9,
     &       1X,/,'OCCURS AT PARENT LAYER ',I0,' ROW ',I0,' COLUMN ',I0,
     &       1X,/,'NEW HEAD AT THIS NODE = ',G16.9,
     &       1X,/,'OLD HEAD AT THIS NODE = ',G16.9,/)
C3
      RETURN
      END
C***********************************************************************
C-----VERSION 1.2 22JUNE SGWF2BFH2CBD
      SUBROUTINE SGWF2BFH2CBD(KPER,KSTP,IBUDFL,NBNODES,BFLUX)
C     ******************************************************************
C     CALCULATE FLUXES FROM THE GHOST-NODE SPECIFIED HEAD BOUNDARIES
C     MODFIFIED FROM GWF2LGR2CBD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT
      USE GLOBAL,      ONLY:LSTCHK
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBFHMODULE,ONLY:VCB
      DIMENSION BFLUX(NBNODES)
      CHARACTER(LEN=16):: VCBNM 
      CHARACTER(LEN=17):: VAL1,VAL2
      DATA VCBNM /' GHOST-NODE HEAD'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS AND SET VALUES FOR PRINTING
      DZERO=0.D0
      RATIN=DZERO
      RATOUT=DZERO
      SMALL=0.1
      BIGVL1=9.99999E11
      ZERO=0.0
      IF(KPER .EQ. 1 .AND. KSTP .EQ. 1)THEN
        VCB(1) = ZERO
        VCB(2) = ZERO
      ENDIF
C
C
C2------LOOP THROUGH FLUX B.C. CALCULATING FLOW.
C2------SEE IF FLOW IS POSITIVE OR NEGATIVE.
C2------FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATOUT.
C2------FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATIN.
      DO N=1,NBNODES  
        Q=BFLUX(N)
        QQ=Q
        IF(Q .GT. 0) RATIN=RATIN+QQ
        IF(Q .LT. 0) RATOUT=RATOUT-QQ
      ENDDO
C
C3------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      RIN=RATIN
      ROUT=RATOUT
      VCB(3)=RIN
      VCB(4)=ROUT
      VCB(1)=VCB(1)+RIN*DELT
      VCB(2)=VCB(2)+ROUT*DELT
C
C4------PRINT RATES TO OUTPUT FILE IF REQUESTED
      IF(IBUDFL .NE. 0)THEN
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,260) KSTP, KPER
        ENDIF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,265) 
        ENDIF
C
C5A-----INFLOWS: CHECK MAGNITUDES FOR PRINT FORMATTING     
C5A-----TOTALS IN VAL1 AND RATES IN VAL2
        IF(VCB(1).NE.ZERO .AND.
     1      (VCB(1).GE.BIGVL1 .OR. VCB(1).LT.SMALL)) THEN
           WRITE(VAL1,'(1PE17.4)') VCB(1)
        ELSE
           WRITE(VAL1,'(F17.4)') VCB(1)
        END IF
        IF(VCB(3).NE.ZERO .AND.
     1      (VCB(3).GE.BIGVL1 .OR. VCB(3).LT.SMALL)) THEN
           WRITE(VAL2,'(1PE17.4)') VCB(3)
        ELSE
           WRITE(VAL2,'(F17.4)') VCB(3)
        END IF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,270) VAL1,VAL2
        ENDIF
C
C5B-----OUTFLOWS: CHECK MAGNITUDES FOR PRINT FORMATTING     
C5B-----TOTALS IN vAL1 AND RATES IN VAL2
        IF(VCB(2).NE.ZERO .AND.
     1      (VCB(2).GE.BIGVL1 .OR. VCB(2).LT.SMALL)) THEN
           WRITE(VAL1,'(1PE17.4)') VCB(2)
        ELSE
           WRITE(VAL1,'(F17.4)') VCB(2)
        END IF
        IF(VCB(4).NE.ZERO .AND.
     1      (VCB(4).GE.BIGVL1 .OR. VCB(4).LT.SMALL)) THEN
           WRITE(VAL2,'(1PE17.4)') VCB(4)
        ELSE
           WRITE(VAL2,'(F17.4)') VCB(4)
        END IF
        IF(LSTCHK(3)) THEN
          WRITE(IOUT,280) VAL1,VAL2
        ENDIF
      ENDIF

  260 FORMAT('1',/2X,'VOLUMETRIC BUDGET FOR BFH2 SPECIFIED HEADS AT'
     1,' TIME STEP',I3,' IN STRESS PERIOD',I4/2X,79('-'))
  265 FORMAT(1X,/5X,'CUMULATIVE VOLUMES',6X,'L**3',7X
     1,'RATES FOR THIS TIME STEP',6X,'L**3/T'/5X,18('-'),17X,24('-'))
  270 FORMAT(1X,/12X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
  280 FORMAT(1X,/11X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A,/)
C6
      RETURN
      END
