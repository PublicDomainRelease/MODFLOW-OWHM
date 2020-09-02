! NOT USED -- COPY_QCAP_TO_Q_EXT
!MODULE FMP_BASE_DATA_INTERFACE
!    IMPLICIT NONE
!    !
!    TYPE FMP_BASE_PROP
!        LOGICAL:: RP
!        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: ARR
!        
!        
!    END TYPE
!END MODULE
MODULE WELL_DATA_FMP_INTERFACE
  ! DEFINES A DATA TYPE THAT HOLDS ALL THE FMP WELLS.
  ! MODULE IS DESIGNED SUCH THAT ONLY 1 INSTANCE OF TYPE(FARM_WELL_DATA), OF ANY DIMENSION SHOULD BE USED 
  ! EG TYPE(FARM_WELL_DATA), DIMENSION(:),ALLOCATABLE:: A  !ONE INSTANCE
  ! BUT NOT
  ! TYPE(FARM_WELL_DATA), DIMENSION(:),ALLOCATABLE:: A, B  !THERE ARE TWO INSTANCES!
  USE CONSTANTS
  USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
  USE UTIL_INTERFACE,                   ONLY: READ_TO_DATA, FILE_IO_ERROR, STOP_ERROR, WARNING_MESSAGE, PARSE_WORD, PARSE_WORD_UP, GET_INTEGER, GET_NUMBER, GET_WORD, NEAR_ZERO, IS_INTEGER
  USE NUM2STR_INTERFACE,                ONLY: NUM2STR, INTFMT
  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
  USE GENERIC_OUTPUT_FILE_INSTRUCTION,  ONLY: GENERIC_OUTPUT_FILE
  USE GENERIC_INPUT_FILE_INSTRUCTION,   ONLY: GENERIC_INPUT_FILE
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: FARM_WELL_DATA, INITIALIZE_FMP_WELL_DATA, FWEL_BASIC_ALLOCATE
  !
  DOUBLE PRECISION, PARAMETER:: MINSIZE=1D-30    !SIZE BEFORE IT IS ASSUMED ZERO
  !
  !
  TYPE SINGLE_FEEDFILE_INPUT
      TYPE(GENERIC_INPUT_FILE):: FL
      INTEGER, DIMENSION(:), ALLOCATABLE:: FID   !(NCELL)
      REAL,    DIMENSION(:), ALLOCATABLE:: RVAL  !(NCELL)
      INTEGER, DIMENSION(:), ALLOCATABLE:: OLD_FID
      LOGICAL                           :: NEW_FID
      LOGICAL                           :: MNWLINK = FALSE
      !
      CHARACTER(20),    DIMENSION(:),   ALLOCATABLE:: WELLID   ! UNIQUE ID
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: Qini     ! (NCELL)
      INTEGER,          DIMENSION(:,:), ALLOCATABLE:: AUX      ! AUX(NCELL,NAUX) HOLDS AUXILIARY VARIABLS
      INTEGER,          DIMENSION(:,:), ALLOCATABLE:: LRC      ! LRC(3,   NCELL) HOLDS LAYER, ROW, COL of WEL
      INTEGER,          DIMENSION(:),   ALLOCATABLE:: MNWLOC   ! 0 = No-LINK  >0 IS MNW2 LOC
      INTEGER,          DIMENSION(:),   ALLOCATABLE:: MASTER_LOC  !HOLDS INDEX IN MASTER_ACT FOR CURRENT WELL
      CONTAINS
      FINAL:: SINGLE_FEEDFILE_INPUT_DEALLOCATE
  END TYPE
  !
  ! INTERNAL GLOBAL VARIABLES
  !INTEGER,                                 SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: LIST_UNIT                    ! GLOBAL CONTENTS OF IOUT BY GRID
  !TYPE(SINGLE_FEEDFILE_INPUT),             SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: GLOBAL_FEED                  ! IF ONE FEED FILE THEN THIS HOLDS GLOBAL INFORMATION
  !TYPE(GENERIC_OUTPUT_FILE),               SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: OUT_INPUT                    ! DIM BY NGRID
  !TYPE(GENERIC_OUTPUT_FILE),               SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: OUT_BYWELL                   ! DIM BY NGRID
  !TYPE(GENERIC_OUTPUT_FILE),               SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: OUT_BYFARM
  !TYPE(GENERIC_OUTPUT_FILE),               SAVE, DIMENSION(:), ALLOCATABLE, TARGET:: OUT_SMOOTH
  !
  ! MAIN DATA TYPE
  TYPE FARM_WELL_DATA               ! ALL INFORMATION FOR FARM WELLS FOR 1 FARM
      INTEGER:: NF                  !FARM ID THAT WELL SET PERTAINS TOO
      !
      DOUBLE PRECISION:: QMAX = 0D0 ! FARM TOTAL PUMPING CAPACITY
      DOUBLE PRECISION:: QMAXini    ! FARM INITIAL TOTAL PUMPING CAPACITY
      DOUBLE PRECISION:: SFAC = 1D0 ! SCALE FACTOR
      DOUBLE PRECISION:: SMOOTH_LIM ! SMOOTHING ACTIVATION LIMIT --EITHER MIN THICKNESS OR FRACTION OF A LAYER
      DOUBLE PRECISION:: ALLOT=-1D0 ! FARM ALLOTMENT FOR PUMPAGE (MAX VOLUME)  <0 INDICATES NO ALLOTMENT
      !
      INTEGER:: N   = Z             ! NUMBER OF ACTIVE WELLS FOR SP
      INTEGER:: NAUX= Z             ! NUMBER OF AUX VARIABLES
      INTEGER:: DIM = Z             ! ARRAY SIZE
      INTEGER:: LOAD_TYPE = Z       ! HOW FARM WELLS WERE LOADED, =1 INDICATES LIST LOAD, =2 INDICATES FID LINEFEED LOAD, =3 Capacity Linefeed
      INTEGER:: SMOOTH    = Z       ! INDICATES IF SMOOTHING IS IN USE. IF =1 THEN FRACTION SMOOTHING IS USED, =2 THEN THICKNESS SMOOTHING IS USED.
      INTEGER:: MNW_SPREAD= Z       ! POINTS TO HOW PUMPAGE IS SPREAD ACROSS WELLS...ONLY MAKES A DIFFERENCE ON SIMULATION TIME.
      LOGICAL:: MNWLINK = FALSE   ! SET TO TRUE IF THERE ARE WELLS WITH MNW LINK WITHIN
      LOGICAL:: QMAXRESET = TRUE  ! AUXILIARY VARIABLE SET TO TRUE WHENB IS USE FOR ALL FARM WELLS FOR THIS FARM
      LOGICAL:: NOCIRNOQ  = FALSE ! AUXILIARY VARIABLE SET TO TRUE WHENB IS USE FOR ALL FARM WELLS FOR THIS FARM
      LOGICAL:: MNW_ON    = FALSE ! AUXILIARY VARIABLE SET TO TRUE WHENB IS USE FOR ALL FARM WELLS FOR THIS FARM
      LOGICAL:: MNW_OFF   = FALSE ! AUXILIARY VARIABLE SET TO TRUE WHENB IS USE FOR ALL FARM WELLS FOR THIS FARM
      LOGICAL:: LISTPRINT = FALSE
      !
      LOGICAL,          DIMENSION(:),   ALLOCATABLE:: ACT      ! TRUE INDICATES WELL IS IN USE FOR SP AND AVAILIBLE FOR CROP DEMAND
      LOGICAL,          DIMENSION(:),   ALLOCATABLE:: EXT      ! TRUE INDICATES WELL IS ELSEWHERE -- EXTERNAL PUMPAGE
      CHARACTER(20),    DIMENSION(:),   ALLOCATABLE:: WELLID   ! UNIQUE ID
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: Q        ! Applied Rate to RHS -- FMP Calculated
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: Qcap     ! Well Capacity --Could be reduced by MNW or Allotments
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: Qini     ! Initial Capacity
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: Qsmf     ! Smoothing factor that is applied to Qcap to reduce capacity do to cell dewatering
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: DMD      ! Initial demand for pumping
      INTEGER,          DIMENSION(:),   ALLOCATABLE:: MNWLOC   ! 0 = No-LINK  >0 IS MNW2 LOC
      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: FRAC     ! EACH WELLS FRACTION OF ITS PUMPING TO THE TOTAL PUMPING (Qcap/QMAX)
      INTEGER,          DIMENSION(:,:), ALLOCATABLE:: AUX      ! AUX(:,NAUX) HOLDS AUXILIARY VARIABLS
      INTEGER,          DIMENSION(:,:), ALLOCATABLE:: LRC      ! LRC(3,:) HOLDS LAYER, ROW, COL of WEL
      INTEGER,          DIMENSION(:),   ALLOCATABLE:: SP_START, SP_END
      INTEGER,          DIMENSION(:),   ALLOCATABLE:: MASTER_LOC  !HOLDS INDEX IN MASTER_ACT FOR CURRENT WELL
      !
      DOUBLE PRECISION, DIMENSION(:), POINTER, CONTIGUOUS:: SMOOTH_LIM_BY_LAYER => NULL() ! WHEN BYLAYER SMOOTHING IS USED THIS HOLDS ON NF=1 THE SMOOTHED VALUE BY LAYER
      !
      !LOGICAL:: LINEFEED_INPUT
      TYPE(GENERIC_INPUT_FILE), ALLOCATABLE:: FEED
      !
      INTEGER:: PRORATE_Q    =1  !DEFAULT IS PRORATE ByCapacity
      INTEGER:: PRORATE_ALLOT=0
      !
      ! GLOBAL PROPERTIES (ONLY ALLOCATED ON NF=1)
      TYPE(WARNING_TYPE),                                 POINTER:: WRN         => NULL()
      TYPE(WARNING_TYPE),                                 POINTER:: WRN2        => NULL()
      TYPE(WARNING_TYPE),                                 POINTER:: WRN3        => NULL()
      LOGICAL,                   DIMENSION(:),CONTIGUOUS, POINTER:: MASTER_ACT  => NULL()
      !CHARACTER(20),             DIMENSION(:),CONTIGUOUS, POINTER:: MASTER_WID  => NULL()
      INTEGER,                                            POINTER:: IOUT        => NULL()
      INTEGER,                                            POINTER:: NFARM       => NULL()
      TYPE(GENERIC_OUTPUT_FILE),                          POINTER:: OUT_INPUT   => NULL()  !THIS WILL POINT TO GLOBAL OUTPUT FILE
      TYPE(GENERIC_OUTPUT_FILE),                          POINTER:: OUT_BYMNW   => NULL()  !THIS WILL POINT TO GLOBAL OUTPUT FILE
      TYPE(GENERIC_OUTPUT_FILE),                          POINTER:: OUT_BYWELL  => NULL()  !THIS WILL POINT TO GLOBAL OUTPUT FILE
      TYPE(GENERIC_OUTPUT_FILE),                          POINTER:: OUT_BYFARM  => NULL()  !THIS WILL POINT TO GLOBAL OUTPUT FILE
      TYPE(GENERIC_OUTPUT_FILE),                          POINTER:: OUT_SMOOTH  => NULL()  !THIS WILL POINT TO GLOBAL OUTPUT FILE
      TYPE(SINGLE_FEEDFILE_INPUT),DIMENSION(:),CONTIGUOUS,POINTER:: FID_FEED    => NULL()  !THIS WILL POINT TO GLOBAL FEED INPUT
      !
      CONTAINS
      !
      PROCEDURE, PASS(FWEL):: INITIALIZE_FMP_WELL_DIMENSION!(DIM)
      !PROCEDURE, PASS(FWEL):: SET_DIM     => SET_FWELL_DIMENSIONS       !(N)
      PROCEDURE, PASS(FWEL):: NEXT         => FWEL_READ_N_PREPARE        !(SP, LFID)
      PROCEDURE, PASS(FWEL):: NEXT_SETUP   => FWEL_READ_N_PREPARE_SETUP
      PROCEDURE, PASS(FWEL):: IBOUND_CHECK=> FWEL_IBOUND_CHECK          !(IOUT)
      PROCEDURE, PASS(FWEL):: SET_ALLOT   => SET_GROUNDWATER_ALLOTMENT  !(ALLOTGW)  --ONLY ALLOTTED VALUES
      PROCEDURE, PASS(FWEL):: APPLY_ALLOT => APPLY_GROUNDWATER_ALLOTMENT!() 
      !
      PROCEDURE, PASS(FWEL):: FWELL_ZERO_Q
      PROCEDURE, PASS(FWEL):: MNW2_INDEX  =>FWELL_BUILD_MNW2_INDEX      !(HAS_MNW2)
      PROCEDURE, PASS(FWEL):: MNW2_NEWSP  =>FWELL_MNW2_SP_INIT          !()
      PROCEDURE, PASS(FWEL):: MNW2_ZERO_Q =>FWELL_MNW2_ZERO_Q           !()
      PROCEDURE, PASS(FWEL):: MNW2_UPDATE_CAP =>FWELL_MNW2_UPDATE_CAPACITY!()
      !
      PROCEDURE, PASS(FWEL):: CALCULATE_Q => CALCULATE_PUMPING_TO_MEET_DEMAND!(QREQ)
      PROCEDURE, PASS(FWEL):: APPLY_Q     => APPLY_PUMPING_TO_MEET_DEMAND    !()
      !
      PROCEDURE, PASS(FWEL)::                AUX_QMAXRESET              !(AUXIDX,IALLOTGW,ALLOTGW)
      PROCEDURE, PASS(FWEL)::                AUX_NOCIRNOQ               !(AUXIDX,IALLOTGW,ALLOTGW,TDR,RESETQ)
      !
      PROCEDURE, PASS(FWEL):: PRINT_INPUT => PRINT_INPUT_TRANSCRIPT !( KPER, KSTP, [IU2]) 
      PROCEDURE, PASS(FWEL):: PRINT_WELL  => PRINT_WELL_LIST        !()
      PROCEDURE, PASS(FWEL)::                PRINT_SMOOTHED_PUMPING !( KPER, KSTP, TOTIME, DATE )
      PROCEDURE, PASS(FWEL):: PRINT_TO_LIST=>PRINT_DETAILS_TO_LIST  !( KPER, KSTP, TOTIME, DATE )
      PROCEDURE, PASS(FWEL):: PRINT_BYMNW  =>PRINT_OUT_BYMNW        !( KPER, KSTP, DELT, DYEAR, DATE )
      PROCEDURE, PASS(FWEL):: PRINT_BYWELL =>PRINT_OUT_BYWELL       !( KPER, KSTP, DELT, DYEAR, DATE )
      PROCEDURE, PASS(FWEL):: PRINT_BYFARM =>PRINT_OUT_BYFARM       !( KPER, KSTP, DELT, DYEAR, DATE )
      !
      FINAL::                                FINAL_DEALLOCATE_FWELL_DIMENSIONS
  END TYPE
  !
  !INTERFACE FARM_WELL_DATA!( BL, FWEL, NAUX ) 
  !  MODULE PROCEDURE FWEL_ARRAY_INITIALIZE
  !END INTERFACE
  !
  CONTAINS
  !
  SUBROUTINE SINGLE_FEEDFILE_INPUT_DEALLOCATE(FED)
    TYPE(SINGLE_FEEDFILE_INPUT):: FED
    IF( ALLOCATED(FED%FID) ) THEN
        DEALLOCATE( FED%FID     )
        DEALLOCATE( FED%RVAL    )
        DEALLOCATE( FED%OLD_FID )
        DEALLOCATE( FED%WELLID  ) 
        DEALLOCATE( FED%Qini    ) 
        DEALLOCATE( FED%AUX     ) 
        DEALLOCATE( FED%LRC     ) 
        DEALLOCATE( FED%MNWLOC  ) 
        DEALLOCATE( FED%MASTER_LOC) 
    END IF
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE INITIALIZE_FMP_WELL_DIMENSION(FWEL, DIM)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER, INTENT(IN):: DIM
    !
    IF (FWEL%DIM > 0) CALL DEALLOCATE_FWELL_DIMENSIONS(FWEL)
    !
    FWEL%DIM=DIM
    !
    IF (DIM > 0 ) THEN
       ALLOCATE( FWEL%MNWLOC(DIM) )
       ALLOCATE( FWEL%ACT   (DIM) )
       ALLOCATE( FWEL%EXT   (DIM) )
       ALLOCATE( FWEL%WELLID(DIM) )
       ALLOCATE( FWEL%Q     (DIM) )
       ALLOCATE( FWEL%Qcap  (DIM) )
       ALLOCATE( FWEL%Qini  (DIM) )
       ALLOCATE( FWEL%DMD   (DIM) )
       ALLOCATE( FWEL%MASTER_LOC(DIM))
       !
       ALLOCATE( FWEL%LRC (3,DIM) )
       IF(FWEL%NAUX>0) ALLOCATE( FWEL%AUX(DIM,FWEL%NAUX), SOURCE=Z )
       !
       IF(FWEL%LOAD_TYPE == 1) THEN
           ALLOCATE( FWEL%SP_START(DIM) )
           ALLOCATE( FWEL%SP_END  (DIM) )
       END IF
       ALLOCATE( FWEL%FRAC(DIM) )
       !IF( FWEL%PRORATE_Q>0 ) ALLOCATE( FWEL%FRAC(DIM) )
       IF( FWEL%SMOOTH   >0 ) ALLOCATE( FWEL%Qsmf(DIM) )
    END IF
    !
  END SUBROUTINE
  !
!  PURE ELEMENTAL SUBROUTINE SET_FWELL_DIMENSIONS(FWEL, N)
!    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
!    INTEGER, INTENT(IN):: N
!    !
!    FWEL%N = N
!    !
!    IF( FWEL%DIM < N ) THEN           
!       CALL INITIALIZE_FMP_WELL_DIMENSION(FWEL, N)
!    END IF
!    !
!    IF(FWEL%LOAD_TYPE == 1 .AND. N > 0 ) THEN
!           FWEL%ACT(:N)=TRUE
!           IF( N < FWEL%DIM ) FWEL%ACT(N+1:)=FALSE
!    END IF
!    !
!  END SUBROUTINE 
!  !
!  PURE SUBROUTINE ADD_FWELL_INFO(FWEL, NF, WELLID, Qini, LRC, AUX)
!    CLASS(FARM_WELL_DATA),DIMENSION(:), INTENT(INOUT)::FWEL
!    INTEGER,                            INTENT(IN   ):: NF
!    CHARACTER(*),                       INTENT(IN   ):: WELLID
!    DOUBLE PRECISION,                   INTENT(IN   ):: Qini
!    INTEGER, DIMENSION(:),              INTENT(IN   ):: LRC, AUX
!    INTEGER:: Z, ONE, DIM, I, J
!    DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: TMP
!    INTEGER,          DIMENSION(:,:), ALLOCATABLE:: ITMP
!    CHARACTER(20),    DIMENSION(:),   ALLOCATABLE:: CTMP
!    DOUBLE PRECISION:: DZ
!    Z = 0
!    !
!    IF (NF < 1) RETURN
!    !
!    DZ = 0D0
!    ONE = 1
!    !
!    FWEL(NF)%DIM=FWEL(NF)%DIM + ONE
!    DIM = FWEL(NF)%DIM
!    !
!    IF(FWEL(NF)%DIM - ONE == Z) THEN
!       !
!       ALLOCATE( FWEL(NF)%WELLID(ONE), SOURCE = WELLID )
!       ALLOCATE( FWEL(NF)%Qini  (ONE), SOURCE = Qini   )
!       !
!       ALLOCATE( FWEL(NF)%LRC (3,ONE) )
!       FWEL(NF)%LRC(:,ONE) = LRC
!       !
!       IF(FWEL(NF)%NAUX>0) THEN
!           ALLOCATE( FWEL(NF)%AUX(ONE,FWEL(NF)%NAUX) )
!           DO CONCURRENT (J=1:FWEL(NF)%NAUX)
!                                                  ITMP(DIM,J) = AUX(J)
!           END DO
!       END IF
!       !
!    ELSE
!       !
!       ALLOCATE( CTMP, SOURCE = [FWEL(NF)%WELLID, WELLID] )
!       CALL MOVE_ALLOC(CTMP, FWEL(NF)%WELLID)
!       !
!       ALLOCATE( TMP, SOURCE = [FWEL(NF)%Qini, Qini]   )
!       CALL MOVE_ALLOC(TMP, FWEL(NF)%Qini)
!       !
!       ALLOCATE( ITMP, SOURCE=RESHAPE([FWEL(NF)%LRC,LRC],[3,DIM]) )
!       CALL MOVE_ALLOC(ITMP, FWEL(NF)%LRC)
!       !
!       IF(FWEL(NF)%NAUX>0) THEN
!                           ALLOCATE( ITMP(DIM,FWEL(NF)%NAUX) )
!                           DO CONCURRENT (I=1:DIM-1, J=1:FWEL(NF)%NAUX)
!                                                                  ITMP(I,J)   = FWEL(NF)%AUX(I,J)
!                           END DO
!                           DO CONCURRENT (J=1:FWEL(NF)%NAUX)
!                                                                  ITMP(DIM,J) = AUX(J)
!                           END DO
!                           CALL MOVE_ALLOC(ITMP, FWEL(NF)%AUX)
!       END IF
!    END IF
!    !
!  END SUBROUTINE
!  !
!  PURE ELEMENTAL SUBROUTINE ADD_FWELL_SETUP_EXTRA_PARTS(FWEL) 
!    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
!    INTEGER:: Z
!    !
!    Z = 0
!    ALLOCATE( FWEL%MNWLOC(FWEL%DIM), SOURCE = Z      )
!    ALLOCATE( FWEL%ACT   (FWEL%DIM), SOURCE = TRUE )
!    ALLOCATE( FWEL%Q     (FWEL%DIM)                  )
!    ALLOCATE( FWEL%Qcap  (FWEL%DIM)                  )
!    !
!    IF( FWEL%PRORATE_Q>0 ) ALLOCATE( FWEL%FRAC(FWEL%DIM) )
!    IF( FWEL%SMOOTH   >0 ) ALLOCATE( FWEL%Qsmf(FWEL%DIM) )
!    
!  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE DEALLOCATE_FWELL_DIMENSIONS(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL  
    !
    IF(FWEL%DIM > 0 ) THEN
       DEALLOCATE( FWEL%MNWLOC )
       DEALLOCATE( FWEL%ACT    )
       DEALLOCATE( FWEL%EXT    )
       DEALLOCATE( FWEL%WELLID )
       DEALLOCATE( FWEL%Q      )
       DEALLOCATE( FWEL%Qcap   )
       DEALLOCATE( FWEL%Qini   )
       DEALLOCATE( FWEL%DMD    )
       DEALLOCATE( FWEL%LRC    )
       DEALLOCATE( FWEL%MASTER_LOC )
       !
       IF(FWEL%LOAD_TYPE == 1) THEN
           DEALLOCATE( FWEL%SP_START )
           DEALLOCATE( FWEL%SP_END   )
       END IF
       !
       DEALLOCATE( FWEL%FRAC )
       !IF( FWEL%PRORATE_Q>0 )  DEALLOCATE( FWEL%FRAC )
       IF( FWEL%SMOOTH   >0 )  DEALLOCATE( FWEL%Qsmf )
       IF(ALLOCATED(FWEL%AUX)) DEALLOCATE( FWEL%AUX  )  !NAUX COULD BE SET TO A NEW VALUE
       !
       FWEL%DIM=0 
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE FINAL_DEALLOCATE_FWELL_DIMENSIONS(FWEL)
    TYPE(FARM_WELL_DATA), INTENT(INOUT)::FWEL 
    !
    ! DEALLOCATE INTERNAL INTERNAL GLOBAL VARIABLES
    IF(FWEL%NF == ONE) THEN
                        DEALLOCATE(FWEL%OUT_INPUT   )
                        DEALLOCATE(FWEL%OUT_BYMNW   )
                        DEALLOCATE(FWEL%OUT_BYWELL  )
                        DEALLOCATE(FWEL%OUT_BYFARM  )
                        DEALLOCATE(FWEL%OUT_SMOOTH  )
                        DEALLOCATE(FWEL%WRN         )
                        DEALLOCATE(FWEL%WRN2        )
                        DEALLOCATE(FWEL%WRN3        )
                        DEALLOCATE(FWEL%IOUT        )
                        IF(ASSOCIATED( FWEL%MASTER_ACT ))          DEALLOCATE( FWEL%MASTER_ACT )
                        !IF(ASSOCIATED( FWEL%MASTER_WID )           DEALLOCATE( FWEL%MASTER_WID )
                        IF(ASSOCIATED(FWEL%FID_FEED))              DEALLOCATE(FWEL%FID_FEED            )
                        IF(ASSOCIATED(FWEL%SMOOTH_LIM_BY_LAYER ))  DEALLOCATE(FWEL%SMOOTH_LIM_BY_LAYER )
    END IF
    !
    CALL DEALLOCATE_FWELL_DIMENSIONS(FWEL)
    !
    !
    FWEL%OUT_INPUT  => NULL()
    FWEL%OUT_BYMNW  => NULL()
    FWEL%OUT_BYWELL => NULL()
    FWEL%OUT_BYFARM => NULL()
    FWEL%OUT_SMOOTH => NULL()
    FWEL%WRN        => NULL()
    FWEL%WRN2       => NULL()
    FWEL%WRN3       => NULL()
    FWEL%MASTER_ACT => NULL()
    !FWEL%MASTER_WID => NULL()
    FWEL%NFARM      => NULL()
    FWEL%IOUT       => NULL()
    FWEL%FID_FEED   => NULL()
    FWEL%SMOOTH_LIM_BY_LAYER => NULL()
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE APPLY_SFAC_TO_Qini(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !
    IF(FWEL%DIM > Z .AND. FWEL%SFAC .NE. DZ) FWEL%Qini = FWEL%Qini * FWEL%SFAC
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE CALCULATE_QMAX(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !
    IF(FWEL%N > Z) THEN
        IF(FWEL%SMOOTH == Z) THEN
                                 FWEL%QMAX = SUM( FWEL%Qcap, MASK=FWEL%ACT )
        ELSE
                                 CALL CALCULATE_SMOOTHING_FACTOR(FWEL)
                                 FWEL%QMAX = SUM( FWEL%Qcap * FWEL%Qsmf, MASK=FWEL%ACT )
        END IF
    ELSEIF(FWEL%DIM > Z)THEN
                       FWEL%QMAX = DZ
    END IF
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE CALCULATE_QMAXini(FWEL)  ! **NOTE that smoothing is not used here. This is because it is the original max capacity.w
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !
    IF(FWEL%N > Z) THEN
                       FWEL%QMAXini=SUM( FWEL%Qini, MASK=FWEL%ACT )
    ELSEIF(FWEL%DIM > Z)THEN
                       FWEL%QMAXini=DZ
    END IF
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE CALCULATE_FRAC(FWEL)  !ASSUMES FWEL%QMAX HAS BEEN CALCULATED!!!
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    DOUBLE PRECISION:: DIV_QMAX
    !
    IF (FWEL%N > Z .AND. FWEL%PRORATE_Q==ONE ) THEN          !(FWEL%PRORATE_Q==1 .OR. FWEL%PRORATE_ALLOT==1)
      !
      IF(FWEL%QMAX > MINSIZE ) THEN
         !
         DIV_QMAX = UNO/FWEL%QMAX
         IF(FWEL%SMOOTH == Z) THEN
                               WHERE(FWEL%ACT) 
                                              FWEL%FRAC = FWEL%Qcap * DIV_QMAX
                               ELSEWHERE
                                              FWEL%FRAC = DZ
                               END WHERE
         ELSE
                               WHERE(FWEL%ACT) 
                                              FWEL%FRAC = FWEL%Qcap * FWEL%Qsmf * DIV_QMAX
                               ELSEWHERE
                                              FWEL%FRAC = DZ
                               END WHERE
         END IF
      ELSE
                               FWEL%FRAC = DZ
      END IF
    ELSEIF(FWEL%DIM>Z) THEN
                               FWEL%FRAC = DZ
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE CALCULATE_SMOOTHING_FACTOR(FWEL)
    USE GLOBAL, ONLY: HNEW, BOTM, LBOTM
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    DOUBLE PRECISION:: MIN_THICK, SAT_THICK, ZERO_THICK, SMOOTH_LIM
    INTEGER:: I, IL, IR, IC
    !
    IF (FWEL%DIM > Z .AND. FWEL%SMOOTH > Z ) THEN          !(FWEL%PRORATE_Q==1 .OR. FWEL%PRORATE_ALLOT==1)
      ZERO_THICK = 1D-6  
      !
      DO CONCURRENT (I=1:FWEL%DIM, FWEL%ACT(I) .OR. FWEL%EXT(I))  
          IF(FWEL%MNWLOC(I) > Z) THEN
              FWEL%Qsmf(I) = UNO   !NO SMOOTHING FOR MNW LINKED WELLS --MNW ALREADY SMOOTHS BASED ON SEEPAGE FACE
          ELSE
              IL = FWEL%LRC(1,I) 
              IR = FWEL%LRC(2,I) 
              IC = FWEL%LRC(3,I)
              !
              IF(FWEL%SMOOTH_LIM < -0.5D0) THEN
                    SMOOTH_LIM = FWEL%SMOOTH_LIM_BY_LAYER(IL)   !SMOOTH BY LAYER
              ELSE
                    SMOOTH_LIM = FWEL%SMOOTH_LIM                !USE GLOBAL SMOOTH OR SMOOTH BY FARM
              END IF
              !
              IF (FWEL%SMOOTH == ONE) THEN   ! CELL THICKNESS
                    MIN_THICK = SMOOTH_LIM * ( BOTM(IC, IR, LBOTM(IL)-ONE) - BOTM(IC, IR, LBOTM(IL)) )
              ELSE
                    MIN_THICK = SMOOTH_LIM
              END IF
              !
              SAT_THICK = HNEW(IC,IR,IL) - BOTM(IC,IR,LBOTM(IL))
              !
              IF     ( SAT_THICK < ZERO_THICK*MIN_THICK) THEN   ! IF THICKNESS IS MORE THAN 6 ORDERS OF MAGNITUDE LESS MIN, THEN ASSUME ZERO --Its not worth running the well if its producing so little water.
                                                    FWEL%Qsmf(I) = DZ
              ELSEIF ( SAT_THICK > MIN_THICK ) THEN
                                                    FWEL%Qsmf(I) = UNO
              ELSE
                !cof1 = SAT_THICK**2.0D0
                !cof2 = -(2.0D0*SAT_THICK)/(MIN_THICK**3.0D0)
                !cof3 = 3.0D0/(MIN_THICK**2.0D0)
                !FWEL%Qsmf(I) = cof1*(cof2+cof3)                   !      coef1             coef3                            coef2
                                                    FWEL%Qsmf(I) = SAT_THICK*SAT_THICK * (  (3D0/(MIN_THICK*MIN_THICK))  -  (2D0*SAT_THICK/(MIN_THICK*MIN_THICK*MIN_THICK))  )
              END IF
          END IF
          !
      END DO
    !
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE COPY_QCAP_TO_Q_ACT(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER:: I
    !
    IF(FWEL%DIM > Z) THEN
        !
        IF(FWEL%SMOOTH == Z) THEN  !SET UP INITIAL ESTIMATE OF Q (ASSUME ALL WELLS ARE MAXED OUT AND WILL BE REDUCED TO MEET QREQ)
            WHERE(FWEL%ACT) 
                           FWEL%Q = FWEL%Qcap
            ELSEWHERE
                           FWEL%Q = DZ
            END WHERE
        ELSE
            DO CONCURRENT (I=1:FWEL%DIM, FWEL%ACT(I))  
                IF(FWEL%MNWLOC(I) > Z) THEN
                                            FWEL%Q(I) = FWEL%Qcap(I)
                ELSE
                                            FWEL%Q(I) = FWEL%Qcap(I) * FWEL%Qsmf(I)
                END IF
            END DO
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE COPY_QCAP_TO_Q_EXT(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER:: I
    !
    IF(FWEL%DIM > Z) THEN
        !
        IF(FWEL%SMOOTH == Z) THEN  !SET UP INITIAL ESTIMATE OF Q (ASSUME ALL WELLS ARE MAXED OUT AND WILL BE REDUCED TO MEET QREQ)
            DO CONCURRENT (I=1:FWEL%DIM, FWEL%EXT(I));   FWEL%Q(I) = FWEL%Qcap(I)
            END DO
        ELSE
            DO CONCURRENT (I=1:FWEL%DIM, FWEL%EXT(I))  
                IF(FWEL%MNWLOC(I) > Z) THEN
                                            FWEL%Q(I) = FWEL%Qcap(I)
                ELSE
                                            FWEL%Q(I) = FWEL%Qcap(I) * FWEL%Qsmf(I)
                END IF
            END DO
        END IF
    END IF
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE COPY_QINI_TO_QCAP(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !
    IF(FWEL%DIM > Z) FWEL%Qcap = FWEL%Qini
    !
  END SUBROUTINE 
  !
  IMPURE ELEMENTAL SUBROUTINE FWEL_READ_N_PREPARE(FWEL, SP)  !IMPURE FORCES CALLS TO BE IN ORDER OF FARM ID (eg 1, 2, 3, ... NFARM)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER, INTENT(IN):: SP
    !LOGICAL, INTENT(IN):: LFID
    INTEGER:: I, IERR
    CHARACTER(120):: LINE
    !
    IF(FWEL%NF==ONE) THEN;  IF(ASSOCIATED(FWEL%MASTER_ACT)) FWEL%MASTER_ACT = FALSE
    END IF
    !
    IF( FWEL%DIM == Z .AND. FWEL%LOAD_TYPE.NE.2 ) RETURN  !IF FWEL%LOAD_TYPE==2 THEN DIM WILL CHANGE WITH EACH FID LINEFEED READ
    !
    SELECT CASE(FWEL%LOAD_TYPE)
    CASE(1)                  !LIST LOADED
            !
            !IF (LFID) THEN
                          FWEL%ACT =  FWEL%SP_START .LE. SP  .AND.  SP .LE. FWEL%SP_END
            !ELSE
            !              FWEL%ACT = FALSE
            !END IF
            !
    CASE(2) ! LINEFEED FID CHANGING
            !
            CALL FWEL_FID_LINEFEED_LOAD(FWEL, SP)
            IF (FWEL%DIM == Z) THEN
                FWEL%N = Z
                RETURN
            ELSE
                FWEL%ACT = TRUE 
            END IF
            !
    CASE(3) ! LINEFEED CAPACITY CHANGING
            !
            CALL READ_TO_DATA(LINE, FWEL%FEED%IU)  !MOVE TO DATE
            BACKSPACE(FWEL%FEED%IU)
            !
            READ(FWEL%FEED%IU,*,IOSTAT=IERR) FWEL%Qini
                                            IF(IERR.NE.Z) CALL FILE_IO_ERROR( IERR, FWEL%FEED%IU, LINE=LINE, OUTPUT=FWEL%IOUT, MSG= "FMP FWELL LINEFEED ERROR WHILE READING CURRENT STRESS PERIOD'S FEED")
            !IF (LFID) THEN
                          FWEL%ACT = FWEL%Qini == FWEL%Qini;     CALL APPLY_SFAC_TO_QINI(FWEL)
            !ELSE
            !              FWEL%ACT = FALSE
            !END IF
            !
    END SELECT
    !
    FWEL%Q   = DZ     !INITIALIZE PUMPAGE
    FWEL%DMD = DZ     !INITIALIZE PUMPAGE
    FWEL%EXT = FALSE  !INITIALIZE ALL EXTERNAL CONNECTIONS TO FALSE
    !
    CALL FWEL_IBOUND_CHECK(FWEL)
    !
    DO CONCURRENT(I=ONE:FWEL%DIM, FWEL%ACT(I)); FWEL%MASTER_ACT( FWEL%MASTER_LOC(I) ) = TRUE
    END DO
    !
  END SUBROUTINE 
  !
  PURE ELEMENTAL SUBROUTINE FWEL_READ_N_PREPARE_SETUP(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !
    IF( FWEL%DIM == Z ) RETURN
    !
    FWEL%N = COUNT(FWEL%ACT)
    !
    CALL COPY_QINI_TO_QCAP(FWEL)
    CALL CALCULATE_QMAX(FWEL)
    !
    CALL CALCULATE_QMAXini(FWEL)
    !
  END SUBROUTINE
  !  
  !
  IMPURE ELEMENTAL SUBROUTINE FWEL_IBOUND_CHECK(FWEL)
    USE GLOBAL, ONLY: IBOUND
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER:: I, J, FIRSTNODE, LASTNODE
    LOGICAL:: IBOUND_CHANGE
    !
    IF(FWEL%DIM == Z) RETURN
    !
    IBOUND_CHANGE = FALSE
    DO CONCURRENT (I=1:FWEL%DIM, (FWEL%ACT(I) .OR. FWEL%EXT(I)) .AND. FWEL%LRC(1,I)>0)
        !
        IF ( IBOUND(FWEL%LRC(3,I),FWEL%LRC(2,I),FWEL%LRC(1,I)) == 0 ) THEN
            IBOUND_CHANGE = TRUE
            FWEL%ACT(I)=FALSE
            FWEL%EXT(I)=FALSE
            !
            IF( FWEL%MNWLOC(I) > Z ) THEN
                  J=FWEL%MNWLOC(I)
                  !
                  FIRSTNODE=NINT( MNW2(4,J) )
                  LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                  !
                  MNW2(5 ,J) = DZ                                                ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
                  MNW2(18,J) = DZ 
                  MNWNOD(4,FIRSTNODE:LASTNODE) = DZ
                  !
                  WRITE(FWEL%IOUT,'(3A)') 'FMP WELL "',FWEL%WELLID(I),'" IS PUMPAGE IS REMOVED FROM THE REMAINDER OF THE CURRENT STRESS PERIOD BECAUSE IT IS LOCATED IN AN IBOUND=0 CELL. THIS WELL IS ALSO LINKED TO MNW2, SO ITS MNW2 DESIRED PUMPING RATE IS NOW SET TO ZERO FOR THE REMAINDER OF THE STRESS PERIOD.'
            ELSE
                  WRITE(FWEL%IOUT,'(3A)') 'FMP WELL "',FWEL%WELLID(I),'" IS PUMPAGE IS REMOVED FROM THE REMAINDER OF THE CURRENT STRESS PERIOD BECAUSE IT IS LOCATED IN AN IBOUND=0 CELL.'
            END IF
        END IF
    END DO  
    !
    IF (IBOUND_CHANGE) THEN
                           FWEL%N = COUNT(FWEL%ACT)
                           CALL CALCULATE_QMAX(FWEL)
                           !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE INITIALIZE_FMP_WELL_DATA( BL, FWEL, LN )  !=ASSUMES FWEL IS OF SIZE NFARMS  ---Called from FMP main to load FWEL 
    USE GLOBAL,                          ONLY: NLAY, IUNIT
    USE ULOAD_AND_SFAC_INTERFACE,        ONLY: SFAC_DATA
    USE GENERIC_OUTPUT_FILE_INSTRUCTION, ONLY: GENERIC_OUTPUT_FILE_DEPOINT
    USE WARNING_TYPE_INSTRUCTION,        ONLY: WARNING_TYPE_DEPOINT
    CLASS(GENERIC_BLOCK_READER),                   INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(FARM_WELL_DATA),CONTIGUOUS,DIMENSION(:), INTENT(INOUT):: FWEL !FARMWELL
    CHARACTER(*),                                  INTENT(INOUT):: LN
    !CONTIGUOUS:: FWEL
    TYPE(SFAC_DATA):: SFAC
    TYPE(SFAC_DATA):: SMOOTH_FACTOR
    TYPE(SFAC_DATA):: PRORATE_Q_FACTOR
    TYPE(SFAC_DATA):: AUX_QMAXRESET
    TYPE(SFAC_DATA):: AUX_NOCIRNOQ
    TYPE(SFAC_DATA):: AUX_MWN_ON
    TYPE(SFAC_DATA):: AUX_MWN_OFF
    TYPE(SFAC_DATA):: AUX_MWN_SPREAD
    !TYPE(GENERIC_OUTPUT_FILE):: TRANSCRIPT
    !CHARACTER(:), ALLOCATABLE:: LN   !ORIGINALLY AUTOALOCATED
    CHARACTER(20):: KEY
    CHARACTER(20), DIMENSION(:), ALLOCATABLE:: MASTER_WID
    CHARACTER(5):: ERROR
    INTEGER:: I,J,K,N,LLOC,ISTART,ISTOP, NFARM, IERR, MXDIM, FD, INPUT_TYPE
    LOGICAL:: BINARY, BYAVERAGE
    REAL:: R
    INTEGER:: INSTYLE   !INSTYLE = 0 for LRC load (default); INSTYLE = 1 for Lay for L and XY for RC; INSTYLE = ADD 10 to use elevation for L (viz 10 and 11), INSTYLE = ADD 20 to use elevation for L (viz 20 and 21)
    TYPE(GENERIC_BLOCK_READER):: INWEL
    !
    WRITE(BL%IOUT,'(/A/)') 'FWELL BLOCK FOUND AND NOW LOADING PROPERTIES'
    !
    INSTYLE = Z
    INPUT_TYPE = Z
    CALL INWEL%INIT(BL%IU,BL%IOUT)
    INWEL%NAME = BL%NAME
    !
    FWEL%NAUX=Z
    !
    NFARM = SIZE(FWEL)
    DO CONCURRENT(I=ONE:NFARM) 
                              FWEL(I)%NF = I
    END DO
    !FWEL%IGRID = IGRID
    ERROR='ERROR'
    !
    CALL BL%START()
    IF( BL%LINE==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !
    IF(IUNIT(63).NE.Z) THEN  !NWT REQUIRES THAT SMOOTHING BE TURNED ON
        FWEL%SMOOTH = ONE
        FWEL%SMOOTH_LIM = 0.05D0
    END IF
    !
    !IF(IGRID==ONE) THEN  !ALLOCATE INTERNAL GLOBAL VARIABLES FOR LGR
    !                 ALLOCATE( LIST_UNIT(NGRID)                   )
    !                 ALLOCATE( OUT_INPUT                  (NGRID) )  !DIM BY NGRID
    !                 ALLOCATE( OUT_BYWELL                 (NGRID) )  !DIM BY NGRID
    !                 ALLOCATE( OUT_BYFARM                  (NGRID) )
    !                 ALLOCATE( OUT_SMOOTH                 (NGRID) )
    !                 ALLOCATE( GLOBAL_FEED                (NGRID) )
    !END IF
    !
    ! XXX_DEPOINT(cls) => GFORTRAN compiler error work-around for pointer data type FINAL statement
    IF(ASSOCIATED(FWEL(ONE)%SMOOTH_LIM_BY_LAYER ))  DEALLOCATE(FWEL(ONE)%SMOOTH_LIM_BY_LAYER )
    IF(ASSOCIATED(FWEL(ONE)%OUT_INPUT           ))  CALL GENERIC_OUTPUT_FILE_DEPOINT(FWEL(ONE)%OUT_INPUT )
    IF(ASSOCIATED(FWEL(ONE)%OUT_BYMNW           ))  CALL GENERIC_OUTPUT_FILE_DEPOINT(FWEL(ONE)%OUT_BYMNW)
    IF(ASSOCIATED(FWEL(ONE)%OUT_BYWELL          ))  CALL GENERIC_OUTPUT_FILE_DEPOINT(FWEL(ONE)%OUT_BYWELL)
    IF(ASSOCIATED(FWEL(ONE)%OUT_BYFARM          ))  CALL GENERIC_OUTPUT_FILE_DEPOINT(FWEL(ONE)%OUT_BYFARM)
    IF(ASSOCIATED(FWEL(ONE)%OUT_SMOOTH          ))  CALL GENERIC_OUTPUT_FILE_DEPOINT(FWEL(ONE)%OUT_SMOOTH)
    IF(ASSOCIATED(FWEL(ONE)%NFARM               ))  DEALLOCATE(FWEL(ONE)%NFARM)
    IF(ASSOCIATED(FWEL(ONE)%IOUT                ))  DEALLOCATE(FWEL(ONE)%IOUT )
    IF(ASSOCIATED(FWEL(ONE)%WRN                 ))  CALL WARNING_TYPE_DEPOINT(FWEL(ONE)%WRN )
    IF(ASSOCIATED(FWEL(ONE)%WRN2                ))  CALL WARNING_TYPE_DEPOINT(FWEL(ONE)%WRN2)
    IF(ASSOCIATED(FWEL(ONE)%WRN3                ))  CALL WARNING_TYPE_DEPOINT(FWEL(ONE)%WRN3)
    IF(ASSOCIATED(FWEL(ONE)%FID_FEED            ))  DEALLOCATE(FWEL(ONE)%FID_FEED           )
    !IF(ASSOCIATED(FWEL(ONE)%MASTER_ACT          ))  DEALLOCATE(FWEL(ONE)%MASTER_ACT          )
    !IF(ASSOCIATED(FWEL(ONE)%MASTER_WID          ))  DEALLOCATE(FWEL(ONE)%MASTER_WID          )
    !FWEL(ONE)%MASTER_WID => NULL()
    !
    CALL FWEL_BASIC_ALLOCATE(FWEL,BL%IOUT,NFARM)
    !
    MXDIM=Z
    FWEL%SFAC = UNO
    DO N=ONE, BL%NLINE
      !
      IF(  BL%LINE==ERROR ) EXIT !--THIS SHOULD NOT HAPPEN!!! CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. THIS MAY HAVE HAPPENED BECAUES IT FAILED TO LOAD THE LAST KEYWORD WHICH IS ANY OF THE FOLLOWING: "TIME FRAME" OR "LINEFEED WBS" OR "LINEFEED CAPACITY". PLEASE DOUBLE CHECK BLOCK SET UP.')
      !
      LLOC=ONE
      CALL GET_WORD(BL%LINE,LLOC,ISTART,ISTOP,KEY)
      !CALL PARSE_WORD(BL%LINE,LLOC,ISTART,ISTOP,TRUE)
      !
      SELECT CASE ( KEY )
      CASE ("SFAC")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                        LN = BL%LIST%LN                                      !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                        !
                        CALL SFAC%INIT()
                        CALL SFAC%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                        !
                        IF ( SFAC%HAS_ALL ) FWEL%SFAC = FWEL%SFAC * SFAC%ALL
                        IF ( SFAC%HAS_ROW ) THEN
                                                DO CONCURRENT (I=ONE:NFARM)
                                                                    FWEL(I)%SFAC = FWEL(I)%SFAC * SFAC%ROW(I)
                                                END DO
                        END IF
                        IF ( SFAC%HAS_COL ) THEN
                                                DO CONCURRENT (I=ONE:NFARM)
                                                                    FWEL(I)%SFAC = FWEL(I)%SFAC * SFAC%COL(I)
                                                END DO
                        END IF
      CASE ("INPUT_OPTION")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("XY")
                                           INSTYLE = INSTYLE + ONE
                        CASE ("ELEVATION")
                                           INSTYLE = INSTYLE + TEN
                        CASE ("DEPTH")
                                           INSTYLE = INSTYLE + 20
                        CASE DEFAULT;      CALL STOP_ERROR(BL%LINE, BL%IU, BL%IOUT, 'FMP FWELL BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "INPUT" WHICH INDICATES NEXT WORD SHOULD BE "XY" OR "ELEVATION"  OR "DEPTH" TO INDICATE HOW INPUT IS INTERPRETED.')
                        END SELECT
                        !
                        IF( ALL( INSTYLE .NE. [0, 1, 10, 11, 20, 21] )) CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='FMP FWELL BLOCK KEYWORD ERROR. INPUT STYLE DECLARATION CONTAINS AN ERROR. EITHER OU SPECIFIED XY TWICE OR CONTAINED ELEVATION AND DEPTH AS KEYWORDS.')
      CASE ("SMOOTH", "SMOOTHING")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC  !REQUIRED TO USE SFAC_DATA_INTERFACE TO PARSE DATA
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BY_FRACTION", "BYFRACTION")
                                           FWEL%SMOOTH = ONE
                        CASE ("BY_THICK", "BYTHICK")
                                           FWEL%SMOOTH = TWO
                        CASE DEFAULT;      CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='FMP FWELL BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "SMOOTH" WHICH INDICATES NEXT WORD SHOULD BE "BY_FRACTION" OR "BY_THICK" TO INDICATE THE TYPE OF PRORATION.')
                        END SELECT
                        !
                        !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                        LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                        !
                        CALL SMOOTH_FACTOR%INIT()
                        CALL SMOOTH_FACTOR%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, "BYLAYER", NLAY, 'BYFARM', NFARM)
                        !
                        IF     ( SMOOTH_FACTOR%HAS_ROW) THEN
                                                            DO CONCURRENT(I=ONE:NFARM) 
                                                                                FWEL(I)%SMOOTH_LIM = SMOOTH_FACTOR%ROW(I)   !LIST OF NFARM THICKNESSES OF FRACTIONS
                                                            END DO
                        ELSEIF ( SMOOTH_FACTOR%HAS_EX1) THEN
                                                            DO CONCURRENT(I=ONE:NFARM) 
                                                                                FWEL(I)%SMOOTH_LIM = SMOOTH_FACTOR%EX1(I)   !LIST OF NFARM THICKNESSES OF FRACTIONS
                                                            END DO
                        ELSEIF ( SMOOTH_FACTOR%HAS_COL) THEN
                                                            FWEL%SMOOTH_LIM = DNEG   !FLAG TO INDICATE THAT IT IS SPECIFIED BY LAYER
                                                            ALLOCATE( FWEL(ONE)%SMOOTH_LIM_BY_LAYER, SOURCE=SMOOTH_FACTOR%COL )
                                                            !
                                                            DO I=2, NFARM
                                                                      FWEL(I)%SMOOTH_LIM_BY_LAYER => FWEL(ONE)%SMOOTH_LIM_BY_LAYER
                                                            END DO
                        ELSEIF ( SMOOTH_FACTOR%HAS_ALL) THEN
                                                            FWEL%SMOOTH_LIM = SMOOTH_FACTOR%ALL   !LIST OF NFARM THICKNESSES OF FRACTIONS
                        ELSE
                                                            CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='FMP FWELL BLOCK UNKNOWN ERROR. FAILED TO LOAD SMOOTH FACTOR.')
                        END IF
      CASE ("PRORATE_DEMAND", "PRORATE")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYQMAX", "BYAVERAGE", "BYCAP", "BYCAPACITY")
                                         BYAVERAGE = BL%LINE(ISTART:ISTOP) == "BYAVERAGE"
                                         FWEL%PRORATE_Q = ONE                                !ASSUME THAT IT IS SET FOR BYQMAX
                                         !
                                         CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                                         READ(  BL%LINE(ISTART:ISTOP),*, IOSTAT=IERR) R
                                         !
                                         IF(IERR==Z                           .OR.  &
                                            BL%LINE(ISTART:ISTOP) == 'BYWBS'  .OR.  &
                                            BL%LINE(ISTART:ISTOP) == 'BYFARM' .OR.  &
                                            BL%LINE(ISTART:ISTOP) == 'ALL'       ) THEN  !PROCESS INFORMATION WITH SFAC DATA TYPE
                                               !
                                               !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                               LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                               !
                                               CALL PRORATE_Q_FACTOR%INIT()
                                               CALL PRORATE_Q_FACTOR%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                               !
                                               IF     ( PRORATE_Q_FACTOR%HAS_ROW ) THEN
                                                          WHERE (PRORATE_Q_FACTOR%ROW > 0.1D0)
                                                                    FWEL%PRORATE_Q = ONE
                                                          ELSEWHERE
                                                                    FWEL%PRORATE_Q = Z
                                                          END WHERE
                                               ELSEIF ( PRORATE_Q_FACTOR%HAS_COL ) THEN
                                                          WHERE (PRORATE_Q_FACTOR%COL > 0.1D0)
                                                                    FWEL%PRORATE_Q = ONE
                                                          ELSEWHERE
                                                                    FWEL%PRORATE_Q = Z
                                                          END WHERE
                                               ELSEIF(PRORATE_Q_FACTOR%HAS_ALL) THEN
                                                      IF (PRORATE_Q_FACTOR%ALL > 0.1D0) THEN
                                                                    FWEL%PRORATE_Q = ONE
                                                      ELSE
                                                                    FWEL%PRORATE_Q = Z
                                                      END IF
                                               ELSE
                                                          CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='FMP FWELL BLOCK UNKNOWN ERROR. FAILED TO LOAD PRORATE BYQMAX.')
                                               END IF
                                         END IF
                                         !
                                         IF (BYAVERAGE) THEN  !THIS MEANS THE OPPOSITE OF BYQMAX, CHANGE FLAG
                                             DO CONCURRENT (I=1:NFARM)
                                                   IF (FWEL(I)%PRORATE_Q == ONE) THEN
                                                            FWEL(I)%PRORATE_Q = Z
                                                   ELSE
                                                            FWEL(I)%PRORATE_Q = ONE
                                                   END IF
                                             END DO
                                         END IF
                                         !
                        !CASE ("ALLOTMENT"); FWEL%PRORATE_ALLOT= ONE
                        CASE DEFAULT;      CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,MSG='FMP FWELL BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "PRORATE" WHICH INDICATES NEXT WORD SHOULD BE "BYQMAX" OR "BYAVERAGE" TO INDICATE THE TYPE OF PRORATION.')
                        END SELECT
      CASE ("PRINT")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        BINARY = FALSE
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        IF(BL%LINE(ISTART:ISTOP) == 'BINARY') THEN
                            BINARY = TRUE
                            CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        END IF
                        !
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("LIST")
                                          DO CONCURRENT(I=ONE:NFARM) 
                                                               FWEL(I)%LISTPRINT = TRUE
                                          END DO
                        CASE ("BYWELL")
                                          CALL FWEL(ONE)%OUT_BYWELL%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("BYMNW", "BYMNW2")
                                          CALL FWEL(ONE)%OUT_BYMNW %OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("BYFARM", "BYWBS")
                                          CALL FWEL(ONE)%OUT_BYFARM%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                        CASE ("SMOOTHING")
                                          CALL FWEL(ONE)%OUT_SMOOTH%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,BINARY=BINARY,SPLITMAXCOUNT=11)
                                                                                                   !
                                                                                                   IF(BINARY) CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP FWELL: FARM WELL "PRINT SMOOTHING" CONTAINED KEYWORD "BINARY" BUT IT DOES NOT SUPPORT BINARY WRITING, SO INSTEAD A TEXT FILE WILL BE WRITTEN. THIS MAY CAUSE A CRASH IF YOU OPENED THE FILE UNIT WITH DATA(BINARY).',INLINE=TRUE) 
                        CASE ("INPUT")
                                          CALL FWEL(ONE)%OUT_INPUT%OPEN(BL%LINE,LLOC,BL%IOUT,BL%IU,SPLITMAXCOUNT=11)
                                                                                                   !
                                                                                                   IF(BINARY) CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP FWELL: FARM WELL "PRINT INPUT" CONTAINED KEYWORD "BINARY" BUT IT DOES NOT SUPPORT BINARY WRITING, SO INSTEAD A TEXT FILE WILL BE WRITTEN. THIS MAY CAUSE A CRASH IF YOU OPENED THE FILE UNIT WITH DATA(BINARY).',INLINE=TRUE) 
                                                                                                   !
                        CASE DEFAULT;     CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,'FMP FWELL BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "PRINT", BUT THE NEXT WORD WAS NOT IDENTIFIED. WORDS EXPECTED ARE: "BYFARM", "BYWELL", "SMOOTHING", "INPUT"')
                        END SELECT
      CASE ("QMAXRESET")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYFARM", "BYWBS", "ALL")!PROCESS INFORMATION WITH SFAC DATA TYPE
                                         !
                                         !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                         LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                         !
                                         CALL AUX_QMAXRESET%INIT()
                                         CALL AUX_QMAXRESET%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                         !
                                         IF ( AUX_QMAXRESET%HAS_ROW ) THEN
                                                     WHERE (AUX_QMAXRESET%ROW > 0.1D0)
                                                             FWEL%QMAXRESET = TRUE
                                                     ELSEWHERE
                                                             FWEL%QMAXRESET = FALSE
                                                     END WHERE
                                         ELSEIF ( AUX_QMAXRESET%HAS_COL ) THEN
                                                     WHERE (AUX_QMAXRESET%COL > 0.1D0)
                                                             FWEL%QMAXRESET = TRUE
                                                     ELSEWHERE
                                                             FWEL%QMAXRESET = FALSE
                                                     END WHERE
                                         ELSE
                                             IF(AUX_QMAXRESET%HAS_ALL) THEN
                                                 FWEL%QMAXRESET = AUX_QMAXRESET%ALL > 0.1D0
                                             ELSE
                                                 FWEL%QMAXRESET = TRUE
                                             END IF
                                         END IF
                                         !
                        CASE DEFAULT      
                                     FWEL%QMAXRESET = TRUE
                        END SELECT
      CASE ("NOQMAXRESET", "NO_QMAXRESET")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        FWEL%QMAXRESET = FALSE
      CASE ("NOCIRNOQ")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYFARM", "ALL")!PROCESS INFORMATION WITH SFAC DATA TYPE
                                         !
                                         !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                         LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                         !
                                         CALL AUX_NOCIRNOQ%INIT()
                                         CALL AUX_NOCIRNOQ%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                         !
                                         IF     ( AUX_NOCIRNOQ%HAS_ROW ) THEN
                                                     WHERE (AUX_NOCIRNOQ%ROW > 0.1D0)
                                                             FWEL%NOCIRNOQ = TRUE
                                                     ELSEWHERE
                                                             FWEL%NOCIRNOQ = FALSE
                                                     END WHERE
                                         ELSEIF ( AUX_NOCIRNOQ%HAS_COL ) THEN
                                                     WHERE (AUX_NOCIRNOQ%COL > 0.1D0)
                                                             FWEL%NOCIRNOQ = TRUE
                                                     ELSEWHERE
                                                             FWEL%NOCIRNOQ = FALSE
                                                     END WHERE
                                         ELSE
                                             IF(AUX_NOCIRNOQ%HAS_ALL) THEN
                                                 FWEL%NOCIRNOQ = AUX_NOCIRNOQ%ALL > 0.1D0
                                             ELSE
                                                 FWEL%NOCIRNOQ = TRUE
                                             END IF
                                         END IF
                                         !
                        CASE DEFAULT      
                                     FWEL%NOCIRNOQ = TRUE
                        END SELECT
      CASE ("MNW2_AUTOMATIC_ON", "MNW_AUTOMATIC_ON")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYFARM", "ALL")!PROCESS INFORMATION WITH SFAC DATA TYPE
                                         !
                                         !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                         LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                         !
                                         CALL AUX_MWN_ON%INIT()
                                         CALL AUX_MWN_ON%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                         !
                                         IF     ( AUX_MWN_ON%HAS_ROW ) THEN
                                                     WHERE (AUX_MWN_ON%ROW > 0.1D0)
                                                             FWEL%MNW_ON = TRUE
                                                     ELSEWHERE
                                                             FWEL%MNW_ON = FALSE
                                                     END WHERE
                                         ELSEIF ( AUX_MWN_ON%HAS_COL ) THEN
                                                     WHERE (AUX_MWN_ON%COL > 0.1D0)
                                                             FWEL%MNW_ON = TRUE
                                                     ELSEWHERE
                                                             FWEL%MNW_ON = FALSE
                                                     END WHERE
                                         ELSE
                                             IF(AUX_MWN_ON%HAS_ALL) THEN
                                                 FWEL%MNW_ON = AUX_MWN_ON%ALL > 0.1D0
                                             ELSE
                                                 FWEL%MNW_ON = TRUE
                                             END IF
                                         END IF
                                         !
                        CASE DEFAULT      
                                     FWEL%MNW_ON = TRUE
                        END SELECT
      CASE ("MNW2_AUTOMATIC_OFF", "MNW_AUTOMATIC_OFF")
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYFARM", "ALL")!PROCESS INFORMATION WITH SFAC DATA TYPE
                                         !
                                         !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                         LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                         !
                                         CALL AUX_MWN_OFF%INIT()
                                         CALL AUX_MWN_OFF%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                         !
                                         IF     ( AUX_MWN_OFF%HAS_ROW ) THEN
                                                     WHERE (AUX_MWN_OFF%ROW > 0.1D0)
                                                             FWEL%MNW_OFF = TRUE
                                                     ELSEWHERE
                                                             FWEL%MNW_OFF = FALSE
                                                     END WHERE
                                         ELSEIF ( AUX_MWN_OFF%HAS_COL ) THEN
                                                     WHERE (AUX_MWN_OFF%COL > 0.1D0)
                                                             FWEL%MNW_OFF = TRUE
                                                     ELSEWHERE
                                                             FWEL%MNW_OFF = FALSE
                                                     END WHERE
                                         ELSE
                                             IF(AUX_MWN_OFF%HAS_ALL) THEN
                                                 FWEL%MNW_OFF = AUX_MWN_OFF%ALL > 0.1D0
                                             ELSE
                                                 FWEL%MNW_OFF = TRUE
                                             END IF
                                         END IF
                                         !
                        CASE DEFAULT      
                                     FWEL%MNW_OFF = TRUE
                        END SELECT
      CASE ("MNW2_PUMP_SPREAD", "MNW_PUMP_SPREAD") !FWEL%MNW_SPREAD
                        IF(INPUT_TYPE > Z) INPUT_TYPE = -INPUT_TYPE  !FLIP SIGN TO STOP ADDING LINES
                        !
                        I=LLOC
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("BYFARM", "ALL")!PROCESS INFORMATION WITH SFAC DATA TYPE
                                         !
                                         !IF(.NOT. ALLOCATED(LN)) ALLOCATE(CHARACTER(700):: LN)   !DUBLICATE VARIABLE MADE IN CASE BL%LIST%LN IS VERY SMALL
                                         LN(:) = BL%LINE(I:)                                  !ALLOCATABLE STRINGS MUST INCLUDE INDEX
                                         !
                                         CALL AUX_MWN_SPREAD%INIT()
                                         CALL AUX_MWN_SPREAD%LOAD(LN, BL%IU, BL%IOUT, 'BYWBS', NFARM, 'BYFARM', NFARM)
                                         !
                                         IF     ( AUX_MWN_SPREAD%HAS_ROW ) THEN
                                             DO CONCURRENT (I=ONE:NFARM)
                                                 IF    (AUX_MWN_SPREAD%ROW(I) < 0.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = Z
                                                 ELSEIF(AUX_MWN_SPREAD%ROW(I) < 1.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = ONE
                                                 ELSEIF(AUX_MWN_SPREAD%ROW(I) < 2.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = TWO
                                                 END IF
                                             END DO
                                         ELSEIF ( AUX_MWN_SPREAD%HAS_COL ) THEN
                                             DO CONCURRENT (I=ONE:NFARM)
                                                 IF    (AUX_MWN_SPREAD%COL(I) < 0.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = Z
                                                 ELSEIF(AUX_MWN_SPREAD%COL(I) < 1.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = ONE
                                                 ELSEIF(AUX_MWN_SPREAD%COL(I) < 2.1D0) THEN
                                                                                           FWEL(I)%MNW_SPREAD = TWO
                                                 END IF
                                             END DO
                                         ELSE
                                             IF(AUX_MWN_SPREAD%HAS_ALL) THEN
                                                 IF    (AUX_MWN_SPREAD%ALL < 0.1D0) THEN
                                                                                           FWEL%MNW_SPREAD = Z
                                                 ELSEIF(AUX_MWN_SPREAD%ALL < 1.1D0) THEN
                                                                                           FWEL%MNW_SPREAD = ONE
                                                 ELSEIF(AUX_MWN_SPREAD%ALL < 2.1D0) THEN
                                                                                           FWEL%MNW_SPREAD = TWO
                                                 END IF
                                             ELSE
                                                                                           FWEL%MNW_SPREAD = Z
                                             END IF
                                         END IF
                        CASE("BYCOND", "BY_COND");  FWEL%MNW_SPREAD = Z
                        CASE("BYCOUNT","BY_COUNT"); FWEL%MNW_SPREAD = ONE
                        CASE("BYTOP",  "BY_TOP" );  FWEL%MNW_SPREAD = TWO
                                         !
                        CASE DEFAULT      
                                     FWEL(I)%MNW_SPREAD = Z
                        END SELECT
      CASE ("TIME")     !FRAME
                        INPUT_TYPE = ONE
                        CALL INWEL%ADD_LINE(BL%LINE)
                        !
      CASE ("LINEFEED", "FEEDFILE", "FEEDFILES")
                        CALL PARSE_WORD_UP(BL%LINE,LLOC,ISTART,ISTOP)
                        SELECT CASE ( BL%LINE(ISTART:ISTOP) )
                        CASE ("WBS","FID","FARM")
                                         INPUT_TYPE = TWO       
                        CASE ("CAPACITY")
                                         INPUT_TYPE = THREE
                                         !
                        CASE DEFAULT;    CALL STOP_ERROR(BL%LINE,BL%IU,BL%IOUT,'FMP FWELL BLOCK KEYWORD ERROR. IDENTIFIED KEYWORD "'//BL%LINE(ISTART:ISTOP)//'", BUT THE NEXT WORD WAS NOT IDENTIFIED. WORDS EXPECTED ARE: "WBS" OR "CAPACITY"')
                        END SELECT
                        !
                        CALL INWEL%ADD_LINE(BL%LINE)
                        !
      !CASE ("FID", "WBS")      !LINEFEED
      !                  CALL FWEL_LOAD_FID_LINEFEED(BL,FWEL,INSTYLE)
      !                  CALL APPLY_SFAC_TO_QINI(FWEL) !--ALL FARMS SHOULD HAVE THE SAME LOAD TYPE --ONLY APPLY SFAC FOR LIST LOAD...LINEFEED READS NEW DATA EVERY SP SO IT WILL BE APPLIED THEN
      !                  EXIT
      !                  !
      !CASE ("CAPACITY") !LINEFEED
      !                  CALL FWEL_LOAD_CAPACITY_LINEFEED(BL,FWEL,INSTYLE)
      !                  EXIT
      CASE DEFAULT
                 IF(INPUT_TYPE > Z) THEN
                     CALL INWEL%ADD_LINE(BL%LINE)
                 ELSE
                     CALL WARNING_MESSAGE(OUTPUT=BL%IOUT,MSG='FMP FWELL BLOCK FOUND UNKNOWN KEYWORD "'//BL%LINE(ISTART:ISTOP)//'" ***IT WILL BE IGNORED***',INLINE=TRUE,CMD_PRINT=TRUE)
                 END IF
      END SELECT
      !
      CALL BL%NEXT()
      !
    END DO
    !
    INPUT_TYPE = ABS(INPUT_TYPE)  !FLIP SIGN BACK
    !
    CALL INWEL%START()
    SELECT CASE(INPUT_TYPE)
    CASE(  ONE)
               CALL FWEL_LOAD_LIST(INWEL,FWEL,INSTYLE)
               CALL APPLY_SFAC_TO_QINI(FWEL) !--ALL FARMS SHOULD HAVE THE SAME LOAD TYPE --ONLY APPLY SFAC FOR LIST LOAD...LINEFEED READS NEW DATA EVERY SP SO IT WILL BE APPLIED THEN
               DO CONCURRENT (I=ONE:NFARM); MXDIM = MXDIM + FWEL(I)%DIM
               END DO
    CASE(  TWO)
               CALL FWEL_LOAD_FID_LINEFEED(INWEL,FWEL,INSTYLE)
               CALL APPLY_SFAC_TO_QINI(FWEL) !--ALL FARMS SHOULD HAVE THE SAME LOAD TYPE --ONLY APPLY SFAC FOR LIST LOAD...LINEFEED READS NEW DATA EVERY SP SO IT WILL BE APPLIED THEN
               MXDIM=ABS(FWEL(ONE)%DIM)
    CASE(THREE)
               CALL FWEL_LOAD_CAPACITY_LINEFEED(INWEL,FWEL,INSTYLE)
               DO CONCURRENT (I=ONE:NFARM); MXDIM = MXDIM + FWEL(I)%DIM
               END DO
    CASE DEFAULT
               CALL STOP_ERROR(BLNK,BL%IU,BL%IOUT,'FMP FWELL BLOCK FAILED TO LOCATE ONE OF THE FOLLOWING KEYWORDS:'//NL//'TIME  FRAME'//NL//'LINEFEED WBS'//NL//'LINEFEED CAPACITY'//NL//'ONE OF THESE THREE KEYWORDS MUST BE PRESENT TO CONTINUE A SIMULATION WITH FARM WELLS')
    END SELECT
    
    !
    !IF(ALLOCATED(LN)) DEALLOCATE(LN)
    !
    DO I=TWO, NFARM
              FWEL(I)%FID_FEED => FWEL(ONE)%FID_FEED
    END DO
    !
    ! SET UP GLOBAL ACTIVE ARRAYS (NECESARY FOR MNW LINK)
    IF(MXDIM>Z) THEN
        !
        LLOC = Z  !USED TO COUNT UNIQUE NAMES
        ALLOCATE(MASTER_WID(MXDIM))
        MASTER_WID=BLNK
        J=ONE
        IF (FWEL(ONE)%LOAD_TYPE == TWO ) THEN
          DO CONCURRENT(FD=ONE:SIZE(FWEL(ONE)%FID_FEED))
            I=ONE
            DO CONCURRENT(K=ONE:SIZE(FWEL(ONE)%FID_FEED(FD)%MASTER_LOC))
              ASSOCIATE(MASTER_LOC=>FWEL(ONE)%FID_FEED(FD)%MASTER_LOC(K), WELLID=>FWEL(ONE)%FID_FEED(FD)%WELLID(K))
                MASTER_LOC = Z
                DO N=ONE, J
                    IF(WELLID==MASTER_WID(N))THEN
                        MASTER_LOC = N
                        EXIT
                    END IF
                END DO
                IF(MASTER_LOC == Z) THEN
                      IF(J > LLOC) LLOC = J
                      MASTER_WID(J) = WELLID
                      MASTER_LOC = J
                      J = J + ONE
                      IF(J>MXDIM)J=MXDIM
                END IF
              END ASSOCIATE
            END DO
          END DO
        ELSE
            DO CONCURRENT(I=ONE:NFARM      )
            DO CONCURRENT(K=ONE:FWEL(I)%DIM)
                FWEL(I)%MASTER_LOC(K) = Z
                DO N=ONE, J
                    IF(FWEL(I)%WELLID(K)==MASTER_WID(N))THEN
                        FWEL(I)%MASTER_LOC(K) = N
                        EXIT
                    END IF
                END DO
                IF(FWEL(I)%MASTER_LOC(K) == Z) THEN
                      IF(J > LLOC) LLOC = J
                      MASTER_WID(J) = FWEL(I)%WELLID(K)
                      FWEL(I)%MASTER_LOC(K) = J
                      J = J + ONE
                      IF(J>MXDIM)J=MXDIM
                END IF
            END DO; END DO
        END IF
        DEALLOCATE(MASTER_WID)
        !FORALL(I=TWO:NFARM) FWEL(I)%MASTER_WID => FWEL(ONE)%MASTER_WID
        !
        ALLOCATE(FWEL(ONE)%MASTER_ACT(LLOC))
        FWEL(ONE)%MASTER_ACT = FALSE
        DO I=TWO, NFARM 
                  FWEL(I)%MASTER_ACT => FWEL(ONE)%MASTER_ACT
        END  DO
    END IF
    !
  END SUBROUTINE
  !
  SUBROUTINE FWEL_BASIC_ALLOCATE(FWEL,IOUT,DIM)  !DIM = 1 or NFARMS
    INTEGER,                               INTENT(IN   ):: IOUT, DIM
    CLASS(FARM_WELL_DATA), DIMENSION(DIM), INTENT(INOUT):: FWEL 
    INTEGER:: I
    !
    ALLOCATE(FWEL(ONE)%WRN )
        CALL FWEL(ONE)%WRN%INIT()
    ALLOCATE(FWEL(ONE)%WRN2 )
        CALL FWEL(ONE)%WRN2%INIT()
    ALLOCATE(FWEL(ONE)%WRN3 )
        CALL FWEL(ONE)%WRN3%INIT()
    !
    ALLOCATE(FWEL(ONE)%NFARM,   SOURCE = DIM )
    ALLOCATE(FWEL(ONE)%IOUT, SOURCE = IOUT )
    ALLOCATE(FWEL(ONE)%OUT_INPUT  )
    ALLOCATE(FWEL(ONE)%OUT_BYMNW  )
    ALLOCATE(FWEL(ONE)%OUT_BYWELL )
    ALLOCATE(FWEL(ONE)%OUT_BYFARM )
    ALLOCATE(FWEL(ONE)%OUT_SMOOTH )
    !
    DO I=TWO, DIM
              FWEL(I)%WRN        => FWEL(ONE)%WRN
              FWEL(I)%WRN2       => FWEL(ONE)%WRN2
              FWEL(I)%WRN3       => FWEL(ONE)%WRN3
              FWEL(I)%NFARM      => FWEL(ONE)%NFARM
              FWEL(I)%IOUT       => FWEL(ONE)%IOUT      
              FWEL(I)%OUT_INPUT  => FWEL(ONE)%OUT_INPUT 
              FWEL(I)%OUT_BYMNW  => FWEL(ONE)%OUT_BYMNW
              FWEL(I)%OUT_BYWELL => FWEL(ONE)%OUT_BYWELL
              FWEL(I)%OUT_BYFARM => FWEL(ONE)%OUT_BYFARM
              FWEL(I)%OUT_SMOOTH => FWEL(ONE)%OUT_SMOOTH
    END DO
    !FORALL(I=TWO:DIM) FWEL(I)%WRN        => FWEL(ONE)%WRN
    !FORALL(I=TWO:DIM) FWEL(I)%WRN2       => FWEL(ONE)%WRN2
    !FORALL(I=TWO:DIM) FWEL(I)%WRN3       => FWEL(ONE)%WRN3
    !FORALL(I=TWO:DIM) FWEL(I)%NFARM      => FWEL(ONE)%NFARM
    !FORALL(I=TWO:DIM) FWEL(I)%IOUT       => FWEL(ONE)%IOUT      
    !FORALL(I=TWO:DIM) FWEL(I)%OUT_INPUT  => FWEL(ONE)%OUT_INPUT 
    !FORALL(I=TWO:DIM) FWEL(I)%OUT_BYWELL => FWEL(ONE)%OUT_BYWELL
    !FORALL(I=TWO:DIM) FWEL(I)%OUT_BYFARM => FWEL(ONE)%OUT_BYFARM 
    !FORALL(I=TWO:DIM) FWEL(I)%OUT_SMOOTH => FWEL(ONE)%OUT_SMOOTH
        
    IF(ASSOCIATED(FWEL(ONE)%MASTER_ACT)) DEALLOCATE(FWEL(ONE)%MASTER_ACT,STAT=I)
    DO I=ONE, DIM
              FWEL(I)%MASTER_ACT => NULL()
    END DO
    !FWEL%MASTER_WID => NULL()
    !
  END SUBROUTINE
  !
  SUBROUTINE FWEL_PROPERTY_READ(BL,WELLID,IL,IR,IC,NAUX,AUX,INSTYLE,NF,Q,SPSTART,SPEND)
    USE GLOBAL,                          ONLY:  NPER, NLAY, BOTM, LBOTM, XYGRID
    CLASS(GENERIC_BLOCK_READER),INTENT(INOUT):: BL   !DATA BLOCK -- MUST POINT TO CURRENT LINE TO PROCESS
    CHARACTER(*),               INTENT(OUT  ):: WELLID
    INTEGER,                    INTENT(OUT  ):: IL,IR,IC
    INTEGER,                    INTENT(IN   ):: NAUX
    INTEGER,      DIMENSION(:), INTENT(INOUT):: AUX
    INTEGER,                    INTENT(IN   ):: INSTYLE  !INSTYLE = 0 for LRC load (default); INSTYLE = 1 for Lay for L and XY for RC; INSTYLE = ADD 10 to use elevation for L (viz 10 and 11), INSTYLE = ADD 20 to use elevation for L (viz 20 and 21)
    INTEGER,          OPTIONAL, INTENT(OUT  ):: NF
    DOUBLE PRECISION, OPTIONAL, INTENT(OUT  ):: Q
    INTEGER,          OPTIONAL, INTENT(OUT  ):: SPSTART,SPEND
    CONTIGUOUS:: AUX
    !
    DOUBLE PRECISION:: ELEV, X, Y, TOL
    INTEGER:: LLOC, ISTART, ISTOP, IERR, I
    CHARACTER(:), ALLOCATABLE:: NAUX_STR
    !
    LLOC=ONE
    CALL PARSE_WORD_UP(BL%LIST%LN,LLOC,ISTART,ISTOP)
    WELLID = BL%LIST%LN(ISTART:ISTOP)
    !
    IF(PRESENT(NF)) CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,NF,MSG='FWEL_PROPERTY_READ ERROR: EXPECTED TO LOAD FOR CURRENT FARM WELL ITS ASSOCIATED FARM ID (FID), BUT FAILD TO CONVERT IT TO AN INTEGER.') 
    !
    TOL = 1D-6
    CALL PARSE_WORD_UP(BL%LIST%LN,LLOC,ISTART,ISTOP)
    !
    IF( BL%LIST%LN(ISTART:ISTOP) =='NAN' .OR. BL%LIST%LN(ISTART:ISTOP) =='MNW') THEN
                                                                   IL = Z  !NaN found so its an MNW2 Link
    ELSE
        IF(INSTYLE > ONE) THEN
                     READ( BL%LIST%LN(ISTART:ISTOP),*,IOSTAT=IERR) ELEV
                     IF(IERR.NE.Z .OR. BL%LIST%LN(ISTART:ISTOP)=='') CALL FILE_IO_ERROR(IERR,BL%IU,LINE=BL%LIST%LN,OUTPUT=BL%IOUT,MSG='FMP FWELL BLOCK FAILED TO CONVERT "ELEVATION" OR "DEPTH" TO A REAL NUMBER.')
                     ELEV = ELEV + TOL*ELEV
                     IF     (INSTYLE == 10 .OR. INSTYLE == 11) THEN
                                                                   IL = -10
                     ELSEIF (INSTYLE == 20 .OR. INSTYLE == 21) THEN
                                                                   IL = -20
                     END IF
        ELSE
                     READ( BL%LIST%LN(ISTART:ISTOP),*,IOSTAT=IERR) IL
                     IF(IERR.NE.Z .OR. BL%LIST%LN(ISTART:ISTOP)=='') CALL FILE_IO_ERROR(IERR,BL%IU,LINE=BL%LIST%LN,OUTPUT=BL%IOUT,MSG='FMP FWELL BLOCK FAILED TO CONVERT "LAYER" TO AN INTEGER.')
        END IF
    END IF
    !
    IF( ANY(INSTYLE == [0, 10, 20]) ) THEN
                       CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,IR,MSG='FWEL_PROPERTY_READ ERROR: EXPECTED TO LOAD FOR CURRENT FARM WELL ITS ROW, BUT FAILD TO CONVERT IT TO AN INTEGER.') 
                       CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,IC,MSG='FWEL_PROPERTY_READ ERROR: EXPECTED TO LOAD FOR CURRENT FARM WELL ITS COLUMN, BUT FAILD TO CONVERT IT TO AN INTEGER.') 
    ELSE
                 CALL GET_NUMBER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,X,MSG='FWEL_PROPERTY_READ ERROR: INPUT INDICATED THAT X-COORDINATE IS READ INSTEAD OF ROW. FAILED TO LOAD X.')
                 !
                 CALL GET_NUMBER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,Y,MSG='FWEL_PROPERTY_READ ERROR: INPUT INDICATED THAT Y-COORDINATE IS READ INSTEAD OF ROW. FAILED TO LOAD Y.')
                 !
                 CALL XYGRID%XY2RC(X,Y,IR,IC) 
    END IF
    !
    IF    ( IL < Z ) THEN
                        IF( IL == -20 ) ELEV = DBLE(BOTM(IC,IR,Z)) - ELEV
                        DO I = ONE, NLAY
                                        IF(ELEV >= BOTM(IC,IR,LBOTM(I)) ) THEN
                                                                       IL=I
                                                                       EXIT
                                        END IF
                        END DO
                        IF(IL < Z) CALL STOP_ERROR(BL%LIST%LN,BL%IU,BL%IOUT,'FMP FWELL BLOCK FAILED TO FIND A LAYER FOR THE SPECIFIED OR CALCULATED "ELEVATION" OF '//NUM2STR(ELEV))
    END IF
    !
    IF(PRESENT(Q))  CALL GET_NUMBER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,Q,MSG='FWEL_PROPERTY_READ ERROR: INPUT EXPECTED TO LOAD FARM WELL CAPACITY (Qcap).')
    !
    IF(PRESENT(SPSTART)) THEN
                       !
                       CALL PARSE_WORD_UP(BL%LIST%LN,LLOC,ISTART,ISTOP)
                       IF( BL%LIST%LN(ISTART:ISTOP)=='1'     .OR. &
                           BL%LIST%LN(ISTART:ISTOP)=='NAN'   .OR. &
                           BL%LIST%LN(ISTART:ISTOP)=='START'     ) THEN
                           SPSTART = ONE
                       ELSEIF(IS_INTEGER(BL%LIST%LN(ISTART:ISTOP))) THEN
                           CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SPSTART,NO_PARSE_WORD=TRUE, MSG='FWEL_PROPERTY_READ ERROR: EXPECTED TO LOAD SPSTART AND DETECTED IT WAS AN INTEGER, BUT FAILED TO LOAD THE INTEGER VALUE.') 
                       ELSE
                           CALL DATE_STR_TO_SP(BL%LIST%LN(ISTART:ISTOP),SPSTART)
                           IF    (SPSTART == -1) THEN
                                  SPSTART = ONE
                                  READ(BL%LIST%LN(ISTART:ISTOP),INTFMT(BL%LIST%LN(ISTART:ISTOP)), IOSTAT=IERR) SPSTART
                                  IF(IERR.NE.Z .OR. BL%LIST%LN(ISTART:ISTOP)=='') CALL FILE_IO_ERROR(IERR,BL%IU,LINE=BL%LIST%LN,OUTPUT=BL%IOUT,MSG='FMP FWELL BLOCK READ FAILED TO IDENTIFY STARTING STRESS PERIOD OR STARTING DATE (MAKE SURE DATES ARE OF THE FORM MM/DD/YYYY)')
                                  !
                           ELSEIF(SPSTART == -2) THEN
                                    CALL STOP_ERROR(BL%LIST%LN,BL%IU,BL%IOUT,'FMP FWELL BLOCK LIST READ SPECIFIED A STARTING DATE BUT THE DIS PACKAGE KEYWORD STARTDATE IS MISSING (OneWater IS NOT DATE AWARE WITHOUT A STARTING DATE).')
                           END IF
                       END IF
                       !
                       CALL PARSE_WORD_UP(BL%LIST%LN,LLOC,ISTART,ISTOP)
                       IF( BL%LIST%LN(ISTART:ISTOP)=='NAN'   .OR. &
                           BL%LIST%LN(ISTART:ISTOP)=='END'   .OR. &
                           BL%LIST%LN(ISTART:ISTOP)=='INF'   .OR. &
                           BL%LIST%LN(ISTART:ISTOP)=='NPER'     ) THEN
                           SPEND = NPER
                       ELSEIF(IS_INTEGER(BL%LIST%LN(ISTART:ISTOP))) THEN
                           CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,SPEND,NO_PARSE_WORD=TRUE, MSG='FWEL_PROPERTY_READ ERROR: EXPECTED TO LOAD SPEND AND DETECTED IT WAS AN INTEGER, BUT FAILED TO LOAD THE INTEGER VALUE.') 
                       ELSE
                           CALL DATE_STR_TO_SP(BL%LIST%LN(ISTART:ISTOP),SPEND)
                           IF    (SPEND == -1) THEN
                                  SPEND =  NPER
                                  READ(BL%LIST%LN(ISTART:ISTOP),INTFMT(BL%LIST%LN(ISTART:ISTOP)), IOSTAT=IERR) SPEND
                                  IF(IERR.NE.Z .OR. BL%LIST%LN(ISTART:ISTOP)=='') CALL FILE_IO_ERROR(IERR,BL%IU,LINE=BL%LIST%LN,OUTPUT=BL%IOUT,MSG='FMP FWELL BLOCK READ FAILED TO IDENTIFY ENDING STRESS PERIOD OR ENDING DATE (MAKE SURE DATES ARE OF THE FORM MM/DD/YYYY)')
                                  !
                           ELSEIF(SPEND == -2) THEN
                                    CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK LIST READ SPECIFIED A ENDING DATE BUT THE DIS PACKAGE KEYWORD STARTDATE IS MISSING (OneWater IS NOT DATE AWARE WITHOUT A STARTING DATE).')
                           END IF
                       END IF
                       !
                       IF( SPSTART > NPER  ) SPEND = SPSTART+ONE  !CASE WHEN DATESTART IS BEYOND SIMULATION, AND DATE END USES NAN
                       IF( SPSTART > SPEND ) CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK LIST READ SPECIFIED A STARTING STRESS PERIOD/DATE THAT IS GREATER THAN THE ENDING DATE.')
    END IF
    !
    IF(NAUX > Z) THEN
               NAUX_STR = NUM2STR(NAUX)
               DO I=ONE, NAUX
                   CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,AUX(I),MSG='FMP FWELL BLOCK LIST READ FAILED FAILED TO LOAD AUXILIARY FLAG (0 or 1). THERE SHOULD BE A TOTAL OF '//NAUX_STR//' AUX VARIABLES READ IN FOR EACH WELL [abc].') 
               END DO
    END IF
    !
  END SUBROUTINE
  !
!  SUBROUTINE GET_FWEL_COUNT(BL,NWEL)
!    !GET COUNT OF FARM WELLS BY FARM. NOTE THAT YOU MUST INITIALIZE NWEL=0 ON ENTRY
!    CLASS(GENERIC_BLOCK_READER),       INTENT(INOUT):: BL    !DATA BLOCK -- MUST POINT TO CURRENT LINE TO PROCESS
!    INTEGER, DIMENSION(:), CONTIGUOUS, INTENT(INOUT):: NWEL  !COUNT OF WELLS PER FARM
!    INTEGER:: POS,I,LLOC,ISTART,ISTOP,ONE,Z,N,NF
!    REAL:: R
!    !  
!    ONE = 1
!    Z = 0
!    !NWEL= Z
!    POS = BL%LIST%GETPOS()
!    !
!    DO I=POS, BL%NLINE
!       LLOC=ONE
!       CALL URWORD(BL%LIST%LN,LLOC,ISTART,ISTOP,Z,N,R,Z,Z)
!       CALL URWORD(BL%LIST%LN,LLOC,ISTART,ISTOP,2,NF,R,BL%IOUT,BL%IU)
!       NWEL(NF) = NWEL(NF) + ONE
!       !
!       CALL BL%LIST%NEXT()
!       CALL BL%LIST%SET_LN()
!       !
!    END DO
!    !
!    CALL BL%LIST%POS(POS)  !MOVE TO START OF INPUT PART OF LIST
!    CALL BL%LIST%SET_LN()
!  END SUBROUTINE
  !
  SUBROUTINE FWEL_LOAD_CAPACITY_LINEFEED(BL,FWEL,INSTYLE)
    !USE READ_FILE_LOCATION_INTERFACE, ONLY: READ_FILE_LOCATION
    USE UTIL_INTERFACE,                                  ONLY: UPPER
    CLASS(GENERIC_BLOCK_READER),               INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(FARM_WELL_DATA),       DIMENSION(:), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER,                                   INTENT(IN   ):: INSTYLE
    CONTIGUOUS:: FWEL
    !TYPE(GENERIC_INPUT_FILE):: FL
    INTEGER,         DIMENSION(:),ALLOCATABLE:: AUX
    INTEGER:: NAUX, NFARM, IU, IL, IR, IC, NF
    INTEGER:: LLOC, ISTART, ISTOP, I, J, N
    CHARACTER(5 ):: ERROR
    CHARACTER(6 ):: NOWELL
    CHARACTER(20):: WELLID
    LOGICAL:: NO_END_ERROR
    !
    WRITE(BL%IOUT,'(A)') 'FARM WELL INITIAL CAPACITIES (Qini) WILL BE READ IN USING LINEFEED FORMAT FOR EACH STRESS PERIOD.'
    FWEL%LOAD_TYPE = 3
    !
    ERROR = 'ERROR'
    NFARM = SIZE(FWEL)
    NAUX = FWEL(ONE)%NAUX
    IF (NAUX > Z) ALLOCATE(AUX(NAUX))
    !
    CALL BL%LIST%NEXT()
    CALL BL%LIST%SET_LN()
    IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !!
    !LLOC=ONE 
    !CALL FL%OPEN(LLOC, BL%LIST%LN, BL%IOUT, BL%IU, NOSTOP=TRUE, REQKEY=TRUE)  !GET LOCATION THAT HOLDS LIST OF FEEDFILE NAMES
    !
    !IF (FL%IU.NE.Z) THEN            !FOUND "EXTERNAL" OR "OPEN/CLOSE".
    !    CALL BL%INNER(' ',FL%IU,BL%IOUT)         !RELOAD LINES --READ TO BOTTOM OF FILE
    !    CALL BL%LIST%START()
    !ELSEIF(FL%IU == Z .AND. .NOT. FL%ERROR) THEN !INTERNAL KEYWORD FOUND, MOVE ONE LINE DOWN
    !    CALL BL%LIST%NEXT()
    !!ELSE                                        !ASSUMES DATA IS ON CURRENT LINE
    !!    FL%IU = Z
    !END IF
    !!
    !CALL BL%LIST%SET_LN()
    !
    !OPEN THE NFARM FEED FILES
    DO I=BL%LIST%GETPOS(), BL%NLINE
        !
        IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
        !
        LLOC=ONE
        CALL GET_INTEGER(BL%LIST%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,NF,MSG='FWEL_LOAD_CAPACITY_LINEFEED ERROR: EXPECTED TO LOAD FARM ID (FID), BUT FAILD TO CONVERT IT TO AN INTEGER.') 
        !
        N = LLOC
        CALL PARSE_WORD(BL%LIST%LN,LLOC,ISTART,ISTOP)  !CHECK FOR NOWELL FLAG
        LLOC = N
        !
        ALLOCATE(FWEL(NF)%FEED)
        !
        NOWELL=BL%LIST%LN(ISTART:ISTOP)                      !CHECK IF KEYWORD NOWELL IS USED
        CALL UPPER(NOWELL)
        IF(NOWELL == 'NOWELL') THEN
            FWEL(NF)%FEED%IU = Z
            FWEL(NF)%N   = Z
            FWEL(NF)%DIM = Z
        ELSE
            CALL FWEL(NF)%FEED%OPEN(BL%LIST%LN, LLOC, BL%IOUT, BL%IU, NOSTOP=TRUE)
            IF(FWEL(NF)%FEED%ERROR) CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK FAILED TO OPEN WBS FEED FILE AFTER "LINEFEED CAPACITY" KEYWORD.'//NEW_LINE('')//'THIS COULD BE BECAUSE EITHER THE UNIT NUMBER COULD NOT BE IDENTIFIED (e.g. "EXTERNAL UNIT")'//NEW_LINE('')//'OR THE PATH IS BAD (e.g. "OPEN/CLOSE FILE")') 
            IF(FWEL(NF)%FEED%IU==Z) CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK DOES NOT ALLOW FOR THE KEYWORD "CONSTANT" OR "INTERNAL" AFTER THE KEYWORD "LINEFEED CAPACITY". LINEFEED INPUT MUST BE SPECIFIED IN A SPEARATE FILE.') 
        END IF
        !
        CALL BL%LIST%NEXT()
        CALL BL%LIST%SET_LN()
    END DO
    DO CONCURRENT ( NF=ONE:NFARM, .NOT. ALLOCATED(FWEL(NF)%FEED) )
                                                                 ALLOCATE(FWEL(NF)%FEED)
                                                                 FWEL(NF)%FEED%IU = Z
                                                                 FWEL(NF)%N   = Z
                                                                 FWEL(NF)%DIM = Z
    END DO
    !
    !INITIALIZE FWELL DIMENSION
    !
    DO NF=1, NFARM
        IU = FWEL(NF)%FEED%IU
        !
        IF(IU .NE. Z) THEN
                          CALL BL%INNER('TEMPORAL',IU,BL%IOUT,END_NOT_FOUND=NO_END_ERROR)  !LOAD LINEFEED DEFINITION LINES
                          !
                          IF(NO_END_ERROR)  CALL STOP_ERROR(INFILE=IU, OUTPUT=BL%IOUT, MSG='FMP FWELL BLOCK "LINEFEED CAPACITY" ERROR WHEN LOADING FROM LINEFEED FILE DESCRIBED ABOVE.'//NL//'THE STATIC/STRUCTAL INPUT MUST BE SEPARATATED FROM THE TRANSIENT/CAPACITY INPUT WITH THE KEYWORD "TEMPROAL".'//NL//'FAILED TO LOCATE THE KEYWORD TEMPORAL AS THE FIRST WORD ON A LINE.')
                          !
                          CALL BL%LIST%START()
                          CALL BL%LIST%SET_LN()
                          IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR READING FIRST LINE OF LINEFEED FILE FOR FARM '//NUM2STR(NF))
                          !
                          N = BL%NLINE
                          CALL INITIALIZE_FMP_WELL_DIMENSION(FWEL(NF), N)
                          !
                          IF(FWEL(NF)%FEED%SCALE.NE.UNO) THEN
                              FWEL(NF)%SFAC = FWEL(NF)%SFAC * FWEL(NF)%FEED%SCALE
                              FWEL(NF)%FEED%SCALE = UNO
                          END IF
                          !
                          DO I=1, N
                              !
                              CALL FWEL_PROPERTY_READ(BL,WELLID,IL,IR,IC,NAUX,AUX,INSTYLE)
                              !
                              FWEL(NF)%WELLID(I)  = WELLID
                              FWEL(NF)%LRC  (1,I) = IL
                              FWEL(NF)%LRC  (2,I) = IR
                              FWEL(NF)%LRC  (3,I) = IC
                              !
                              IF (IL == 0) THEN
                                               FWEL(NF)%MNWLINK          = TRUE
                                               FWEL(NF)%MNWLOC(I) = NEG  !JUST NEED A NONZERO VALUE TO START INDEX SEARCHES
                              ELSE
                                               FWEL(NF)%MNWLOC(I) =  Z   
                              END IF
                              !
                              IF(NAUX > Z) THEN
                                  DO CONCURRENT(J=ONE:NAUX) 
                                                      FWEL(NF)%AUX(I,J) = AUX(J)
                                  END DO
                              END IF
                              !
                              CALL BL%LIST%NEXT()
                              CALL BL%LIST%SET_LN()
                              !IF(  BL%LIST%LN==ERROR .AND. I.NE.N) CALL FILE_IO_ERROR(Z,BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR.  POSSIBLE CRASH HAPPENED WHEN LOADING WELL PROPERTIES IN A FEEDFILE WITH THE PERVIOUS WELL BEING:'//TRIM(WELLID)')
                          END DO
                          !
                          CALL BL%LIST%DESTROY()
        END IF
    END DO
    !
    !DO CONCURRENT ( NF=2:NFARM, FWEL(NF)%DIM==Z )  !DIM=0 MEANS NOTHING GETS LOADED  --BAD CAUSE YOU LOSE WANT NF HAS FOR ITS LOADTYPE
    !                                             FWEL(NF)%LOAD_TYPE = Z
    !END DO
    !
    IF(ALLOCATED(AUX)) DEALLOCATE(AUX)
  END SUBROUTINE
  !
  SUBROUTINE FWEL_LOAD_FID_LINEFEED(BL,FWEL,INSTYLE)
    !USE READ_FILE_LOCATION_INTERFACE, ONLY: READ_FILE_LOCATION
    CLASS(GENERIC_BLOCK_READER),               INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(FARM_WELL_DATA),       DIMENSION(:), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER,                                   INTENT(IN   ):: INSTYLE
    CONTIGUOUS:: FWEL
    !TYPE(GENERIC_INPUT_FILE):: FL
    INTEGER,         DIMENSION(:),ALLOCATABLE::  AUX
    INTEGER:: NAUX, NFARM, IU, IL, IR, IC
    INTEGER:: LLOC, ISTART, ISTOP, I, J, N, NFEED, FD
    DOUBLE PRECISION:: Q, SFAC
    CHARACTER(5  ):: ERROR
    CHARACTER(20 ):: WELLID
    LOGICAL:: NO_END_ERROR, HAS_SFAC
    !LOGICAL, DIMENSION(:), ALLOCATABLE:: LFID
    !
    WRITE(BL%IOUT,'(A)') 'FARM WELL FARM ASSOCIATIONS (FID) WILL BE READ IN USING LINEFEED FORMAT FOR EACH STRESS PERIOD.'
    FWEL%LOAD_TYPE = 2
    !
    ERROR = 'ERROR'
    NFARM = SIZE(FWEL)
    NAUX = FWEL(ONE)%NAUX
    IF (NAUX > Z) ALLOCATE(AUX(NAUX))
    !ALLOCATE( LFID(NFARM), SOURCE = TRUE)
    !
    CALL BL%LIST%NEXT()
    CALL BL%LIST%SET_LN()
    IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !
    !LLOC=ONE 
    !CALL FL%OPEN(LLOC, BL%LIST%LN, BL%IOUT, BL%IU, NOSTOP=TRUE, REQKEY=TRUE)  !GET LOCATION THAT HOLDS LIST OF FEEDFILE NAMES
    !!
    !IF (FL%IU.NE.Z) THEN            !FOUND "EXTERNAL" OR "OPEN/CLOSE".
    !    CALL BL%INNER(' ',FL%IU,BL%IOUT)         !RELOAD LINES --READ TO BOTTOM OF FILE
    !    CALL BL%LIST%START()
    !ELSEIF(FL%IU == Z .AND. .NOT. FL%ERROR) THEN !INTERNAL KEYWORD FOUND, MOVE ONE LINE DOWN
    !    CALL BL%LIST%NEXT()
    !!ELSE                                        !ASSUMES DATA IS ON CURRENT LINE
    !!    FL%IU = Z
    !END IF
    !!
    !CALL BL%LIST%SET_LN()
    !IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR TRYING TO READ IN PART OF BLOCK THAT PERTAINS TO THE ACTUAL FEEDFILE NAMES.')
    !
    !GET FEED FILE COUNT
    NFEED = BL%LIST%LEN() - BL%LIST%GETPOS() + ONE
    ALLOCATE(FWEL(ONE)%FID_FEED(NFEED))
    WRITE(BL%IOUT,'(A)') 'FOUND '//NUM2STR(NFEED)//' FEED FILES.'
    !
    !OPEN THE FEED FILES
    DO I=ONE, NFEED
        !
        IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
        !
        LLOC=ONE
        CALL FWEL(ONE)%FID_FEED(I)%FL%OPEN(BL%LIST%LN, LLOC, BL%IOUT, BL%IU, NOSTOP=TRUE)
        IF (FWEL(ONE)%FID_FEED(I)%FL%ERROR) CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK FAILED TO OPEN WBS FEED FILE AFTER "LINEFEED WBS" KEYWORD.'//NEW_LINE('')//'THIS COULD BE BECAUSE EITHER THE UNIT NUMBER COULD NOT BE IDENTIFIED (e.g. "EXTERNAL UNIT")'//NEW_LINE('')//'OR THE PATH IS BAD (e.g. "OPEN/CLOSE FILE")') 
        IF (FWEL(ONE)%FID_FEED(I)%FL%IU==Z) CALL STOP_ERROR(BL%LIST%LN, BL%IU, BL%IOUT, 'FMP FWELL BLOCK DOES NOT ALLOW FOR THE KEYWORD "CONSTANT" OR "INTERNAL" AFTER THE KEYWORD "LINEFEED WBS". LINEFEED INPUT MUST BE SPECIFIED IN A SPEARATE FILE.') 
        !
        CALL BL%LIST%NEXT()
        CALL BL%LIST%SET_LN()
    END DO
    !
    !INITIALIZE FWELL DIMENSION
    !
    FWEL(ONE)%DIM = Z
    DO FD=1, NFEED
        IU = FWEL(ONE)%FID_FEED(FD)%FL%IU
        !
        IF(IU .NE. Z) THEN
                          CALL BL%INNER('TEMPORAL',IU,BL%IOUT,END_NOT_FOUND=NO_END_ERROR)  !LOAD LINEFEED DEFINITION LINES
                          !
                          IF(NO_END_ERROR)  CALL STOP_ERROR(INFILE=IU, OUTPUT=BL%IOUT, MSG='FMP FWELL BLOCK "LINEFEED WBS" OR "LINEFEED FARM" ERROR WHEN LOADING FROM LINEFEED FILE DESCRIBED ABOVE.'//NL//'THE STATIC/STRUCTAL INPUT MUST BE SEPARATATED FROM THE TRANSIENT/CAPACITY INPUT WITH THE KEYWORD "TEMPROAL".'//NL//'FAILED TO LOCATE THE KEYWORD TEMPORAL AS THE FIRST WORD ON A LINE.')
                          !
                          !
                          CALL BL%LIST%START()
                          CALL BL%LIST%SET_LN()
                          IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR READING THE '//NUM2STR(FD)//' LINEFEED FILE')
                          !
                          N = BL%NLINE
                          FWEL(ONE)%DIM = FWEL(ONE)%DIM + N
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%FID(N))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%RVAL(N))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%OLD_FID(N), SOURCE=Z)
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%WELLID(N))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%Qini(N))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%AUX(N, NAUX))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%LRC(3,N))
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%MNWLOC(N), SOURCE=Z)
                          ALLOCATE(FWEL(ONE)%FID_FEED(FD)%MASTER_LOC(N), SOURCE=Z)
                          !
                          HAS_SFAC = FWEL(ONE)%FID_FEED(FD)%FL%SCALE.NE.UNO
                          IF(HAS_SFAC) THEN
                                 SFAC = FWEL(ONE)%FID_FEED(FD)%FL%SCALE
                                 FWEL(ONE)%FID_FEED(FD)%FL%SCALE = UNO
                          ELSE
                                 SFAC = UNO
                                 FWEL(ONE)%FID_FEED(FD)%FL%SCALE = UNO
                          END IF
                          !
                          DO I=ONE, N
                              !
                              CALL FWEL_PROPERTY_READ(BL,WELLID,IL,IR,IC,NAUX,AUX,INSTYLE, Q=Q)
                              !
                              IF(HAS_SFAC) Q = Q * SFAC
                              !
                              FWEL(ONE)%FID_FEED(FD)%WELLID(I)  = WELLID
                              FWEL(ONE)%FID_FEED(FD)%LRC  (1,I) = IL
                              FWEL(ONE)%FID_FEED(FD)%LRC  (2,I) = IR
                              FWEL(ONE)%FID_FEED(FD)%LRC  (3,I) = IC
                              FWEL(ONE)%FID_FEED(FD)%Qini(I)    = Q
                              !
                              IF (IL == Z) THEN
                                  FWEL(ONE)%FID_FEED%MNWLINK  = TRUE
                                  FWEL(ONE)%FID_FEED(FD)%MNWLOC(I)=-1
                              ELSE
                                  FWEL(ONE)%FID_FEED(FD)%MNWLOC(I) = Z
                              END IF
                              !
                              IF(NAUX > Z) THEN
                                  DO CONCURRENT(J=ONE:NAUX)  
                                                      FWEL(ONE)%FID_FEED(FD)%AUX(I,J) = AUX(J)
                                  END DO
                              END IF
                              !
                              CALL BL%LIST%NEXT()
                              CALL BL%LIST%SET_LN()
                              !IF(  BL%LIST%LN==ERROR .AND. I.NE.N) CALL FILE_IO_ERROR(Z,BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR.  POSSIBLE CRASH HAPPENED WHEN LOADING WELL PROPERTIES IN A FEEDFILE WITH THE PERVIOUS WELL BEING:'//TRIM(WELLID)')
                          END DO
                          !
                          CALL BL%LIST%DESTROY()
        END IF
    END DO
    !
    FWEL(ONE)%DIM = NEG*FWEL(ONE)%DIM   !ONLY NEEDED TO GET INITAL WELL COUNT. MADE NEGATIVE SO DEALLOCATE ROUTINE DOES NOT FREAK OUT
    !
    IF(ALLOCATED(AUX)) DEALLOCATE(AUX)
  END SUBROUTINE
  !
  SUBROUTINE FWEL_LOAD_LIST(BL,FWEL,INSTYLE)
    !USE READ_FILE_LOCATION_INTERFACE, ONLY: READ_FILE_LOCATION
    CLASS(GENERIC_BLOCK_READER),               INTENT(INOUT):: BL   !DATA BLOCK
    CLASS(FARM_WELL_DATA),       DIMENSION(:), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER,                                   INTENT(IN   ):: INSTYLE
    CONTIGUOUS:: FWEL
    TYPE(GENERIC_INPUT_FILE):: CHK_FL
    TYPE(GENERIC_INPUT_FILE), DIMENSION(:),ALLOCATABLE:: FL
    !TYPE(READ_FILE_LOCATION):: FLOC
    INTEGER,DIMENSION(:),ALLOCATABLE:: NWEL, AUX
    LOGICAL,DIMENSION(:),ALLOCATABLE:: EMPTY_FILE
    INTEGER:: NAUX,I,J,K, NFILE
    INTEGER:: LLOC, NF, IL,IR,IC, SPSTART, SPEND,ISTART,ISTOP
    LOGICAL:: EOF, HAS_SFAC
    CHARACTER(20):: WELLID
    CHARACTER(6 ):: ERROR
    DOUBLE PRECISION:: Q
    CHARACTER(:), ALLOCATABLE:: ERR
    !
    WRITE(BL%IOUT,'(A)') 'FARM WELLS WILL BE APPLIED BY SPECIFIED TIME RANGES.'
    FWEL%LOAD_TYPE = ONE
    !
    ERROR='ERROR'
    NAUX = FWEL(ONE)%NAUX
    IF (NAUX > Z) ALLOCATE(AUX(NAUX))
    !
    CALL BL%LIST%NEXT()
    CALL BL%LIST%SET_LN()
    IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
    !
    LLOC = ONE
    CALL CHK_FL%OPEN(BL%LIST%LN,LLOC,BL%IOUT,BL%IU,REQKEY=TRUE, NO_CONSTANT=TRUE) 
    !
    IF (CHK_FL%IU.NE.Z) THEN
        NFILE = BL%NLINE - BL%LIST%GETPOS() + ONE
        CALL BL%LIST%NEXT()
    ELSE
        NFILE = ONE
        CALL BL%LIST%NEXT()  
        CALL BL%MAKE_SCRATCH_FILE(TRUE)  !WRITE REMAINING PORTION TO SCRATCH FILE
        !
        Q =CHK_FL%SCALE
        !
        CALL CHK_FL%CLOSE()
        CHK_FL%IU = BL%SCRATCH
        !
        CHK_FL%SCALE = Q
    END IF
    !
    ALLOCATE(FL(NFILE))
    CALL CHK_FL%MOVE(FL(ONE))
    !
    I=ONE
    DO J=BL%LIST%GETPOS(), BL%NLINE
        !
        CALL BL%LIST%SET_LN()
        IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR READING FIRST LINE OF LIST LOAD.')
        !
        I = I + ONE
        LLOC  = ONE
        CALL FL(I)%OPEN(BL%LIST%LN,LLOC,BL%IOUT,BL%IU,REQKEY=TRUE, NO_CONSTANT=TRUE)
        CALL BL%LIST%NEXT()
    END DO
    !
    ALLOCATE(NWEL(SIZE(FWEL)), SOURCE=Z)
    ALLOCATE(EMPTY_FILE(NFILE), SOURCE=TRUE)
    
    CALL FL%REWIND()  !REWINDS ALL FILES OPENED ON FL
    DO I=ONE, NFILE
       DO
         CALL BL%READ_AND_SET_LINE(FL(I)%IU,EOF=EOF)  !THIS USES THE ALREADY ALLOCATED BL%LN TO LOAD THE LINE
         IF(EOF) EXIT
         !
         EMPTY_FILE(I) = FALSE
         !
         LLOC  = ONE
         CALL PARSE_WORD(BL%LN,LLOC,ISTART,ISTOP)  !BYPASS WELLNAME
         CALL GET_INTEGER(BL%LN,LLOC,ISTART,ISTOP,BL%IOUT,BL%IU,NF,MSG='FWEL_LOAD_LIST ERROR: EXPECTED TO LOAD FARM ID (FID) AFTER FARM WELL NAME, BUT FAILD TO CONVERT IT TO AN INTEGER.') 
         !
         NWEL(NF) = NWEL(NF) + ONE
         !
       END DO
    END DO
    !
    CALL INITIALIZE_FMP_WELL_DIMENSION( FWEL, NWEL )   !SETS ALL DIMENSIONS  NOTE THAT FWEL(NF) and NWEL(NF) ARE INLINE WITH EACH OTHER
    !
    NWEL = Z
    CALL FL%REWIND()
    !
    DO I=ONE, NFILE
       IF(EMPTY_FILE(I)) CYCLE
       !
       CALL BL%INNER(' ',FL(I)%IU,BL%IOUT)  !RELOAD LINES  --READ ENTIRE FILE
       CALL BL%LIST%START()
       CALL BL%LIST%SET_LN()
       IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR READING FIRST LINE OF LIST LOAD.')
       !
       HAS_SFAC = FL(I)%SCALE.NE.UNO
       !
       DO J=ONE, BL%NLINE !BL%LIST%GETPOS()
           !
           CALL FWEL_PROPERTY_READ(BL,WELLID,IL,IR,IC,NAUX,AUX,INSTYLE,NF,Q,SPSTART,SPEND)
           !
           IF(HAS_SFAC) Q = Q * FL(I)%SCALE
           !
           NWEL(NF) = NWEL(NF) + ONE
           !
           FWEL(NF)%WELLID (NWEL(NF)) = WELLID
           FWEL(NF)%Qini   (NWEL(NF)) = Q
           FWEL(NF)%LRC  (1,NWEL(NF)) = IL
           FWEL(NF)%LRC  (2,NWEL(NF)) = IR
           FWEL(NF)%LRC  (3,NWEL(NF)) = IC
           !
           FWEL(NF)%SP_START(NWEL(NF)) = SPSTART
           FWEL(NF)%SP_END  (NWEL(NF)) = SPEND
           !
           IF (IL == Z) THEN
                            FWEL(NF)%MNWLINK          = TRUE
                            FWEL(NF)%MNWLOC(NWEL(NF)) = -1  !JUST NEED A NONZERO VALUE TO START INDEX SEARCHES
           ELSE
                            FWEL(NF)%MNWLOC(NWEL(NF)) =  Z   
           END IF
           !
           IF(NAUX > Z) THEN
               DO CONCURRENT(K=ONE:NAUX) 
                           FWEL(NF)%AUX(NWEL(NF),K) = AUX(K)
               END DO
           END IF
           !
           CALL BL%LIST%NEXT()
           CALL BL%LIST%SET_LN()
           !
       END DO
    END DO
    !
    ERR = BLNK
    DO CONCURRENT (NF=ONE:SIZE(FWEL))
    DO CONCURRENT (I=ONE:FWEL(NF)%DIM, FWEL(NF)%SP_END(I) < FWEL(NF)%SP_START(I) )  !DIM=0 MEANS NOTHING GETS LOADED  < 
                                                        ERR = ERR//FWEL(NF)%WELLID(I)//'    '//NUM2STR(FWEL(NF)%SP_START(I))//'    '//NUM2STR(FWEL(NF)%SP_END(I))//NL
    END DO; END DO
    IF (ERR .NE. '') CALL STOP_ERROR(OUTPUT=BL%IOUT, MSG='FMP FWELL CONTAINED WELLS WITH ENDING DATES/STRESS PERIODS THAT ARE BEFORE THE STARTING DATE/STRESS PERIOD. THE FOLLOWING WELL NAMES ARE THE WELLS THAT HAD THIS PROBLEM WITH THEIR STARTING AND ENDING STRESS PERIODS.'//NL//ERR//NL)
    !
    CALL BL%LIST%DESTROY()
    !
    !DO CONCURRENT ( NF=ONE:SIZE(FWEL), FWEL(NF)%DIM==Z )  !DIM=0 MEANS NOTHING GETS LOADED
    !                                                    FWEL(NF)%LOAD_TYPE = Z
    !END DO
    !
    IF(ALLOCATED(NWEL)) DEALLOCATE(NWEL)
    IF(ALLOCATED(AUX) ) DEALLOCATE(AUX)
    IF(ALLOCATED(FL)  ) DEALLOCATE(FL)
    !
  END SUBROUTINE
  !
!  SUBROUTINE FWEL_LOAD_LIST(BL,FWEL,INSTYLE)
!    USE READ_FILE_LOCATION_INTERFACE, ONLY: READ_FILE_LOCATION
!    CLASS(GENERIC_BLOCK_READER),               INTENT(INOUT):: BL   !DATA BLOCK
!    CLASS(FARM_WELL_DATA),       DIMENSION(:), INTENT(INOUT):: FWEL !FARMWELL
!    INTEGER,                                   INTENT(IN   ):: INSTYLE
!    CONTIGUOUS:: FWEL
!    TYPE(READ_FILE_LOCATION):: FLOC
!    INTEGER,DIMENSION(:),ALLOCATABLE:: NWEL, AUX
!    INTEGER:: NAUX,I,J,Z,ONE
!    INTEGER:: NF, IL,IR,IC, SPSTART, SPEND
!    !LOGICAL:: OPENCLOSE
!    CHARACTER(20):: WELLID
!    CHARACTER(6 ):: ERROR
!    DOUBLE PRECISION:: Q
!    !
!    WRITE(BL%IOUT,'(A)') 'FARM WELLS WILL BE APPLIED BY SPECIFIED TIME RANGES.'
!    FWEL%LOAD_TYPE = 1
!    !
!    Z=0
!    ONE=1
!    ERROR='ERROR'
!    NAUX = FWEL(ONE)%NAUX
!    IF (NAUX > Z) ALLOCATE(AUX(NAUX))
!    !
!    ALLOCATE(NWEL(SIZE(FWEL)))
!    !
!    CALL BL%LIST%NEXT()
!    CALL BL%LIST%SET_LN()
!    IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT,MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. PLEASE DOUBLE CHECK BLOCK SET UP.')
!    !
!    CALL FLOC%GET_UNIT(BL%LIST%LN,BL%IU,BL%IOUT)
!    !
!    IF (FLOC%IU.NE.Z) THEN
!        CALL BL%INNER(' ',FLOC%IU,BL%IOUT)  !RELOAD LINES  --READ ENTIRE FILE
!        CALL BL%LIST%START()
!    ELSE
!        CALL BL%LIST%NEXT()
!    END IF
!    CALL BL%LIST%SET_LN()
!    IF(  BL%LIST%LN==ERROR ) CALL STOP_ERROR(INFILE=BL%IU, OUTPUT=BL%IOUT, MSG='UNFORTUNATELY UNKNOWN FMP FWELL BLOCK ERROR. POSSIBLE ERROR READING FIRST LINE OF LIST LOAD.')
!    !
!    ! FIRST GET FARM COUNTS
!    !
!    CALL GET_FWEL_COUNT(BL,NWEL)        !GET WELL COUNTS BY FARM
!    !
!    CALL INITIALIZE_FMP_WELL_DIMENSION( FWEL, NWEL )   !SETS ALL DIMENSIONS  NOTE THAT FWEL(NF) and NWEL(NF) ARE INLINE WITH EACH OTHER
!    !
!    NWEL = Z
!    !
!    DO I=BL%LIST%GETPOS(), BL%NLINE
!        !
!       CALL FWEL_PROPERTY_READ(BL,WELLID,IL,IR,IC,NAUX,AUX,INSTYLE,NF,Q,SPSTART,SPEND)
!       !
!       NWEL(NF) = NWEL(NF) + ONE
!       !
!       FWEL(NF)%WELLID (NWEL(NF)) = WELLID
!       FWEL(NF)%Qini   (NWEL(NF)) = Q
!       FWEL(NF)%LRC  (1,NWEL(NF)) = IL
!       FWEL(NF)%LRC  (2,NWEL(NF)) = IR
!       FWEL(NF)%LRC  (3,NWEL(NF)) = IC
!       !
!       FWEL(NF)%SP_START(NWEL(NF)) = SPSTART
!       FWEL(NF)%SP_END  (NWEL(NF)) = SPEND
!       !
!       IF (IL == 0) THEN
!                        FWEL(NF)%MNWLINK          = TRUE
!                        FWEL(NF)%MNWLOC(NWEL(NF)) = -1  !JUST NEED A NONZERO VALUE TO START INDEX SEARCHES
!       ELSE
!                        FWEL(NF)%MNWLOC(NWEL(NF)) =  Z   
!       END IF
!       !
!       IF(NAUX > 0) FORALL(J=ONE:NAUX) FWEL(NF)%AUX(NWEL(NF),J) = AUX(J)
!       !
!       CALL BL%LIST%NEXT()
!       CALL BL%LIST%SET_LN()
!       !
!    END DO
!    !
!    CALL BL%LIST%DESTROY()
!    !
!    DO CONCURRENT ( NF=ONE:SIZE(FWEL), FWEL(NF)%DIM==Z )  !DIM=0 MEANS NOTHING GETS LOADED
!                                                        FWEL(NF)%LOAD_TYPE = Z
!    END DO
!    !
!    IF(ALLOCATED(AUX)) DEALLOCATE(AUX)
!    !
!  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FWEL_FID_LINEFEED_LOAD(FWEL, SP)
    USE GLOBAL, ONLY: IUNIT
    USE GWFMNW2MODULE, ONLY: WELLID, MNWMAX
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER,               INTENT(IN   ):: SP
    !LOGICAL,               INTENT(IN   ):: LFID
    CHARACTER(:), ALLOCATABLE:: ERR
    CHARACTER(120):: LINE
    INTEGER:: I, J, K, IU, FD, DIM, NFEED, IERR
    LOGICAL:: NOT_FOUND
    !
    !IF (FWEL%LOAD_TYPE .NE. 2 ) RETURN
    !
    NFEED = SIZE(FWEL%FID_FEED)
    !
    IF(SP == ONE .AND. FWEL%NF == ONE) THEN  !FIRST ENTRY REQUIRES BUILDING MNW2 INDEX
             !
             IF(ANY(FWEL%FID_FEED%MNWLINK) .AND. IUNIT(50) == Z) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL HAS WELLS WITH LAY=0, WHICH INDICATES AN MNW2-LINK, BUT THE MNW2 PACKAGE IS NOT AVAILIBLE.')
             !
             ERR = BLNK
             DO CONCURRENT ( FD=ONE:NFEED, FWEL%FID_FEED(FD)%MNWLINK )
                !
                DO CONCURRENT ( I=ONE:SIZE(FWEL%FID_FEED(FD)%FID), FWEL%FID_FEED(FD)%MNWLOC(I) .NE. Z )
                      !
                      NOT_FOUND = TRUE
                      DO J=1, MNWMAX
                          IF( WELLID(J) == FWEL%FID_FEED(FD)%WELLID(I) ) THEN
                              FWEL%FID_FEED(FD)%MNWLOC(I) = J
                              NOT_FOUND = FALSE
                              !EXIT  --ALLOW FOR POTENTIAL DUBLICATE LINKAGES TO MNW2 WELLS
                          END IF
                      END DO
                      IF (NOT_FOUND) ERR = TRIM(ERR//FWEL%FID_FEED(FD)%WELLID(I))//NL
                END DO
             END DO
             IF (ERR .NE. BLNK) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL WITH MNW2-LINK FAILED TO LOCATE THE FOLLOWING WELL NAMES IN THE MNW2 WELLID LIST:'//NL//'(NO NAME MATCH BETWEEN THESE FMP FWELLs AND THE LIST OF MNW2 WELLs)'//NL//ERR//NL)
    END IF
    !
    IF(FWEL%NF == ONE) THEN
                         DO FD=ONE, NFEED
                             IU = FWEL%FID_FEED(FD)%FL%IU
                             !
                             IF(IU .NE. Z) THEN
                                CALL READ_TO_DATA(LINE, IU)  !MOVE TO DATE
                                BACKSPACE(IU)
                                !
                                READ(IU,*,IOSTAT=IERR) FWEL%FID_FEED(FD)%RVAL
                                IF(IERR.NE.Z) CALL FILE_IO_ERROR( IERR, FWEL%FEED%IU, LINE=LINE, OUTPUT=FWEL%IOUT, MSG= "FMP FWELL LINEFEED ERROR WHILE READING CURRENT STRESS PERIOD'S FEED")
                                !
                                DO CONCURRENT(I=ONE:SIZE(FWEL%FID_FEED(FD)%FID))
                                    IF(FWEL%FID_FEED(FD)%RVAL(I).NE.FWEL%FID_FEED(FD)%RVAL(I))THEN
                                        FWEL%FID_FEED(FD)%FID(I) = Z
                                    ELSE
                                        FWEL%FID_FEED(FD)%FID(I) = INT(FWEL%FID_FEED(FD)%RVAL(I))
                                    END IF
                                END DO
                             END IF
                         END DO
                         !
                         IF( SP > ONE) THEN
                            FWEL%FID_FEED(ONE)%NEW_FID = FALSE
                            DO CONCURRENT (FD=ONE:NFEED, .NOT. ALL(FWEL%FID_FEED(FD)%FID == FWEL%FID_FEED(FD)%OLD_FID) )
                                  FWEL%FID_FEED(ONE)%NEW_FID = TRUE
                            END DO
                         ELSE
                             FWEL%FID_FEED(ONE)%NEW_FID = TRUE
                         END IF
                         !
                         IF(FWEL%FID_FEED(ONE)%NEW_FID) THEN
                             DO CONCURRENT(FD=ONE:NFEED) 
                                                  FWEL%FID_FEED(FD)%OLD_FID = FWEL%FID_FEED(FD)%FID
                             END DO
                         END IF
    END IF
    !
    IF( FWEL%FID_FEED(ONE)%NEW_FID ) THEN
       DIM=Z
       DO CONCURRENT (FD=ONE:NFEED)
            DIM = DIM + COUNT(FWEL%FID_FEED(FD)%FID == FWEL%NF)
       END DO
       !
       CALL INITIALIZE_FMP_WELL_DIMENSION(FWEL, DIM)
       !
       FWEL%MNWLINK  = FALSE
       !IF( DIM > Z) FWEL%ACT = LFID
       !
       IF(DIM > Z) THEN  !LFID .AND. 
             K = Z
             DO FD=ONE, NFEED
                   DO  I=ONE, SIZE(FWEL%FID_FEED(FD)%FID)
                             IF( FWEL%FID_FEED(FD)%FID(I) == FWEL%NF ) THEN
                                  !
                                  K=K+ONE
                                  FWEL%WELLID(K) = FWEL%FID_FEED(FD)%WELLID(I)
                                  FWEL%LRC(:, K) = FWEL%FID_FEED(FD)%LRC (:,I)
                                  FWEL%Qini(  K) = FWEL%FID_FEED(FD)%Qini(  I)
                                  FWEL%MNWLOC(K) = FWEL%FID_FEED(FD)%MNWLOC(I)  
                                  FWEL%MASTER_LOC(K) = FWEL%FID_FEED(FD)%MASTER_LOC(I)
                                  !
                                  IF(FWEL%NAUX > Z) THEN
                                      DO CONCURRENT(J=ONE:FWEL%NAUX)  
                                                          FWEL%AUX(K,J) = FWEL%FID_FEED(FD)%AUX(I,J)
                                      END DO
                                  END IF
                             END IF
                   END DO
             END DO
             !
             IF ( ANY(FWEL%MNWLOC > Z) ) FWEL%MNWLINK  = TRUE
             !
       END IF
    !ELSE
    !    IF(FWEL%DIM > Z) FWEL%ACT = LFID
    END IF
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FWELL_BUILD_MNW2_INDEX(FWEL,HAS_MNW2)
    USE GWFMNW2MODULE, ONLY: WELLID, MNWMAX, MNW_FMP_LINK
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    LOGICAL,INTENT(IN):: HAS_MNW2
    INTEGER:: I, J
    LOGICAL::  NOT_FOUND
    CHARACTER(:),ALLOCATABLE:: ERR
    INTEGER:: NF,FD
    !
    IF(FWEL%NF==ONE) THEN; IF(HAS_MNW2) MNW_FMP_LINK = FALSE
    END IF
    !
    IF(FWEL%LOAD_TYPE == TWO .AND. FWEL%NF==ONE ) THEN
             DO FD=ONE, SIZE(FWEL%FID_FEED)
                 IF(FWEL%FID_FEED(FD)%MNWLINK .AND. .NOT. HAS_MNW2) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL HAS WELLS WITH LAY=0 OR "MNW", WHICH INDICATES AN MNW2-LINK, BUT THE MNW2 PACKAGE IS NOT AVAILIBLE.')
             END DO
             !
             IF(HAS_MNW2) THEN
                DO CONCURRENT ( FD=ONE:SIZE(FWEL%FID_FEED), FWEL%FID_FEED(FD)%MNWLINK )
                    !
                    DO CONCURRENT ( I=ONE:SIZE(FWEL%FID_FEED(FD)%FID), FWEL%FID_FEED(FD)%MNWLOC(I) .NE. Z )
                          !
                          DO J=1, MNWMAX
                              IF( WELLID(J) == FWEL%FID_FEED(FD)%WELLID(I) ) THEN
                                  MNW_FMP_LINK(J) = TRUE
                                  EXIT
                              END IF
                          END DO
                    END DO
                END DO
             END IF
    ELSEIF( FWEL%MNWLINK .AND. FWEL%LOAD_TYPE.NE.TWO .AND. FWEL%DIM > Z) THEN
             !
             IF(.NOT. HAS_MNW2) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL HAS WELLS WITH LAY=0, WHICH INDICATES AN MNW2-LINK, BUT THE MNW2 PACKAGE IS NOT AVAILIBLE.')
             !
             NF = FWEL%NF
             ERR=BLNK
             DO I=1, FWEL%DIM
                 NOT_FOUND = TRUE
                 IF(FWEL%MNWLOC(I) .NE. Z) THEN
                     DO J=1, MNWMAX
                         IF( WELLID(J) == FWEL%WELLID(I) ) THEN
                             MNW_FMP_LINK(J) = TRUE
                             FWEL%MNWLOC (I)  = J
                             NOT_FOUND = FALSE
                             EXIT
                         END IF
                     END DO
                     IF (NOT_FOUND) ERR = TRIM(ERR//FWEL%WELLID(I))//NL
                 END IF
             END DO
             !
             IF (ERR .NE. BLNK) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL WITH MNW2-LINK FAILED TO LOCATE THE FOLLOWING WELL NAMES IN THE MNW2 WELLID LIST:'//NL//'(NO NAME MATCH BETWEEN THESE FMP FWELLs AND THE LIST OF MNW2 WELLs)'//NL//ERR//NL)
    END IF
    !
  END SUBROUTINE
  !
!  IMPURE ELEMENTAL SUBROUTINE FWELL_BUILD_MNW2_INDEX(FWEL,IU_MNW2)
!    USE GWFMNW2MODULE, ONLY: WELLID, MNWMAX, MNW_FMP_LINK
!    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
!    INTEGER,INTENT(IN):: IU_MNW2
!    INTEGER:: I, J
!    LOGICAL::  NOT_FOUND
!    CHARACTER(:),ALLOCATABLE:: ERR
!    INTEGER:: NF,FD
!    !
!    IF(FWEL%NF==ONE) THEN
!        MNW_FMP_LINK = FALSE
!        !
!        IF(FWEL%LOAD_TYPE == TWO) THEN
!                 DO CONCURRENT ( FD=ONE:SIZE(FWEL%FID_FEED), FWEL%FID_FEED(FD)%MNWLINK )
!                    !
!                    DO CONCURRENT ( I=ONE:SIZE(FWEL%FID_FEED(FD)%FID), FWEL%FID_FEED(FD)%MNWLOC(I) .NE. Z )
!                          !
!                          NOT_FOUND = TRUE
!                          DO J=1, MNWMAX
!                              IF( WELLID(J) == FWEL%FID_FEED(FD)%WELLID(I) ) THEN
!                                  FWEL%FID_FEED(FD)%MNWLOC(I) = J
!                                  NOT_FOUND = FALSE
!                                  !EXIT  --ALLOW FOR POTENTIAL DUBLICATE LINKAGES TO MNW2 WELLS
!                              END IF
!                          END DO
!                          IF (NOT_FOUND) ERR = TRIM(ERR//FWEL%FID_FEED(FD)%WELLID(I))//NL
!                    END DO
!                 END DO
!        END IF
!    END IF
!    !
!    IF( FWEL%LOAD_TYPE == TWO .OR. .NOT. FWEL%MNWLINK .OR. FWEL%DIM == Z) RETURN
!    !
!    IF(FWEL%MNWLINK .AND. IU_MNW2 == Z) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL HAS WELLS WITH LAY=0, WHICH INDICATES AN MNW2-LINK, BUT THE MNW2 PACKAGE IS NOT AVAILIBLE.')
!    !
!    NF = FWEL%NF
!    ERR=BLNK
!    DO I=1, FWEL%DIM
!        NOT_FOUND = TRUE
!        IF(FWEL%MNWLOC(I) .NE. Z) THEN
!            DO J=1, MNWMAX
!                IF( WELLID(J) == FWEL%WELLID(I) ) THEN
!                    MNW_FMP_LINK(J) = TRUE
!                    FWEL%MNWLOC(I) = J
!                    NOT_FOUND = FALSE
!                END IF
!            END DO
!            IF (NOT_FOUND) ERR = TRIM(ERR//FWEL%WELLID(I))//NL
!        END IF
!    END DO
!    !
!    IF (ERR .NE. BLNK) CALL STOP_ERROR(OUTPUT=FWEL%IOUT, MSG='FMP FWELL WITH MNW2-LINK FAILED TO LOCATE THE FOLLOWING WELL NAMES IN THE MNW2 WELLID LIST:'//NL//'(NO NAME MATCH BETWEEN THESE FMP FWELLs AND THE LIST OF MNW2 WELLs)'//NL//ERR//NL)
!    !
!  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FWELL_MNW2_SP_INIT(FWEL)
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD, NMNW2, CapTable, MNWPRNT
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER:: IOUT
    INTEGER:: I, J, K 
    INTEGER:: PUMPCAP, FIRSTNODE, LASTNODE!,IL,IR,IC
    LOGICAL:: RECAL_QMAX, MASTER_ACT
    INTEGER:: IACT, NF

    !IF(FWEL%NF==ONE) WRITE(*,'(I6/,A)') FWEL%MSG%N,FWEL%MSG%STR
    !
    IF(FWEL%MNWLINK .AND. FWEL%DIM > Z) THEN
        !
        NF = FWEL%NF
        IOUT = FWEL%IOUT
        RECAL_QMAX = FALSE
        WELL: DO I=1, FWEL%DIM
          IF(FWEL%MNWLOC(I) > Z) THEN
                J=FWEL%MNWLOC(I)
                IACT = NINT(MNW2(1,J))
                MASTER_ACT = FWEL%MASTER_ACT( FWEL%MASTER_LOC(I) )  !SET TO TRUE IF ANY WELLID HAS ACT OR EXT SET TO TRUE
                !
                IF (IACT == NEG) THEN
                        IF(FWEL%ACT(I)) FWEL%N = FWEL%N - ONE
                        !
                        FWEL%ACT(I) = FALSE
                        FWEL%EXT(I) = FALSE
                        RECAL_QMAX = TRUE
                        CALL FWEL%WRN2%ADD(NUM2STR(FWEL%NF,-9)//BLNK//TRIM(FWEL%WELLID(I))//NL)
                        CYCLE WELL
                END IF
                !
                IF (IACT == Z .AND. .NOT. MASTER_ACT) CYCLE WELL
                !   
                IF (IACT == Z .AND. MASTER_ACT) THEN
                    !
                    IF(FWEL%MNW_ON) THEN
                        CALL FWEL%WRN%ADD(BLNK//NUM2STR(NF,-8)//BLNK//FWEL%WELLID(I)//' ON'//NL)
                        !
                        MNW2(ONE,J) = UNO  !TURN ON MNW2 WELL
                        NMNW2=NMNW2+ONE    !FLAG FOR MNW2 TO USE FM ROUTINE
                    ELSE
                        IF(FWEL%ACT(I)) FWEL%N = FWEL%N - ONE
                        !
                        FWEL%ACT(I) = FALSE
                        FWEL%EXT(I) = FALSE
                        RECAL_QMAX = TRUE
                        CALL FWEL%WRN2%ADD(NUM2STR(FWEL%NF,-9)//BLNK//TRIM(FWEL%WELLID(I))//NL)
                        CYCLE WELL
                    END IF
                    !
                ELSEIF ( IACT == ONE .AND. .NOT. MASTER_ACT) THEN
                    IF(FWEL%MNW_OFF) THEN
                        CALL FWEL%WRN%ADD(BLNK//NUM2STR(NF,-8)//BLNK//FWEL%WELLID(I)//' OFF'//NL)
                        !
                        MNW2(ONE,J) = DZ   !TURN OFF MNW2 WELL
                        NMNW2=NMNW2-ONE    !FLAG FOR MNW2 TO USE FM ROUTINE
                        IF(NMNW2<Z) NMNW2=Z
                    ELSE
                        CALL FWEL%WRN3%ADD(NUM2STR(FWEL%NF,-9)//BLNK//TRIM(FWEL%WELLID(I))//NL)
                    END IF
                    !WRITE(IOUT,'(3A)') 'WARNING: FMP FWELL "',FWEL%WELLID(I),'" WITH MNW-LINK IS NOT SPECIFIED FOR USE DURING THIS STRESS PERIOD, BUT THE MNW2 WELL OF THE SAME NAME IS AVAILIBLE. THE MNW2 WELL WILL HAVE ITS DESIRED PUMPING RATE SET TO ZERO FOR THE STRESS PERIOD (NO PUMPING BUT INTRABOREHOLE FLOW).'
                END IF
                !
                IF(NMNW2 < ONE .AND. MASTER_ACT) NMNW2=ONE    !FLAG FOR MNW2 TO USE FM/BD ROUTINE
                !
                FIRSTNODE=NINT( MNW2(4,J) )
                LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                !
                MNW2(5 ,J) = DZ                                                ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
                MNW2(18,J) = DZ 
                MNWNOD(4,FIRSTNODE:LASTNODE)=DZ
                !
                PUMPCAP=NINT(MNW2(22,J))
                IF(PUMPCAP.GT.Z) THEN
                     IF( FWEL%Qcap(I) < MINSIZE ) THEN                           !IF QDES IS 0D0 FOR THIS STRESS PERIOD, DO NOT APPLY PUMP CAPACITY RESTRAINTS (Should )
                          MNW2(25,J)=Z
                          MNW2(27,J)=Z                                        !INITIALIZE CAPFLAG
                     ELSE
                          MNW2(25,J)=ONE
                          IF (CAPTABLE(J, PUMPCAP+2,2).LE. DZ) CAPTABLE(J,PUMPCAP+2,2)=FWEL%Qcap(I)      !ONLY SET QDES AT UPPER END OF CAPTABLE IF NOT SET ALREADY
                          !
                          WRITE(IOUT,'(2A)') 'FMP-MNW2 LINKED WELL: ', FWEL%WELLID(I)
                          WRITE(IOUT,'(A, ES12.4)') 'REFERENCE HEAD FOR CALCULATING LIFT = ', MNW2(23,J)
                          WRITE(IOUT,'(A, ES12.4)') 'PUMP CAPACITY MULTIPLIER = ', MNW2(24,J)
                          IF(MNWPRNT.GT.Z  ) WRITE(IOUT,'(A, ES12.4)') 'HWTOL = ', MNW2(28,J)
                          IF(MNWPRNT.GT.ONE) THEN
                              WRITE(IOUT,'(5x A)') '(NOTE: SOLUTION MAY BE SENSITIVE TO VALUE OF HWTOL; ADJUST VALUE IF SOLUTION FAILS TO CONVERGE.)'
                              !
                              !   ZERO CAPACITY USE FLAG IF MNW2(24,J)=0
                              IF( ABS(MNW2(24,J)) < DZ) THEN
                                                        MNW2(25,J)=Z
                                                        MNW2(27,J)=Z
                              END IF
                              WRITE(IOUT,*) 
                              WRITE(IOUT,*) 'WELL CAPACITY TABLE'
                              WRITE(IOUT,*) '     LIFT     DISCHARGE'
                              DO K=1, PUMPCAP+2
                                               WRITE(IOUT,'(1X,ES12.5,G11.4)') CAPTABLE(J,K,1), CAPTABLE(J,K,2)
                              END DO
                          END IF
                     END IF
                END IF
          END IF
        END DO WELL
        !
        IF(RECAL_QMAX) THEN
                           CALL CALCULATE_QMAX(FWEL)
                           !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
                           CALL CALCULATE_QMAXini(FWEL)
        END IF
    END IF
    !
    IF(FWEL%NF==FWEL%NFARM) THEN
        IF(FWEL%WRN%RAISED ) CALL FWEL%WRN %CHECK(HED='FMP-MNW2 WELL LINK HAD TO TURN ON OR OFF AUTMATICALLY THE FOLLOWING MNW2 WELLS'//BLN//'   FARM   WELLID               ON/OFF',OUTPUT=FWEL%IOUT, INIT=TRUE)
        !
        IF(FWEL%WRN2%RAISED) CALL FWEL%WRN2%CHECK(HED='THE FOLLOWING FMP FWELLs HAVE A MNW-LINK POINTS TO'//NL//'AN MNW2 WELL THAT IS INACTIVE (NOT AVAILIBLE) FOR THE CURRENT STRESS PERIOD.'//NL// &
                                                      'THESE FARM WELLS WILL BE REMOVED FROM THIS STRESS PERIOD'//NL//'AND NOT BE AVAILIBLE FOR PUMPING SINCE THERE IS NO MNW2 WELL TO PUMP FROM.'//NL//'FARM       WELLID',OUTPUT=FWEL%IOUT, INIT=TRUE)
        !
        IF(FWEL%WRN3%RAISED) CALL FWEL%WRN3%CHECK(HED='THE FOLLOWING FMP FWELLs ARE INACTIVE (NOT IN USE) FOR CURRENT STRESS PERIOD,'//NL//'BUT THEY ARE LINKED TO AN MNW WELL THAT IS ACTIVE (AVAILIBLE) FOR THIS STRESS PERIOD.'//NL// &
                                                      'THE FOLLOWING MNW2 WELLs WILL HAVE THEIR DESIRED PUMPING RATE SET TO ZERO FOR THIS STRESS PERIOD'//NL//'THIS WILL RESULT IN NO PUMPING BUT ALLOWS INTRABOREHOLE FLOW.'//NL//'FARM       WELLID',OUTPUT=FWEL%IOUT, INIT=TRUE)
    END IF
    !
  END SUBROUTINE
  !
  !!!IMPURE ELEMENTAL SUBROUTINE FWELL_MNW2_SP_INIT(FWEL)
  !!!  USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD, NMNW2, CapTable, MNWPRNT
  !!!  CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
  !!!  INTEGER:: IOUT
  !!!  INTEGER:: I, J, K 
  !!!  INTEGER:: PUMPCAP, FIRSTNODE, LASTNODE!,IL,IR,IC
  !!!  LOGICAL:: RECAL_QMAX, MASTER_ACT
  !!!  CHARACTER(:),ALLOCATABLE:: NAM_NOMNW,NAM_NOFMP
  !!!  INTEGER:: IACT, NF
  !!!  !
  !!!  IF(FWEL%NF==ONE) FWEL%MSG = 'FMP-MNW2 WELL LINK HAD TO TURN ON THE FOLLOWING MNW2 WELLS'//BLN//'    FARM   WELLID'
  !!!
  !!!  !IF(FWEL%NF==ONE) WRITE(*,'(I6/,A)') FWEL%MSG%N,FWEL%MSG%STR
  !!!  !
  !!!  IF(FWEL%MNWLINK .AND. FWEL%DIM > Z) THEN
  !!!      !
  !!!      NF = FWEL%NF
  !!!      IOUT = FWEL%IOUT
  !!!      RECAL_QMAX = FALSE
  !!!      NAM_NOMNW = BLNK
  !!!      NAM_NOFMP = BLNK
  !!!      WELL: DO I=1, FWEL%DIM
  !!!        IF(FWEL%MNWLOC(I) > Z) THEN
  !!!              J=FWEL%MNWLOC(I)
  !!!              IACT = NINT(MNW2(1,J))
  !!!              MASTER_ACT = FWEL%MASTER_ACT( FWEL%MASTER_LOC(I) )  !SET TO TRUE IF ANY WELLID HAS ACT OR EXT SET TO TRUE
  !!!              !
  !!!              IF (IACT == Z .AND. .NOT. MASTER_ACT) CYCLE WELL
  !!!              !   
  !!!              IF (IACT == Z .AND. MASTER_ACT) THEN
  !!!                  !
  !!!                  IF(FWEL%MNW_ON) THEN
  !!!                      CALL FWEL%MSG%ADD(NL//BLNK//NUM2STR(NF,-8)//BLNK//TRIM(FWEL%WELLID(I)))
  !!!                      !
  !!!                      MNW2(ONE,J) = UNO  !TURN ON MNW2 WELL
  !!!                      NMNW2=NMNW2+ONE    !FLAG FOR MNW2 TO USE FM ROUTINE
  !!!                  ELSE
  !!!                      IF(FWEL%ACT(I)) FWEL%N = FWEL%N - ONE
  !!!                      !
  !!!                      FWEL%ACT(I) = FALSE
  !!!                      FWEL%EXT(I) = FALSE
  !!!                      RECAL_QMAX = TRUE
  !!!                      NAM_NOMNW = TRIM(NAM_NOMNW)//NL//FWEL%WELLID(I)
  !!!                      CYCLE WELL
  !!!                  END IF
  !!!                  !
  !!!              ELSEIF ( IACT == ONE .AND. .NOT. MASTER_ACT) THEN
  !!!                  IF(FWEL%MNW_OFF) THEN
  !!!                      MNW2(ONE,J) = DZ   !TURN ON MNW2 WELL
  !!!                      NMNW2=NMNW2-ONE    !FLAG FOR MNW2 TO USE FM ROUTINE
  !!!                      IF(NMNW2<Z) NMNW2=Z
  !!!                  ELSE
  !!!                      NAM_NOFMP = TRIM(NAM_NOFMP)//NL//FWEL%WELLID(I)
  !!!                  END IF
  !!!                  !WRITE(IOUT,'(3A)') 'WARNING: FMP FWELL "',FWEL%WELLID(I),'" WITH MNW-LINK IS NOT SPECIFIED FOR USE DURING THIS STRESS PERIOD, BUT THE MNW2 WELL OF THE SAME NAME IS AVAILIBLE. THE MNW2 WELL WILL HAVE ITS DESIRED PUMPING RATE SET TO ZERO FOR THE STRESS PERIOD (NO PUMPING BUT INTRABOREHOLE FLOW).'
  !!!              END IF
  !!!              !
  !!!              IF(NMNW2 < ONE .AND. MASTER_ACT) NMNW2=ONE    !FLAG FOR MNW2 TO USE FM/BD ROUTINE
  !!!              !
  !!!              FIRSTNODE=NINT( MNW2(4,J) )
  !!!              LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )
  !!!              !
  !!!              MNW2(5,J)=DZ                                                ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
  !!!              MNWNOD(4,FIRSTNODE:LASTNODE)=DZ
  !!!              !
  !!!              PUMPCAP=NINT(MNW2(22,J))
  !!!              IF(PUMPCAP.GT.Z) THEN
  !!!                   IF( FWEL%Qcap(I) < MINSIZE ) THEN                           !IF QDES IS 0D0 FOR THIS STRESS PERIOD, DO NOT APPLY PUMP CAPACITY RESTRAINTS (Should )
  !!!                        MNW2(25,J)=Z
  !!!                        MNW2(27,J)=Z                                        !INITIALIZE CAPFLAG
  !!!                   ELSE
  !!!                        MNW2(25,J)=ONE
  !!!                        IF (CAPTABLE(J, PUMPCAP+2,2).LE. DZ) CAPTABLE(J,PUMPCAP+2,2)=FWEL%Qcap(I)      !ONLY SET QDES AT UPPER END OF CAPTABLE IF NOT SET ALREADY
  !!!                        !
  !!!                        WRITE(IOUT,'(2A)') 'FMP-MNW2 LINKED WELL: ', FWEL%WELLID(I)
  !!!                        WRITE(IOUT,'(A, ES12.4)') 'REFERENCE HEAD FOR CALCULATING LIFT = ', MNW2(23,J)
  !!!                        WRITE(IOUT,'(A, ES12.4)') 'PUMP CAPACITY MULTIPLIER = ', MNW2(24,J)
  !!!                        IF(MNWPRNT.GT.Z  ) WRITE(IOUT,'(A, ES12.4)') 'HWTOL = ', MNW2(28,J)
  !!!                        IF(MNWPRNT.GT.ONE) THEN
  !!!                            WRITE(IOUT,'(5x A)') '(NOTE: SOLUTION MAY BE SENSITIVE TO VALUE OF HWTOL; ADJUST VALUE IF SOLUTION FAILS TO CONVERGE.)'
  !!!                            !
  !!!                            !   ZERO CAPACITY USE FLAG IF MNW2(24,J)=0
  !!!                            IF( ABS(MNW2(24,J)) < DZ) THEN
  !!!                                                      MNW2(25,J)=Z
  !!!                                                      MNW2(27,J)=Z
  !!!                            END IF
  !!!                            WRITE(IOUT,*) 
  !!!                            WRITE(IOUT,*) 'WELL CAPACITY TABLE'
  !!!                            WRITE(IOUT,*) '     LIFT     DISCHARGE'
  !!!                            DO K=1, PUMPCAP+2
  !!!                                             WRITE(IOUT,'(1X,ES12.5,G11.4)') CAPTABLE(J,K,1), CAPTABLE(J,K,2)
  !!!                            END DO
  !!!                        END IF
  !!!                   END IF
  !!!              END IF
  !!!        END IF
  !!!      END DO WELL
  !!!      !
  !!!      IF(RECAL_QMAX) THEN
  !!!                         CALL CALCULATE_QMAX(FWEL)
  !!!                         !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
  !!!                         CALL CALCULATE_QMAXini(FWEL)
  !!!      END IF
  !!!      !
  !!!      IF( NAM_NOMNW .NE. BLNK ) THEN
  !!!          CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG='FMP FWELL: FOR FARM '//NUM2STR(FWEL%NF)//NL// &
  !!!                                    'THE FOLLOWING FMP FWELLs HAVE A MNW-LINK POINTS TO AN MNW2 WELL THAT IS INACTIVE (NOT AVAILIBLE) FOR THE CURRENT STRESS PERIOD.'//NL// &
  !!!                                    'THESE FARM WELLS WILL BE REMOVED FROM THIS STRESS PERIOD:'//NL//NAM_NOMNW)
  !!!      END IF
  !!!      
  !!!      IF( NAM_NOFMP .NE. BLNK ) THEN
  !!!          CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG='FMP FWELL: FOR FARM '//NUM2STR(FWEL%NF)//NL// &
  !!!                                     'THE FOLLOWING FMP FWELLs ARE INACTIVE (NOT IN USE) FOR CURRENT STRESS PERIOD,'//NL// &
  !!!                                     'BUT THEY ARE LINKED TO AN MNW WELL THAT IS ACTIVE (AVAILIBLE) FOR THIS STRESS PERIOD.'//NL// &
  !!!                                     'THE MNW2 WELL WILL HAVE ITS DESIRED PUMPING RATE SET TO ZERO FOR THIS STRESS PERIOD (NO PUMPING BUT INTRABOREHOLE FLOW).'//NL// &
  !!!                                     'THE FOLLWING ARE THOSE WELLS:'//NL//NAM_NOFMP)
  !!!      END IF
  !!!  END IF
  !!!  !
  !!!  IF(FWEL%NF==FWEL%NFARM) THEN
  !!!      IF(FWEL%MSG%N>77) CALL FWEL%MSG%WRITE(FWEL%IOUT)
  !!!      FWEL%MSG = BLNK
  !!!  END IF
  !!!  !
  !!!END SUBROUTINE
  !
!  IMPURE ELEMENTAL SUBROUTINE FWELL_MNW2_SP_INIT(FWEL)
!    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD, CapTable, MNWPRNT
!    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
!    INTEGER:: IOUT
!    INTEGER:: I, J, K 
!    INTEGER:: PUMPCAP, FIRSTNODE, LASTNODE!,IL,IR,IC
!    LOGICAL:: RECAL_QMAX
!    CHARACTER(:),ALLOCATABLE:: NAM_NOMNW, NAM_NOFMP
!    INTEGER:: IACT, NF
!    !
!    IF(.NOT. FWEL%MNWLINK .OR. FWEL%N == Z) RETURN
!    !
!    NF = FWEL%NF
!    IOUT = FWEL%IOUT
!    RECAL_QMAX = FALSE
!    NAM_NOMNW = BLNK
!    NAM_NOFMP = BLNK
!    WELL: DO I=1, FWEL%DIM
!      IF(FWEL%MNWLOC(I) > Z) THEN
!            J=FWEL%MNWLOC(I)
!            IACT = NINT(MNW2(1,J))
!            !
!            IF (IACT == Z .AND. .NOT. FWEL%ACT(I) .AND. .NOT. FWEL%EXT(I)) CYCLE WELL
!            !   
!            IF (IACT == Z) THEN
!                !
!                IF(FWEL%ACT(I)) FWEL%N = FWEL%N - ONE
!                !
!                FWEL%ACT(I) = FALSE
!                FWEL%EXT(I) = FALSE
!                RECAL_QMAX = TRUE
!                NAM_NOMNW = TRIM(NAM_NOMNW)//NL//FWEL%WELLID(I)
!                CYCLE WELL
!                !WRITE(IOUT,'(/3A/)') 'WARNING: FMP FWELL "',FWEL%WELLID(I),'" WITH MNW-LINK POINTS TO AN MNW2 WELL THAT IS INACTIVE (NOT AVAILIBLE) FOR THE CURRENT STRESS PERIOD. THIS FARM WELL WILL BE REMOVED FROM THIS STRESS PERIOD'
!                !
!            ELSEIF ( .NOT. FWEL%ACT(I) .AND. .NOT. FWEL%EXT(I)) THEN
!                NAM_NOFMP = TRIM(NAM_NOFMP)//NL//FWEL%WELLID(I)
!                !WRITE(IOUT,'(3A)') 'WARNING: FMP FWELL "',FWEL%WELLID(I),'" WITH MNW-LINK IS NOT SPECIFIED FOR USE DURING THIS STRESS PERIOD, BUT THE MNW2 WELL OF THE SAME NAME IS AVAILIBLE. THE MNW2 WELL WILL HAVE ITS DESIRED PUMPING RATE SET TO ZERO FOR THE STRESS PERIOD (NO PUMPING BUT INTRABOREHOLE FLOW).'
!            END IF
!            !
!            FIRSTNODE=NINT( MNW2(4,J) )
!            LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - 1D0 )
!            !
!            MNW2(5,J)=DZ                                                ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
!            MNWNOD(4,FIRSTNODE:LASTNODE)=DZ
!            !
!            PUMPCAP=NINT(MNW2(22,J))
!            IF(PUMPCAP.GT.Z) THEN
!                 IF( FWEL%Qcap(I) < MINSIZE ) THEN                           !IF QDES IS 0D0 FOR THIS STRESS PERIOD, DO NOT APPLY PUMP CAPACITY RESTRAINTS (Should )
!                      MNW2(25,J)=Z
!                      MNW2(27,J)=Z                                        !INITIALIZE CAPFLAG
!                 ELSE
!                      MNW2(25,J)=ONE
!                      IF (CAPTABLE(J, PUMPCAP+2,2).LE. DZ) CAPTABLE(J,PUMPCAP+2,2)=FWEL%Qcap(I)      !ONLY SET QDES AT UPPER END OF CAPTABLE IF NOT SET ALREADY
!                      !
!                      WRITE(IOUT,'(2A)') 'FMP-MNW2 LINKED WELL: ', FWEL%WELLID(I)
!                      WRITE(IOUT,'(A, ES12.4)') 'REFERENCE HEAD FOR CALCULATING LIFT = ', MNW2(23,J)
!                      WRITE(IOUT,'(A, ES12.4)') 'PUMP CAPACITY MULTIPLIER = ', MNW2(24,J)
!                      IF(MNWPRNT.GT.Z  ) WRITE(IOUT,'(A, ES12.4)') 'HWTOL = ', MNW2(28,J)
!                      IF(MNWPRNT.GT.ONE) THEN
!                          WRITE(IOUT,'(5x A)') '(NOTE: SOLUTION MAY BE SENSITIVE TO VALUE OF HWTOL; ADJUST VALUE IF SOLUTION FAILS TO CONVERGE.)'
!                          !
!                          !   ZERO CAPACITY USE FLAG IF MNW2(24,J)=0
!                          IF( ABS(MNW2(24,J)) < DZ) THEN
!                                                    MNW2(25,J)=Z
!                                                    MNW2(27,J)=Z
!                          END IF
!                          WRITE(IOUT,*) 
!                          WRITE(IOUT,*) 'WELL CAPACITY TABLE'
!                          WRITE(IOUT,*) '     LIFT     DISCHARGE'
!                          DO K=1, PUMPCAP+2
!                                           WRITE(IOUT,'(1X,ES12.5,G11.4)') CAPTABLE(J,K,1), CAPTABLE(J,K,2)
!                          END DO
!                      END IF
!                 END IF
!            END IF
!      END IF
!    END DO WELL
!    !
!    IF(RECAL_QMAX) THEN
!                       CALL CALCULATE_QMAX(FWEL)
!                       !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
!                       CALL CALCULATE_QMAXini(FWEL)
!    END IF
!    !
!    IF( NAM_NOMNW .NE. BLNK ) THEN
!        CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG='FMP FWELL: FOR FARM '//NUM2STR(FWEL%NF)//NL// &
!                                  'THE FOLLOWING FMP FWELLs HAVE A MNW-LINK POINTS TO AN MNW2 WELL THAT IS INACTIVE (NOT AVAILIBLE) FOR THE CURRENT STRESS PERIOD.'//NL// &
!                                  'THESE FARM WELLS WILL BE REMOVED FROM THIS STRESS PERIOD:'//NL//NAM_NOMNW)
!    END IF
!    
!    IF( NAM_NOFMP .NE. BLNK ) THEN
!        CALL WARNING_MESSAGE(OUTPUT=IOUT,MSG='FMP FWELL: FOR FARM '//NUM2STR(FWEL%NF)//NL// &
!                                   'THE FOLLOWING FMP FWELLs ARE INACTIVE (NOT IN USE) FOR CURRENT STRESS PERIOD,'//NL// &
!                                   'BUT THEY ARE LINKED TO AN MNW WELL THAT IS ACTIVE (AVAILIBLE) FOR THIS STRESS PERIOD.'//NL// &
!                                   'THE MNW2 WELL WILL HAVE ITS DESIRED PUMPING RATE SET TO ZERO FOR THIS STRESS PERIOD (NO PUMPING BUT INTRABOREHOLE FLOW).'//NL// &
!                                   'THE FOLLWING ARE THOSE WELLS:'//NL//NAM_NOFMP)
!    END IF
!    !
!  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE FWELL_ZERO_Q(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    !
    IF(FWEL%DIM > Z ) FWEL%Q = DZ
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FWELL_MNW2_ZERO_Q(FWEL)
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER:: I, J, FIRSTNODE, LASTNODE
    !
    IF(.NOT. FWEL%MNWLINK) RETURN  
    IF(FWEL%DIM == Z     ) RETURN
    !
    DO I=1, FWEL%DIM
      IF( FWEL%MNWLOC(I) > Z ) THEN
            J=FWEL%MNWLOC(I)
            !
            FIRSTNODE=NINT( MNW2(4,J) )
            LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
            !
            MNW2(5 ,J) = DZ                                                ! RP SENDS A MAX FLOW RATE TO MNW2 SO IT CAN CALCULATE AN INITIAL WELL HEAD
            MNW2(18,J) = DZ 
            MNWNOD(4,FIRSTNODE:LASTNODE) = DZ
            !
      END IF
    END DO
    !
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE FWELL_MNW2_UPDATE_CAPACITY(FWEL)
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL !FARMWELL
    INTEGER:: I, J, FIRSTNODE, LASTNODE
    DOUBLE PRECISION:: QDES, QACT
    DOUBLE PRECISION:: FPS
    LOGICAL:: RECAL_QMAX
    !
    IF(.NOT. FWEL%MNWLINK) RETURN  
    IF(FWEL%DIM == Z     ) RETURN  !NO WELLS TO UPDATE CAP WITH
    !
    FPS=1D-6
    !
    RECAL_QMAX = FALSE
    !
    DO I=1, FWEL%DIM
           IF( FWEL%MNWLOC(I) > Z .AND. (FWEL%ACT(I) .OR. FWEL%EXT(I)) ) THEN
                 J=FWEL%MNWLOC(I)
                 !
                 FIRSTNODE=NINT( MNW2(4,J) )
                 LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                 !
                 QDES=DNEG*MNW2(5,J)                                           !Flip sign back to positive to represent a capacity
                 IF(QDES > NEARZERO_10) THEN
                       QACT=DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE))                   !SUM ALL RATES FROM EACH NODE TO GET ACTUAL PUMPING RATE
                       !
                       !IF ( QACT+FPS < QDES  .AND. QACT > NEARZERO_10 ) THEN          !QDES was set by FMP to meet demand, but actual pumping is less, so update capacty
                       !    FWEL%Qcap(I) = QACT   !MISSED (I)
                       !    IF(FWEL%ACT(I)) RECAL_QMAX   = TRUE
                       !
                       IF ( QACT+FPS < QDES ) THEN
                           IF ( QACT < NEARZERO_10 ) THEN
                               FWEL%Qcap(I) = DZ
                           ELSE
                               FWEL%Qcap(I) = QACT   !MISSED (I)
                           END IF
                           IF(FWEL%ACT(I)) RECAL_QMAX   = TRUE
                       END IF
                 END IF
                 !
           END IF
    END DO
    !
    IF(RECAL_QMAX) THEN
                       CALL CALCULATE_QMAX(FWEL)
                       !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE SET_GROUNDWATER_ALLOTMENT(FWEL,ALLOTGW)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    DOUBLE PRECISION, INTENT(IN):: ALLOTGW
    !
    FWEL%ALLOT = ALLOTGW
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE APPLY_GROUNDWATER_ALLOTMENT(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    DOUBLE PRECISION::  FRAC, QMAX
    !
    IF(FWEL%DIM   == Z      ) RETURN
    IF(FWEL%ALLOT < -MINSIZE) RETURN
    !
    QMAX = SUM(FWEL%Qcap, MASK = FWEL%ACT .OR. FWEL%EXT)  ! ALLOTMENT SHOULD BE APPLIED ONLY TO UNSMOOTHED MAXIMUM CAPACITY. IF THEY LOSE CAPACITY TO DEWATERING THAT IS THEIR OWN PROBLEM.
    !
    IF (QMAX > FWEL%ALLOT .AND. QMAX > MINSIZE) THEN
        !
        SELECT CASE( FWEL%PRORATE_ALLOT )
        CASE(0)                            ! REDUCTION BY FRACTION OF ALLOT/QMAX
               FRAC = FWEL%ALLOT / QMAX
               FWEL%Qcap = FWEL%Qcap * FRAC
        CASE(1)                            !TBA
             CONTINUE                      !Place holder for future options
        END SELECT
        !
        CALL CALCULATE_QMAX(FWEL)
        !CALL CALCULATE_FRAC(FWEL)  !currently only needed during final calculation
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE AUX_QMAXRESET(FWEL)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    !DOUBLE PRECISION::  FRAC, QMAX
    !
    IF(FWEL%DIM > Z .AND. FWEL%QMAXRESET) THEN
        !
        CALL COPY_QINI_TO_QCAP(FWEL)
        !
        IF(FWEL%ALLOT < DZ) THEN
                                    CALL CALCULATE_QMAX(FWEL)
        ELSE!IF(IALLOTGW.NE.Z) THEN
                                    CALL APPLY_GROUNDWATER_ALLOTMENT(FWEL)
        END IF
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE AUX_NOCIRNOQ(FWEL,TDR,RESETQ)
    CLASS(FARM_WELL_DATA), INTENT(INOUT):: FWEL
    DOUBLE PRECISION,      INTENT(IN   ), DIMENSION(:,:), CONTIGUOUS:: TDR
    LOGICAL,               INTENT(IN   ):: RESETQ
    INTEGER:: I
    DOUBLE PRECISION:: FPS
    LOGICAL:: ZERO_Q
    !
    IF(FWEL%N > 0 .AND. FWEL%NOCIRNOQ) THEN
        !
        FPS = NEARZERO_10
        ZERO_Q = FALSE
        !
        IF(RESETQ) CALL COPY_QINI_TO_QCAP(FWEL)
        !
        DO CONCURRENT (I=1:FWEL%DIM, TDR( FWEL%LRC(3,I), FWEL%LRC(2,I) ) < FPS .AND. .NOT. FWEL%EXT(I))
            FWEL%Qcap(I)  = DZ
            ZERO_Q = TRUE
        END DO
        !
        IF (ZERO_Q) THEN
            IF(FWEL%ALLOT < DZ) THEN
                                        CALL CALCULATE_QMAX(FWEL)
            ELSE!IF(IALLOTGW.NE.Z) THEN
                                        CALL APPLY_GROUNDWATER_ALLOTMENT(FWEL)
            END IF
        END IF
        !
    END IF
    !
  END SUBROUTINE
  !
  PURE ELEMENTAL SUBROUTINE CALCULATE_PUMPING_TO_MEET_DEMAND(FWEL,QREQ)  !ASSUMES FWEL%QMAX HAS BEEN CALCULATED
    !
    !   IF QMAXF > QREQ: THEN
    !       CASE(0)
    !        --> PULL THE VALUES OF FARM-WELL DEFICIENCY AND EXCESS COMPARED TO Q-AVERAGE
    !        --> CHECK FOR CAPACITY CRITERION OF INDIVIDUAL WELLS:
    !            PUMP WELLS ON AVERAGE WITH QAVF:
    !              WELLS WITH LESS THAN QAVF: PUMP Qcap
    !              WELLS WITH MORE THAN QAVF: PUMP QAVF + SHARE WHICH ACCOUNTS FOR THE DEFICIENCIES OF 'WELLS WITH LESS THAN QAVF'
    !       CASE(1)
    !        --> CALCULATE FRACTION OF WELL CAPACITY TO QMAX
    !        --> APPROPRIATE PUMPAUGE BASED ON FRACTION TO MEET QREQ
    !
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    DOUBLE PRECISION, INTENT(IN)::QREQ
    DOUBLE PRECISION:: QAVF, QDEF, QEXC, FRAC
    INTEGER:: I
    !
    IF(FWEL%N == Z) RETURN  !PUMPAGE IS ZEROED OUT AT START OF FM
    !
    CALL CALCULATE_QMAX(FWEL)   !FIND TOTAL CAPACITY  --MAYBE REDUNDANT, BUT DONE TO BE SAFE
    !
    ! DETERMINE PUMPING RATE TO MEET REQUIREMENT
    !
    IF (FWEL%QMAX > QREQ) THEN  !PUMPING CAN MEET DEMAND SO CALCULATE THE DISTRIBUTION OF PUMPAUGE --IF NOT TRUE THEN ALL WELLS ARE MAXED OUT
       !
       SELECT CASE (FWEL%PRORATE_Q)
       CASE (0) ! PRORATE BY AVERAGE REQUIRED PUMPING
                !
                CALL COPY_QCAP_TO_Q_ACT(FWEL) !SET UP INITIAL ESTIMATE OF Q (ASSUME ALL WELLS ARE MAXED OUT AND WILL BE REDUCED TO MEET QREQ). NOTE THIS WILL ALSO INCLUDE SMOOTHING
                !
                QAVF = QREQ / DBLE(FWEL%N)  !AVERAGE FLOW PER WELL
                !
                QDEF = DZ
                QEXC = DZ
                FRAC = DZ
                DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) )
                    IF ( FWEL%Q(I) > QAVF ) THEN
                                                 QEXC = QEXC + FWEL%Q(I) - QAVF
                    ELSE
                                                 QDEF = QDEF + QAVF - FWEL%Q(I)
                    END IF
                END DO
                !
                IF(QEXC .NE. DZ) FRAC = QDEF/QEXC
                !
                DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) )
                    IF (FWEL%Q(I) > QAVF) THEN
                        FWEL%Q(I) = DNEG * ( FRAC*(FWEL%Q(I)-QAVF) + QAVF )
                    ELSE
                        FWEL%Q(I) = DNEG * FWEL%Q(I)
                    END IF
                END DO
                !
       CASE (1) ! PRORATE PUMPING BY FRACTIONS OF Qcap/QMAX
                !
                CALL CALCULATE_FRAC(FWEL)
                DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) ); FWEL%Q(I) = DNEG * QREQ * FWEL%FRAC(I)
                END DO
                !
       END SELECT
       !
       DO CONCURRENT (I=1:FWEL%DIM) !ZERO OUT WELLS THAT HAVE SMALL PUMPAGE
                                    IF(FWEL%ACT(I)) THEN
                                       IF(NEGNEARZERO_10 < FWEL%Q(I) .AND. FWEL%Q(I) < NEARZERO_10)  THEN
                                           FWEL%Q(I) = DZ
                                       END IF
                                    ELSE
                                        FWEL%Q(I) = DZ
                                    END IF
       END DO
       !
       DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) )
           IF(FWEL%Q(I) > DZ) THEN
                                  FWEL%DMD(I) = DZ
           ELSE
                                  FWEL%DMD(I) = DNEG*FWEL%Q(I)
           END IF
       END DO
       !
    ELSE !QMAX<QREQ
                CALL COPY_QCAP_TO_Q_ACT(FWEL) ! PUMPING IS MAXED OUT. COPY OVER CAPACITIES. NOTE THIS WILL ALSO INCLUDE SMOOTHING
                !
                DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) ); FWEL%Q(I) = DNEG * FWEL%Q(I)
                END DO
                !
                CALL CALCULATE_FRAC(FWEL)
                DO CONCURRENT ( I=1:FWEL%DIM, FWEL%ACT(I) ); FWEL%DMD(I) = QREQ * FWEL%FRAC(I)
                END DO
    END IF
    !
  END SUBROUTINE 
  !
  IMPURE ELEMENTAL SUBROUTINE APPLY_PUMPING_TO_MEET_DEMAND(FWEL)  !ONLY RUN AFTER CALLING CALCULATE_PUMPING_TO_MEET_DEMAND
    USE GLOBAL,        ONLY: RHS
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER:: I, J, FIRSTNODE, LASTNODE
    INTEGER:: IL, IR, IC
    DOUBLE PRECISION:: TMP
    !
    IF(FWEL%DIM == 0) RETURN
    !
    DO CONCURRENT ( I=1:FWEL%DIM, (FWEL%ACT(I) .OR. FWEL%EXT(I)) .AND. FWEL%MNWLOC(I) == Z ) 
              ! FOR WELLS SIMULATED WITH THE FARM PROCESS:
              ! SEND FLOW RATE TO RHS OF FINITE DIFFERENCE EQUATION
              IL = FWEL%LRC(1,I)
              IR = FWEL%LRC(2,I)
              IC = FWEL%LRC(3,I)
              !
              RHS(IC,IR,IL) = RHS(IC,IR,IL) - FWEL%Q(I)
    END DO
    !
    IF (FWEL%MNWLINK) THEN  !ONLY LOOK FOR MNW-LINK WELLS WHEN THERE ARE ONES WITHIN FARM
       !
       DO CONCURRENT ( I=1:FWEL%DIM, (FWEL%ACT(I) .OR. FWEL%EXT(I)) .AND. FWEL%MNWLOC(I) > Z )
              !
              J=FWEL%MNWLOC(I)
              FIRSTNODE=NINT( MNW2(4,J) )
              LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
              !
              MNW2(5 ,J) = FWEL%Q(I)             !SET QDES TO Q IN MNW2 NOTE THAT MNW2 DOES NOT REALLY MAKE USE OF QDEV IN BETWEEN STRESS PERIODS
              MNW2(18,J) = FWEL%Q(I)             !SET QACT TO Q SO MNW2 CAN ADJUST IT
              !
              SELECT CASE (FWEL%MNW_SPREAD)
              CASE(Z)
                    TMP = DZ
                    DO CONCURRENT (IR=FIRSTNODE:LASTNODE)
                        TMP = TMP + MNWNOD(14,IR)
                    END DO
                    IF(TMP>DZ) THEN
                         DO CONCURRENT (IR=FIRSTNODE:LASTNODE)
                             MNWNOD(4,IR) = FWEL%Q(I)*(MNWNOD(14,IR)/TMP)
                         END DO
                    ELSE
                             MNWNOD(4,FIRSTNODE:LASTNODE) = FWEL%Q(I)/DBLE(LASTNODE-FIRSTNODE+ONE)    !SPREAD PUMPAGE EVENTLY ACROSS THE NODES
                    END IF
              CASE(ONE)
                       MNWNOD(4,FIRSTNODE:LASTNODE) = DZ
                       MNWNOD(4,FIRSTNODE) = FWEL%Q(I)    !SET FIRST NODE TO Q SO MNW2 CAN SPREAD PUMPING ACROSS THE REMAINDER OF NODES
              CASE(TWO)
                       MNWNOD(4,FIRSTNODE:LASTNODE) = FWEL%Q(I)/DBLE(LASTNODE-FIRSTNODE+ONE)    !SPREAD PUMPAGE EVENTLY ACROSS THE NODES
              END SELECT
       END DO
    END IF
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE PRINT_INPUT_TRANSCRIPT(FWEL, KPER, IU2)
    USE GLOBAL,                     ONLY: BOTM, LBOTM
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER, INTENT(IN):: KPER
    INTEGER, INTENT(IN), OPTIONAL:: IU2
    DOUBLE PRECISION:: SMOOTH_LIM
    INTEGER:: I, J, IL, IR, IC, IU, FD
    CHARACTER(7):: MNWLINK, ACT, EXT
    CHARACTER(15):: PRORATE
    CHARACTER(12):: QMAXRESET, NOCIRNOQ
    CHARACTER(20):: AUX
    CHARACTER(17):: SMOOTH, SMF, QINI, QCAP, FRAC
    CHARACTER(:),ALLOCATABLE:: FEEDFILE
    !
    IF(PRESENT(IU2)) THEN
        IU = IU2
    ELSE
        IF(FWEL%NF == ONE) CALL FWEL%OUT_INPUT%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
        IU = FWEL%OUT_INPUT%IU
    END IF
    !
    IF(IU == Z) RETURN
    !
    AUX =''
    IF(FWEL%NF == ONE .AND. ((KPER==ONE .OR. IU==FWEL%IOUT) .OR. FWEL%IOUT==IU) ) THEN  !PRINT FOR FIRST FARM AND EITHER FIRST SP/TS OR IF WRITING TO LIST FILE
        !
        IF (FWEL%IOUT==IU) WRITE(IU,*)
        !
        IF(FWEL%NAUX> Z) AUX = '  AUX'
        SELECT CASE (FWEL%LOAD_TYPE)
        CASE (1)   !LIST
                              CALL FWEL%OUT_INPUT%SET_HEADER( '   PER  FARM   WELLID              ACTIVE EXTERN    LAY   ROW   COL        Q-CAP-INI            Q-CAP MNWLINK  PRORATE_DEMAND      QMAX_FRAC  MIN_THCK_SMOOTH    INI_SM_FACTOR  QMAXRESET   NOCIRNOQ     SP_START  SP_END'//TRIM(AUX) )
        CASE (2,3) !LINEFEED
                   IF(FWEL%NAUX> Z) THEN
                              CALL FWEL%OUT_INPUT%SET_HEADER( '   PER  FARM   WELLID              ACTIVE EXTERN    LAY   ROW   COL        Q-CAP-INI            Q-CAP MNWLINK  PRORATE_DEMAND      QMAX_FRAC  MIN_THCK_SMOOTH    INI_SM_FACTOR  QMAXRESET   NOCIRNOQ     '//AUX//'   FEEDFILE'     )!FWEL%FEED%IU.NE.0
                   ELSE
                              CALL FWEL%OUT_INPUT%SET_HEADER( '   PER  FARM   WELLID              ACTIVE EXTERN    LAY   ROW   COL        Q-CAP-INI            Q-CAP MNWLINK  PRORATE_DEMAND      QMAX_FRAC  MIN_THCK_SMOOTH    INI_SM_FACTOR  QMAXRESET   NOCIRNOQ          FEEDFILE'     )!FWEL%FEED%IU.NE.0
                   END IF
        END SELECT
        AUX =''
    END IF
    !
    IF(FWEL%DIM == Z) RETURN  ! NO WELLS TO PRINT
    !
    IF(ANY( FWEL%LOAD_TYPE == [2,3] ))  ALLOCATE(CHARACTER(700)::FEEDFILE)
    IF(     FWEL%LOAD_TYPE == 3 )     INQUIRE(FWEL%FEED%IU, NAME=FEEDFILE)
    !
    DO CONCURRENT (I=1:FWEL%DIM, .NOT. (FWEL%ACT(I) .OR. FWEL%EXT(I)));   FWEL%Qcap(I) = DZ
    END DO
    !
    IF(FWEL%PRORATE_Q==ONE) THEN
         PRORATE='     ByCapacity'
         !WHERE(.NOT. FWEL%ACT) FWEL%FRAC = DZ
         CALL CALCULATE_FRAC(FWEL)
    ELSE        
         PRORATE='     ByAverage'
    END IF
    !
    DO I=ONE,FWEL%DIM
              IL = FWEL%LRC(1,I)
              IR = FWEL%LRC(2,I)
              IC = FWEL%LRC(3,I)
              !
              QINI = NUM2STR(FWEL%Qini(I))
              QINI = ADJUSTR(QINI)
              QCAP = NUM2STR(FWEL%Qcap(I))
              QCAP = ADJUSTR(QCAP)
              !
              IF(FWEL%NAUX > 0) WRITE(AUX,'(*(I2))') (FWEL%AUX(I,J), J=1, FWEL%NAUX)
              !
              IF(FWEL%ACT(I)) THEN
                  ACT='  TRUE'
              ELSE
                  ACT='  FALSE'
              END IF
              IF(FWEL%EXT(I)) THEN
                  EXT='  TRUE'
                  ACT=EXT
              ELSE
                  EXT='  FALSE'
              END IF
              !
              IF(FWEL%MNWLOC(I)>Z) THEN
                  MNWLINK='  TRUE'
              ELSE
                  MNWLINK='  FALSE'
              END IF
              !
              IF(FWEL%QMAXRESET) THEN
                  QMAXRESET='  TRUE'
              ELSE
                  QMAXRESET='  FALSE'
              END IF
              !
              IF(FWEL%NOCIRNOQ) THEN
                  NOCIRNOQ='  TRUE'
              ELSE
                  NOCIRNOQ='  FALSE'
              END IF
              !
              IF(FWEL%PRORATE_Q==ONE) THEN
                  FRAC = NUM2STR(FWEL%FRAC(I))
              ELSE
                  FRAC = 'NaN'
              END IF
              FRAC = ADJUSTR(FRAC)
              !
              IF( FWEL%SMOOTH == Z .OR. IL == Z ) THEN
                    SMOOTH = 'NaN'
                    SMF    = 'NaN'
              ELSE
                   SMF = NUM2STR(FWEL%Qsmf(I))
                   IF(FWEL%SMOOTH_LIM < -0.5D0) THEN
                         SMOOTH_LIM = FWEL%SMOOTH_LIM_BY_LAYER(IL)   !SMOOTH BY LAYER
                   ELSE
                         SMOOTH_LIM = FWEL%SMOOTH_LIM                !USE GLOBAL SMOOTH OR SMOOTH BY FARM
                   END IF
                   !
                   IF (FWEL%SMOOTH == 1) THEN   ! CELL THICKNESS
                         SMOOTH = NUM2STR (SMOOTH_LIM * ( BOTM(IC, IR, LBOTM(IL)-ONE) - BOTM(IC, IR, LBOTM(IL)) ) )
                   ELSE
                         SMOOTH = NUM2STR (SMOOTH_LIM)
                   END IF
              END IF
              SMOOTH = ADJUSTR(SMOOTH)
              SMF    = ADJUSTR(SMF)
              !
              SELECT CASE(FWEL%LOAD_TYPE)
              CASE(1)  !LIST
                                 WRITE(IU,'(2I6, 3x 3A, 3I6, 9A, I6, I10, 2x A)') KPER, FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), QINI, QCAP, MNWLINK, PRORATE, FRAC, SMOOTH, SMF, QMAXRESET, NOCIRNOQ, FWEL%SP_START(I),  FWEL%SP_END(I), TRIM(AUX)
              CASE(2)  !LINEFEED
                                 FEED: DO FD=ONE, SIZE(FWEL%FID_FEED)
                                       DO J =ONE, SIZE(FWEL%FID_FEED(FD)%FID)
                                           IF(FWEL%FID_FEED(FD)%WELLID(J) == FWEL%WELLID(I)) THEN
                                               INQUIRE(FWEL%FID_FEED(FD)%FL%IU, NAME=FEEDFILE)
                                               EXIT FEED
                                           END IF
                                       END DO
                                 END DO FEED
                                 !
                                 WRITE(IU,'(2I6, 3x 3A, 3I6, 9A, 2(2x A))')       KPER, FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), QINI, QCAP, MNWLINK, PRORATE, FRAC, SMOOTH, SMF, QMAXRESET, NOCIRNOQ, TRIM(AUX), TRIM(FEEDFILE)
                                 !
              CASE(3)  !LINEFEED
                                 WRITE(IU,'(2I6, 3x 3A, 3I6, 9A, 2(2x A))')       KPER, FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), QINI, QCAP, MNWLINK, PRORATE, FRAC, SMOOTH, SMF, QMAXRESET, NOCIRNOQ, TRIM(AUX), TRIM(FEEDFILE)
              END SELECT
    END DO
    !
  END SUBROUTINE
  !  
  IMPURE ELEMENTAL SUBROUTINE PRINT_WELL_LIST(FWEL)
    USE GLOBAL,                     ONLY: BOTM, LBOTM
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER:: I, J, IL, IR, IC, IU, FD
    DOUBLE PRECISION:: SMOOTH_LIM
    CHARACTER(7):: MNWLINK, ACT, EXT
    CHARACTER(15):: PRORATE
    CHARACTER(20):: AUX
    CHARACTER(17):: SMOOTH, QINI
    CHARACTER(:),ALLOCATABLE:: FEEDFILE
    !
    AUX =''
    IU = FWEL%IOUT
    !
    IF(FWEL%NF == ONE ) THEN  !PRINT FOR FIRST FARM AND EITHER FIRST SP/TS OR IF WRITING TO LIST FILE
        !
        IF (FWEL%IOUT==IU) WRITE(IU,*)
        !
        IF(FWEL%NAUX> 0) AUX = '  AUX'
        SELECT CASE (FWEL%LOAD_TYPE)
        CASE (1)   !LIST
                              WRITE(IU,'(A)') '  FARM    WELLID              ACTIVE EXTERN  LAY   ROW   COL  MNWLINK   PRORATE_DEMAND  MIN_THCK_SMOOTH     Q-CAP-INI SP_START  SP_END'//TRIM(AUX)
        CASE (2,3) !LINEFEED
                              WRITE(IU,'(A)') '  FARM    WELLID              ACTIVE EXTERN  LAY   ROW   COL  MNWLINK   PRORATE_DEMAND  MIN_THCK_SMOOTH     Q-CAP-INI '//(AUX)//'     FEEDFILE' !FWEL%FEED%IU.NE.0
        END SELECT
        AUX =''
    END IF
    !
    IF(FWEL%DIM == Z) RETURN  ! NO WELLS TO PRINT
    !
    IF(ANY( FWEL%LOAD_TYPE == [2,3] ))  ALLOCATE(CHARACTER(700)::FEEDFILE)
    IF(     FWEL%LOAD_TYPE == 3 )     INQUIRE(FWEL%FEED%IU, NAME=FEEDFILE)
    !
    IF(FWEL%PRORATE_Q==ONE) THEN
         PRORATE='     ByCapacity'
         !WHERE(.NOT. FWEL%ACT) FWEL%FRAC = DZ
         CALL CALCULATE_FRAC(FWEL)
    ELSE        
         PRORATE='     ByAverage'
    END IF
    !
    DO I=ONE,FWEL%DIM
              IL = FWEL%LRC(1,I)
              IR = FWEL%LRC(2,I)
              IC = FWEL%LRC(3,I)
              !
              QINI = NUM2STR(FWEL%Qini(I))
              QINI = ADJUSTR(QINI)
              !
              IF(FWEL%NAUX > Z) WRITE(AUX,'(*(I2))') (FWEL%AUX(I,J), J=1, FWEL%NAUX)
              !
              IF(IL == Z .OR. FWEL%MNWLOC(I)>0) THEN
                  MNWLINK='  TRUE'
              ELSE
                  MNWLINK='  FALSE'
              END IF
              !
              IF(FWEL%ACT(I)) THEN
                  ACT='  TRUE'
              ELSE
                  ACT='  FALSE'
              END IF
              IF(FWEL%EXT(I)) THEN
                  EXT='  TRUE'
                  ACT=EXT
              ELSE
                  EXT='  FALSE'
              END IF
              !
              IF( FWEL%SMOOTH == Z .OR. IL == Z ) THEN
                    SMOOTH = 'NaN'
              ELSE
                   IF(FWEL%SMOOTH_LIM < -0.5D0) THEN
                         SMOOTH_LIM = FWEL%SMOOTH_LIM_BY_LAYER(IL)   !SMOOTH BY LAYER
                   ELSE
                         SMOOTH_LIM = FWEL%SMOOTH_LIM                !USE GLOBAL SMOOTH OR SMOOTH BY FARM
                   END IF
                   !
                   IF (FWEL%SMOOTH == 1) THEN   ! CELL THICKNESS
                         SMOOTH = NUM2STR (SMOOTH_LIM * ( BOTM(IC, IR, LBOTM(IL)-ONE) - BOTM(IC, IR, LBOTM(IL)) ) )
                   ELSE
                         SMOOTH = NUM2STR (SMOOTH_LIM)
                   END IF
              END IF
              SMOOTH = ADJUSTR(SMOOTH)
              !
              SELECT CASE (FWEL%LOAD_TYPE)
              CASE (1)   !LIST
                                    WRITE(IU,'(I6, 2x 3A, 3I6, 4A, I6, I9, 2x A)') FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), MNWLINK, PRORATE, SMOOTH, QINI, FWEL%SP_START(I),  FWEL%SP_END(I), TRIM(AUX)
              CASE (2) !LINEFEED
                                    FEED: DO FD=ONE, SIZE(FWEL%FID_FEED)
                                          DO J =ONE, SIZE(FWEL%FID_FEED(FD)%FID)
                                              IF(FWEL%FID_FEED(FD)%WELLID(J) == FWEL%WELLID(I)) THEN
                                                  INQUIRE(FWEL%FID_FEED(FD)%FL%IU, NAME=FEEDFILE)
                                                  EXIT FEED
                                              END IF
                                          END DO
                                    END DO FEED
                                    !
                                    WRITE(IU,'(I6, 2x 3A, 3I6, 4A, 2(2x A))')      FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), MNWLINK, PRORATE, SMOOTH, QINI, TRIM(AUX), TRIM(FEEDFILE)
              CASE (3) !LINEFEED
                                    WRITE(IU,'(I6, 2x 3A, 3I6, 4A, 2(2x A))')      FWEL%NF, FWEL%WELLID(I), ACT, EXT, FWEL%LRC(:,I), MNWLINK, PRORATE, SMOOTH, QINI, TRIM(AUX), TRIM(FEEDFILE)
              END SELECT
    END DO
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE PRINT_SMOOTHED_PUMPING(FWEL, KPER, KSTP, TOTIME, DATE)  !ONLY RUN AFTER CALLING CALCULATE_PUMPING_TO_MEET_DEMAND
    USE GLOBAL,        ONLY: HNEW, BOTM, LBOTM
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER,               INTENT(IN):: KPER, KSTP
    REAL,                  INTENT(IN):: TOTIME
    CHARACTER(*),          INTENT(IN):: DATE
    !
    DOUBLE PRECISION:: SMOOTH_LIM
    INTEGER:: I, IL, IR, IC, IU
    CHARACTER(17):: QINI, QCAP, QFIN, QSMF, H, BOT, TIM, SMOOTH
    CHARACTER(6) :: FID, PER, STP
    !
    IF( FWEL%SMOOTH == Z        ) RETURN
    IF( FWEL%OUT_SMOOTH%IU == Z ) RETURN
    !
    !
    IF(FWEL%NF == ONE) CALL FWEL%OUT_SMOOTH%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    IU = FWEL%OUT_SMOOTH%IU
    !
    IF(FWEL%NF==ONE .AND. ((KPER==ONE .AND. KSTP==ONE) .OR. FWEL%IOUT==IU) ) THEN
        !
        IF (FWEL%IOUT==IU) WRITE(IU,*)
        !
        CALL FWEL%OUT_SMOOTH%SET_HEADER( ' PER   STP    WBS  WELLID                   LAY   ROW   COL            Q-CAP-INI        Q-CAP            Q-SMF    SMOOTH_FACTOR  MIN_THCK_SMOOTH     CELL-GW-HEAD        CELL-BOTM          SIMTIME     DATE_START' )
    END IF
    !
    IF(FWEL%N == Z) RETURN  ! NO WELLS TO PRINT
    !
    FID = NUM2STR(FWEL%NF)
    FID = ADJUSTL(FID)
    PER = NUM2STR(KPER)
    PER = ADJUSTL(PER)
    STP = NUM2STR(KSTP)
    STP = ADJUSTL(STP)
    !
    DO I=ONE,FWEL%DIM
        IF((FWEL%ACT(I) .OR. FWEL%EXT(I)) .AND. FWEL%Qsmf(I) < 1D0) THEN
              IL = FWEL%LRC(1,I)
              IR = FWEL%LRC(2,I)
              IC = FWEL%LRC(3,I)
              QINI = NUM2STR(FWEL%Qini(I))
              QINI = ADJUSTR(QINI)
              QCAP = NUM2STR(FWEL%Qcap(I))
              QCAP = ADJUSTR(QCAP)
              QFIN = NUM2STR(-1D0*FWEL%Q(I))
              QFIN = ADJUSTR(QFIN)
              QSMF = NUM2STR(FWEL%Qsmf(I))
              QSMF = ADJUSTR(QSMF)
              IF(FWEL%SMOOTH_LIM < -0.5D0) THEN
                    SMOOTH_LIM = FWEL%SMOOTH_LIM_BY_LAYER(IL)   !SMOOTH BY LAYER
              ELSE
                    SMOOTH_LIM = FWEL%SMOOTH_LIM                !USE GLOBAL SMOOTH OR SMOOTH BY FARM
              END IF
              !
              IF (FWEL%SMOOTH == ONE) THEN   ! CELL THICKNESS
                    SMOOTH = NUM2STR (SMOOTH_LIM * ( BOTM(IC, IR, LBOTM(IL)-1) - BOTM(IC, IR, LBOTM(IL)) ) )
              ELSE
                    SMOOTH = NUM2STR (SMOOTH_LIM)
              END IF
              SMOOTH = ADJUSTR(SMOOTH)
              H = NUM2STR(HNEW(IC,IR,IL))
              H = ADJUSTR(H)
              BOT= NUM2STR(BOTM(IC,IR,LBOTM(IL)))
              BOT= ADJUSTR(BOT)
              TIM= NUM2STR(TOTIME)
              TIM= ADJUSTR(TIM)
              WRITE(IU, '(1x 4A, 2x 3I6, 8A, 2x A)') PER, STP, FID, FWEL%WELLID(I), IL, IR, IC, QINI, QCAP, QFIN, QSMF,SMOOTH, H, BOT, TIM, DATE
        END IF
    END DO
    END SUBROUTINE
    !
    IMPURE ELEMENTAL SUBROUTINE PRINT_DETAILS_TO_LIST(FWEL, KPER, KSTP, TOTIME, DATE)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER,     OPTIONAL, INTENT(IN):: KPER, KSTP
    REAL,        OPTIONAL, INTENT(IN):: TOTIME
    CHARACTER(*),OPTIONAL, INTENT(IN):: DATE
    INTEGER:: I, IL, IR, IC
    CHARACTER(17):: QINI, QCAP, QFIN
    CHARACTER(7):: MNWLINK
    CHARACTER(20):: WELLID
    CHARACTER(:),ALLOCATABLE:: LN
    !
    IF(FWEL%NF==ONE) THEN
        IF(PRESENT(KPER)) THEN
            IF(DATE=='   NaN') THEN
                                   LN=' '
            ELSE
                                   LN=' AND START DATE '//DATE
            END IF
            !
            WRITE(FWEL%IOUT,'(/A,I6,A,I6,2x A, ES12.5, 2x A/)') 'FARM WELL INFORMATION FOR STRESS PERIOD',KPER,', TIMESTEP',KSTP,' WITH SIM-TIME OF ',TOTIME,LN
            WRITE(FWEL%IOUT,'( A)') 'FARM  WELLID  LAY ROW COL Q-CAP-INI Q-CAP  Q-FIN  MNWLINK'
        ELSE
            WRITE(FWEL%IOUT,'(/A)') 'FARM  WELLID  LAY ROW COL Q-CAP-INI Q-CAP  MNWLINK'
        END IF
    END IF
    !
    IF(FWEL%N == Z) RETURN  ! NO WELLS TO PRINT
    !
    DO I=ONE,FWEL%DIM
        IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
              WELLID = ADJUSTR(FWEL%WELLID(I))
              IL = FWEL%LRC(1,I)
              IR = FWEL%LRC(2,I)
              IC = FWEL%LRC(3,I)
              !
              IF(FWEL%MNWLOC(I)>Z) THEN
                  MNWLINK='  TRUE'
              ELSE
                  MNWLINK='  FALSE'
              END IF
              !
              QINI = NUM2STR(FWEL%Qini(I))
              QINI = ADJUSTR(QINI)
              QCAP = NUM2STR(FWEL%Qcap(I))
              QCAP = ADJUSTR(QCAP)
              QFIN = NUM2STR(-1D0*FWEL%Q(I))
              QFIN = ADJUSTR(QFIN)
              !
              IF(PRESENT(KPER)) THEN
                WRITE(FWEL%IOUT, '(I6, 3x A, 2x 3I6, 4A)') FWEL%NF, FWEL%WELLID(I), IL, IR, IC, QINI, QCAP, QFIN, MNWLINK
              ELSE
                WRITE(FWEL%IOUT, '(I6, 3x A, 2x 3I6, 3A)') FWEL%NF, FWEL%WELLID(I), IL, IR, IC, QINI, QCAP,       MNWLINK
              END IF
        END IF
    END DO
  END SUBROUTINE
  !
  !IMPURE ELEMENTAL SUBROUTINE PRINT_OUT_BYWELL(FWEL, KPER, KSTP, DELT, DYEAR, DATE)
  !  !NOTE THAT THE CALCULATED ALLOTMENT WILL BE OFF WHEN USING NOCIRNOQ FOR WELLS WITH NOCIR
  !  USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
  !  CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
  !  INTEGER,         INTENT(IN):: KPER, KSTP
  !  DOUBLE PRECISION,INTENT(IN):: DELT
  !  DOUBLE PRECISION,INTENT(IN):: DYEAR
  !  CHARACTER(*),    INTENT(IN):: DATE
  !  INTEGER:: I, J, IL, IR, IC, IU, MNW, EXT_FLG, FIRSTNODE, LASTNODE
  !  DOUBLE PRECISION:: FRAC, SMF, QACT_BIN, QDMD_BIN
  !  CHARACTER(17):: QINI, QALOT, QSMF, QCAP, QFIN, QACT, QDMD, DT
  !  CHARACTER(7):: MNWLINK, EXT
  !  CHARACTER(20):: WELLID
  !  !
  !  IF(FWEL%OUT_BYWELL%IU == Z) RETURN  !NOTHING TO PRINT OUT
  !  !
  !  IF(FWEL%NF == ONE) CALL FWEL%OUT_BYWELL%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
  !  !
  !  IU = FWEL%OUT_BYWELL%IU
  !  !
  !  IF(FWEL%OUT_BYWELL%BINARY) THEN
  !                        IF(FWEL%NF == ONE .AND. KPER==ONE .AND. KSTP==ONE) WRITE(FWEL%IOUT,'(A,/A)')'FARM WELL BY WELL OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:','WELLID (20char), LAY (int), ROW (int), COL (int), MNWLINK (int), EXTERNAL_USE_FLAG (int) DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), FARM ID (int), INITIAL WELL CAPACIY (double), WELL ALLOTTED CAPACITY (double), WELL ALLOTTED AND MNW LIMITTED CAPACITY (double), WELL ALLOT, MNW, AND SMOOTHED CAPACITY (double), FINAL WELL PUMPAGE FROM FMP (double), FINAL WELL PUMPAGE FROM MNW2 [SET TO FMP RATE IF NOT LINKED TO MNW] (double)  '
  !  ELSE
  !                        IF(FWEL%NF==ONE .AND. ((KPER==ONE .AND. KSTP==ONE) .OR. FWEL%IOUT==IU) )  THEN
  !                            !
  !                            IF (FWEL%IOUT==IU) WRITE(IU,*)
  !                            !                                                                                                                                                                                     
  !                            CALL FWEL%OUT_BYWELL%SET_HEADER(  '   KPER   KSTP   FARM   WELLID                MNWLINK EXTERN      Q-CAP-INI      Q-CAP-ALLOT  Q-CAP-ALLOT-MNW        Q-CAP-FIN        Q-FIN-FMP        Q-FIN-MNW        Q-DES-DMD      LAY    ROW    COL             DELT   DYEAR            DATE_START' )
  !                        END IF
  !  END IF
  !  !
  !  IF(FWEL%N > Z) THEN
  !          IF(FWEL%QMAXini > MINSIZE .AND. FWEL%ALLOT > -0.1D0) THEN
  !              FRAC = FWEL%ALLOT / FWEL%QMAXini
  !          ELSE
  !              FRAC = UNO
  !          END IF
  !          !
  !          IF(FWEL%OUT_BYWELL%BINARY) THEN
  !              !
  !              DO I=ONE,FWEL%DIM
  !                  IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
  !                        WELLID = FWEL%WELLID(I)
  !                        IL = FWEL%LRC(1,I)
  !                        IR = FWEL%LRC(2,I)
  !                        IC = FWEL%LRC(3,I)
  !                        J=FWEL%MNWLOC(I)
  !                        !
  !                        IF(J>Z) THEN
  !                            MNW = ONE
  !                            !
  !                            IF(FWEL%Q(I).NE.DZ) THEN
  !                                FIRSTNODE=NINT( MNW2(4,J) )
  !                                LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
  !                                QACT_BIN=DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
  !                            ELSE
  !                                QACT_BIN=DZ
  !                            END IF
  !                        ELSE
  !                                QACT_BIN=DNEG*FWEL%Q(I)
  !                                MNW = Z
  !                        END IF
  !                        IF(FWEL%EXT(I)) THEN
  !                            EXT_FLG = 1
  !                        ELSE
  !                            EXT_FLG = 0
  !                        END IF
  !                        IF(FWEL%SMOOTH>0) THEN
  !                            SMF = FWEL%Qsmf(I)
  !                        ELSE
  !                            SMF = 1D0
  !                        END IF
  !                        !
  !                        IF(FWEL%Q(I) < NEGNEARZERO_6) THEN
  !                            QDMD_BIN = FWEL%DMD(I)
  !                        ELSE
  !                            QDMD_BIN = DZ
  !                        END IF
  !                        !
  !                        WRITE(IU) FWEL%WELLID(I), IL, IR, IC, MNW, EXT_FLG, DATE, DYEAR, DELT, KPER, KSTP, FWEL%NF, FWEL%Qini(I), FWEL%Qini(I)*FRAC, FWEL%Qcap(I), FWEL%Qcap(I)*SMF, DNEG*FWEL%Q(I), QACT_BIN, QDMD_BIN
  !                        !
  !                  END IF
  !              END DO
  !          ELSE
  !              DO I=ONE,FWEL%DIM
  !                  IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
  !                        WELLID = ADJUSTR(FWEL%WELLID(I))
  !                        IL = FWEL%LRC(1,I)
  !                        IR = FWEL%LRC(2,I)
  !                        IC = FWEL%LRC(3,I)
  !                        J=FWEL%MNWLOC(I)
  !                        !
  !                        IF(J>Z) THEN
  !                            MNWLINK='  TRUE'
  !                            !
  !                            IF(FWEL%Q(I).NE.DZ) THEN
  !                                FIRSTNODE=NINT( MNW2(4,J) )
  !                                LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
  !                                QACT=NUM2STR(DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE)))
  !                            ELSE
  !                                QACT='0.0'
  !                            END IF
  !                        ELSE
  !                                QACT='NaN'
  !                                MNWLINK='  FALSE'
  !                        END IF
  !                        IF(FWEL%EXT(I)) THEN
  !                            EXT='  TRUE'
  !                        ELSE
  !                            EXT='  FALSE'
  !                        END IF
  !                        IF(FWEL%SMOOTH>0) THEN
  !                            SMF = FWEL%Qsmf(I)
  !                        ELSE
  !                            SMF = UNO
  !                        END IF
  !                        !
  !                        IF(FWEL%Q(I) < NEGNEARZERO_6) THEN
  !                            QDMD = NUM2STR(FWEL%DMD(I))
  !                        ELSE
  !                            QDMD = '0.0'
  !                        END IF
  !                        QDMD = ADJUSTR(QDMD)
  !                        !
  !                        QINI = NUM2STR(FWEL%Qini(I))
  !                        QINI = ADJUSTR(QINI)
  !                        QALOT= NUM2STR(FWEL%Qini(I)*FRAC)
  !                        QALOT= ADJUSTR(QALOT)
  !                        QCAP = NUM2STR(FWEL%Qcap(I))
  !                        QCAP = ADJUSTR(QCAP)
  !                        QSMF = NUM2STR(FWEL%Qcap(I)*SMF)
  !                        QSMF = ADJUSTR(QSMF)
  !                        QFIN = NUM2STR(DNEG*FWEL%Q(I))
  !                        QFIN = ADJUSTR(QFIN)
  !                        DT = NUM2STR(DELT)
  !                        DT = ADJUSTR(DT)
  !                        QACT=ADJUSTR(QACT)
  !                        !
  !                        WRITE(IU, '(3I7, 3x 10A, 2x 3I7, A, 2x F13.7, 2x A)') KPER, KSTP, FWEL%NF, FWEL%WELLID(I), MNWLINK, EXT, QINI, QALOT, QCAP, QSMF, QFIN, QACT, QDMD, IL, IR, IC, DT, DYEAR, DATE
  !                        !
  !                  END IF
  !              END DO
  !          END IF
  !  END IF
  !END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE PRINT_OUT_BYWELL(FWEL, KPER, KSTP, DELT, DYEAR, DATE)
    !NOTE THAT THE CALCULATED ALLOTMENT WILL BE OFF WHEN USING NOCIRNOQ FOR WELLS WITH NOCIR
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER,         INTENT(IN):: KPER, KSTP
    DOUBLE PRECISION,INTENT(IN):: DELT
    DOUBLE PRECISION,INTENT(IN):: DYEAR
    CHARACTER(*),    INTENT(IN):: DATE
    INTEGER:: I, J, IL, IR, IC, IU, MNW, EXT_FLG, FIRSTNODE, LASTNODE
    DOUBLE PRECISION:: FRAC, SMF, QACT_BIN, QDMD_BIN
    CHARACTER(17):: QINI, QALOT, QSMF, QCAP, QACT, QDMD, DT
    CHARACTER(7):: MNWLINK, EXT
    CHARACTER(20):: WELLID
    !
    IF(FWEL%OUT_BYWELL%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    IF(FWEL%NF == ONE) CALL FWEL%OUT_BYWELL%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = FWEL%OUT_BYWELL%IU
    !
    IF(FWEL%OUT_BYWELL%BINARY) THEN
                          IF(FWEL%NF == ONE .AND. KPER==ONE .AND. KSTP==ONE) WRITE(FWEL%IOUT,'(A,/A)')'FARM WELL BY WELL OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:','WELLID (20char), LAY (int), ROW (int), COL (int), MNWLINK (int), EXTERNAL_USE_FLAG (int) DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), FARM ID (int), INITIAL WELL CAPACIY (double), WELL ALLOTTED CAPACITY (double), WELL ALLOTTED AND MNW LIMITTED CAPACITY (double), WELL ALLOT, MNW, AND SMOOTHED CAPACITY (double), FINAL WELL PUMPAGE FROM FMP (double), FINAL WELL PUMPAGE FROM MNW2 [SET TO FMP RATE IF NOT LINKED TO MNW] (double)  '
    ELSE
                          IF(FWEL%NF==ONE .AND. ((KPER==ONE .AND. KSTP==ONE) .OR. FWEL%IOUT==IU) )  THEN
                              !
                              IF (FWEL%IOUT==IU) WRITE(IU,*)
                              !                                                                                                                                                                                     
                              CALL FWEL%OUT_BYWELL%SET_HEADER(  '    PER    STP    WBS   WELLID                      Q-CAP-INI        Q-CAP-FIN            Q-ACT        Q-DES-DMD      LAY    ROW    COL             DELT   DYEAR            DATE_START' )
                          END IF
    END IF
    !
    IF(FWEL%N > Z) THEN
            IF(FWEL%QMAXini > MINSIZE .AND. FWEL%ALLOT > -0.1D0) THEN
                FRAC = FWEL%ALLOT / FWEL%QMAXini
            ELSE
                FRAC = UNO
            END IF
            !
            IF(FWEL%OUT_BYWELL%BINARY) THEN
                !
                DO I=ONE,FWEL%DIM
                    IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
                          WELLID = FWEL%WELLID(I)
                          IL = FWEL%LRC(1,I)
                          IR = FWEL%LRC(2,I)
                          IC = FWEL%LRC(3,I)
                          J=FWEL%MNWLOC(I)
                          !
                          IF(J>Z) THEN
                              MNW = ONE
                              !
                              IF(FWEL%Q(I).NE.DZ) THEN
                                  FIRSTNODE=NINT( MNW2(4,J) )
                                  LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                                  QACT_BIN=DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
                              ELSE
                                  QACT_BIN=DZ
                              END IF
                          ELSE
                                  QACT_BIN=DNEG*FWEL%Q(I)
                                  MNW = Z
                          END IF
                          IF(FWEL%EXT(I)) THEN
                              EXT_FLG = 1
                          ELSE
                              EXT_FLG = 0
                          END IF
                          IF(FWEL%SMOOTH>0) THEN
                              SMF = FWEL%Qsmf(I)
                          ELSE
                              SMF = 1D0
                          END IF
                          !
                          IF(FWEL%Q(I) < NEGNEARZERO_6) THEN
                              QDMD_BIN = FWEL%DMD(I)
                          ELSE
                              QDMD_BIN = DZ
                          END IF
                          !
                          WRITE(IU) FWEL%WELLID(I), IL, IR, IC, DATE, DYEAR, DELT, KPER, KSTP, FWEL%NF, FWEL%Qini(I), FWEL%Qcap(I)*SMF, QACT_BIN, QDMD_BIN
                          !
                    END IF
                END DO
            ELSE
                DO I=ONE,FWEL%DIM
                    IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
                          WELLID = ADJUSTR(FWEL%WELLID(I))
                          IL = FWEL%LRC(1,I)
                          IR = FWEL%LRC(2,I)
                          IC = FWEL%LRC(3,I)
                          J=FWEL%MNWLOC(I)
                          !
                          IF(J>Z) THEN
                              !MNWLINK='  TRUE'
                              !
                              IF(FWEL%Q(I).NE.DZ) THEN
                                  FIRSTNODE=NINT( MNW2(4,J) )
                                  LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                                  QACT=NUM2STR(DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE)))
                              ELSE
                                  QACT='0.0'
                              END IF
                          ELSE
                                  QACT= NUM2STR(DNEG*FWEL%Q(I))
                                  !MNWLINK='  FALSE'
                          END IF
                          !IF(FWEL%EXT(I)) THEN
                          !    EXT='  TRUE'
                          !ELSE
                          !    EXT='  FALSE'
                          !END IF
                          IF(FWEL%SMOOTH>0) THEN
                              QSMF = NUM2STR(FWEL%Qcap(I)*FWEL%Qsmf(I))
                          ELSE
                              QSMF = NUM2STR(FWEL%Qcap(I))
                          END IF
                          !
                          IF(FWEL%Q(I) < NEGNEARZERO_6) THEN
                              QDMD = NUM2STR(FWEL%DMD(I))
                          ELSE
                              QDMD = '0.0'
                          END IF
                          QDMD = ADJUSTR(QDMD)
                          !
                          QINI = NUM2STR(FWEL%Qini(I))
                          QINI = ADJUSTR(QINI)
                          !QALOT= NUM2STR(FWEL%Qini(I)*FRAC)
                          !QALOT= ADJUSTR(QALOT)
                          !QCAP = NUM2STR(FWEL%Qcap(I))
                          !QCAP = ADJUSTR(QCAP)
                          !QSMF = NUM2STR(FWEL%Qcap(I)*SMF)
                          QSMF = ADJUSTR(QSMF)
                          DT = NUM2STR(DELT)
                          DT = ADJUSTR(DT)
                          QACT=ADJUSTR(QACT)
                          !
                          WRITE(IU, '(3I7, 3x 5A, 2x 3I7, A, 2x F13.7, 2x A)') KPER, KSTP, FWEL%NF, FWEL%WELLID(I), QINI, QSMF, QACT, QDMD, IL, IR, IC, DT, DYEAR, DATE
                          !
                    END IF
                END DO
            END IF
    END IF
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE PRINT_OUT_BYMNW(FWEL, KPER, KSTP, DELT, DYEAR, DATE)
    !NOTE THAT THE CALCULATED ALLOTMENT WILL BE OFF WHEN USING NOCIRNOQ FOR WELLS WITH NOCIR
    USE GLOBAL,        ONLY: HNEW
    USE GWFBASMODULE,  ONLY: HDRY
    USE GWFMNW2MODULE, ONLY: MNW2, MNWNOD
    USE MNW2_FUNCT,    ONLY: MNW2_COMPOSITE_HEAD
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER,         INTENT(IN):: KPER, KSTP
    DOUBLE PRECISION,INTENT(IN):: DELT
    DOUBLE PRECISION,INTENT(IN):: DYEAR
    CHARACTER(*),    INTENT(IN):: DATE
    INTEGER:: I, J, IL, IR, IC, IU, MNW, EXT_FLG, FIRSTNODE, LASTNODE
    DOUBLE PRECISION:: FRAC, SMF, QACT_BIN, QDMD_BIN
    CHARACTER(17):: QINI, QALOT, QACT, QDMD, Hw, Hc, DT
    CHARACTER(20):: WELLID
    !
    IF(FWEL%OUT_BYMNW%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    IF(FWEL%NF == ONE) CALL FWEL%OUT_BYMNW%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = FWEL%OUT_BYMNW%IU
    !
    IF(FWEL%OUT_BYMNW%BINARY) THEN
                          IF(FWEL%NF == ONE .AND. KPER==ONE .AND. KSTP==ONE) WRITE(FWEL%IOUT,'(A,/A)')'FARM WELL BY MNW2 WELL OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:','WELLID (20char), LAY (int), ROW (int), COL (int), MNWLINK (int), EXTERNAL_USE_FLAG (int) DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), FARM ID (int), INITIAL WELL CAPACIY (double), WELL ALLOTTED CAPACITY (double), WELL ALLOTTED AND MNW LIMITTED CAPACITY (double), WELL ALLOT, MNW, AND SMOOTHED CAPACITY (double), FINAL WELL PUMPAGE FROM FMP (double), FINAL WELL PUMPAGE FROM MNW2 [SET TO FMP RATE IF NOT LINKED TO MNW] (double)  '
    ELSE
                          IF(FWEL%NF==ONE .AND. ((KPER==ONE .AND. KSTP==ONE) .OR. FWEL%IOUT==IU) )  THEN
                              !
                              IF (FWEL%IOUT==IU) WRITE(IU,*)
                              !                                                                                                                                                 
                              CALL FWEL%OUT_BYMNW%SET_HEADER(  '    PER    STP    WBS   WELLID                      Q-CAP-INI            Q-ACT        Q-DES-DMD            H-WEL        H-CEL-AVE             DELT   DYEAR            DATE_START' )
                          END IF
    END IF
    !
    IF(FWEL%N > Z) THEN
            IF(FWEL%QMAXini > MINSIZE .AND. FWEL%ALLOT > -0.1D0) THEN
                FRAC = FWEL%ALLOT / FWEL%QMAXini
            ELSE
                FRAC = UNO
            END IF
            !
            IF(FWEL%OUT_BYMNW%BINARY) THEN
                !!!! TODO
                !!!DO I=ONE,FWEL%DIM
                !!!    IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
                !!!          WELLID = FWEL%WELLID(I)
                !!!          IL = FWEL%LRC(1,I)
                !!!          IR = FWEL%LRC(2,I)
                !!!          IC = FWEL%LRC(3,I)
                !!!          J=FWEL%MNWLOC(I)
                !!!          !
                !!!          IF(J == Z) CYCLE
                !!!          !
                !!!          FIRSTNODE=NINT( MNW2(4,J) )
                !!!          LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                !!!          !
                !!!          IF(FWEL%Q(I).NE.DZ) THEN
                !!!              QACT_BIN=DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
                !!!          ELSE
                !!!              QACT_BIN=DZ
                !!!          END IF
                !!!          IF(FWEL%EXT(I)) THEN
                !!!              EXT_FLG = 1
                !!!          ELSE
                !!!              EXT_FLG = 0
                !!!          END IF
                !!!          IF(FWEL%SMOOTH>0) THEN
                !!!              SMF = FWEL%Qsmf(I)
                !!!          ELSE
                !!!              SMF = 1D0
                !!!          END IF
                !!!          !
                !!!          IF(FWEL%Q(I) < NEGNEARZERO_6) THEN
                !!!              QDMD_BIN = FWEL%DMD(I)
                !!!          ELSE
                !!!              QDMD_BIN = DZ
                !!!          END IF
                !!!          !
                !!!          WRITE(IU) FWEL%WELLID(I), IL, IR, IC, MNW, EXT_FLG, DATE, DYEAR, DELT, KPER, KSTP, FWEL%NF, FWEL%Qini(I), FWEL%Qini(I)*FRAC, FWEL%Qcap(I), FWEL%Qcap(I)*SMF, QACT_BIN, QDMD_BIN
                !!!          !
                !!!    END IF
                !!!END DO
            ELSE
                DO I=ONE,FWEL%DIM
                    IF(FWEL%ACT(I) .OR. FWEL%EXT(I)) THEN
                          J=FWEL%MNWLOC(I)
                          IF(J < ONE)  CYCLE
                          !
                          WELLID = ADJUSTR(FWEL%WELLID(I))
                          !
                          Hc = NUM2STR(MNW2_COMPOSITE_HEAD(J, MNW2, MNWNOD, HNEW, HDRY))
                          Hc=ADJUSTR(Hc)
                          !
                          Hw = NUM2STR(mnw2(17,J))
                          Hw=ADJUSTR(Hw)
                          !
                          FIRSTNODE=NINT( MNW2(4,J) )
                          LASTNODE =NINT( MNW2(4,J) + ABS(MNW2(2,J)) - UNO )
                          QACT_BIN = DNEG*SUM(MNWNOD(4,FIRSTNODE:LASTNODE))
                          IF(ABS(QACT_BIN) > NEGNEARZERO_6) THEN
                              QACT=NUM2STR(QACT_BIN)
                          ELSE
                              QACT='0.0'
                          END IF
                          !
                          IF(FWEL%Q(I) < NEGNEARZERO_6) THEN  !Q is stored as negative when pumping occurs
                              QDMD = NUM2STR(FWEL%DMD(I))
                          ELSE
                              QDMD = '0.0'
                          END IF
                          QDMD = ADJUSTR(QDMD)
                          !
                          QINI = NUM2STR(FWEL%Qini(I))
                          QINI = ADJUSTR(QINI)
                          QALOT= NUM2STR(FWEL%Qini(I)*FRAC)
                          QALOT= ADJUSTR(QALOT)
                          DT = NUM2STR(DELT)
                          DT = ADJUSTR(DT)
                          QACT=ADJUSTR(QACT)
                          !
                          WRITE(IU, '(3I7, 3x 6A, A, 2x F13.7, 2x A)') KPER, KSTP, FWEL%NF, FWEL%WELLID(I), QINI, QACT, QDMD, Hw, Hc, DT, DYEAR, DATE
                          !
                    END IF
                END DO
            END IF
    END IF
  END SUBROUTINE
  !
  IMPURE ELEMENTAL SUBROUTINE PRINT_OUT_BYFARM(FWEL, KPER, KSTP, DELT, DYEAR, DATE, TFDR_INI, TFDR)
    CLASS(FARM_WELL_DATA), INTENT(INOUT)::FWEL
    INTEGER,         INTENT(IN):: KPER, KSTP
    DOUBLE PRECISION,INTENT(IN):: DELT
    DOUBLE PRECISION,INTENT(IN):: DYEAR
    CHARACTER(*),    INTENT(IN):: DATE
    DOUBLE PRECISION,INTENT(IN):: TFDR_INI, TFDR
    INTEGER:: IU
    DOUBLE PRECISION::  FRAC
    CHARACTER(17):: QINI, QALOT, QCAP, QMAX, QFIN, DT, TFINI, TF
    CHARACTER(17):: ZER
    !
    IF(FWEL%OUT_BYFARM%IU == Z) RETURN  !NOTHING TO PRINT OUT
    !
    IF(FWEL%NF == ONE) CALL FWEL%OUT_BYFARM%SIZE_CHECK()  !CHECK SIZE EVERY 10 STRESS PERIODS
    !
    IU = FWEL%OUT_BYFARM%IU
    !
    IF(FWEL%OUT_BYFARM%BINARY) THEN
       IF(FWEL%NF == ONE .AND. KPER==ONE .AND. KSTP==ONE) WRITE(FWEL%IOUT,'(A,/A)')'FARM WELL CAPCITIES BY FARM OUTPUT WRITTEN TO BINARY FILE USING STREAM UNFORMATTED STRUCTURE. EACH THE RECORD IN BINARY HAS THE FOLLOWING STRUCTURE:',"DATE_START (19char), DECIMAL YEAR (double), TIME STEP LENGTH (double), STRESS PERIOD (int), TIME STEP (int), FARM ID (int), INITIAL FARM'S CAPACIY (double), FARM'S CAPACITY (double), FINAL FARM'S PUMPAGE (double), INITIAL FARM'S DEMAND (double)"
       !
       IF(FWEL%N > Z) THEN
            WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, FWEL%NF, FWEL%QMAXini, FWEL%QMAX, -1D0*SUM(FWEL%Q, MASK=FWEL%ACT.OR.FWEL%EXT), TFDR_INI, TFDR
       ELSE
            WRITE(IU) DATE, DYEAR, DELT, KPER, KSTP, FWEL%NF, DZ,           DZ,                            DZ,         DZ,         DZ
       END IF
       !
    ELSE
       !
       IF(FWEL%NF==ONE .AND. ((KPER==ONE .AND. KSTP==ONE) .OR. FWEL%IOUT==IU) )  THEN
           !
           IF (FWEL%IOUT==IU) WRITE(IU,*)
           !
           CALL FWEL%OUT_BYFARM%SET_HEADER( '    PER    STP    WBS        Q-CAP-INI      Q-CAP-ALLOT        Q-CAP-FIN            Q-FIN        TFDR-INI          TFDR-FIN             DELT   DYEAR            DATE_START' )
       END IF
       !
       DT = NUM2STR(DELT)
       DT = ADJUSTR(DT)
       IF(FWEL%N > Z) THEN
            IF(FWEL%QMAXini > MINSIZE .AND. FWEL%ALLOT > -0.1D0) THEN
                FRAC = FWEL%ALLOT / FWEL%QMAXini
            ELSE
                FRAC = 1D0
            END IF
            !
            TFINI = NUM2STR(TFDR_INI)
            TFINI = ADJUSTR(TFINI)
            TF = NUM2STR(TFDR)
            TF = ADJUSTR(TF)
            !
            QINI = NUM2STR(FWEL%QMAXini)
            QINI = ADJUSTR(QINI)
            QALOT= NUM2STR(FWEL%QMAXini*FRAC)
            QALOT= ADJUSTR(QALOT)
            QCAP = NUM2STR(SUM(FWEL%Qcap, MASK=FWEL%ACT.OR.FWEL%EXT))
            QCAP = ADJUSTR(QCAP)
            !QMAX = NUM2STR(FWEL%QMAX)
            !QMAX = ADJUSTR(QMAX)
            QFIN = NUM2STR(-1D0*SUM(FWEL%Q, MASK=FWEL%ACT.OR.FWEL%EXT))
            QFIN = ADJUSTR(QFIN)
            WRITE(IU, '(3I7, 7A, 2x F13.7, 2x A)') KPER, KSTP, FWEL%NF, QINI, QALOT, QCAP, QFIN,TFINI,   TF, DT, DYEAR, DATE
       ELSE
            ZER = '              0.0'
            WRITE(IU, '(3I7, 7A, 2x F13.7, 2x A)') KPER, KSTP, FWEL%NF,  ZER,  ZER,   ZER,  ZER,  ZER,  ZER, DT, DYEAR, DATE
       END IF
       !
    END IF
  END SUBROUTINE
    END MODULE
!
!######################################################################################################################
!######################################################################################################################
!######################################################################################################################
!      
    !***SUPERCEDED BY WBS_DATA_FMP_INTERFACE
!!!MODULE FARM_LOCATION_INTERFACE
!!!  IMPLICIT NONE
!!!  PRIVATE
!!!  PUBLIC:: FARMLOCATION
!!!  !
!!!  !TYPE INT_1D_ARRAY
!!!  !    INTEGER,DIMENSION(:),ALLOCATABLE:: V
!!!  !END TYPE
!!!  !
!!!  TYPE FARMLOCATION
!!!    INTEGER:: NF                                                     !--NOT CURRENTLY SUPPORTED, WILL EVENTUALLY HOLD THE FARM ID WHEN THE INDEX IS NO LONGER HARDWIRED AS THE FID
!!!    INTEGER:: Count                                                  ! TOTAL NUMBER OF CELLS THAT CONTRAIN THE FARM [FARMLOCATION(1) REFERS TO FARM 1]
!!!    INTEGER,           DIMENSION(:,:),ALLOCATABLE:: RC               ! LOC(1,:) CONTAINS THE ROW LOCATION OF THE FARM; LOC(2,:) CONTAINS THE COL LOCATION OF THE FARM
!!!    INTEGER,           DIMENSION(:),  ALLOCATABLE:: NONIRR           ! NONIrrigation Flag
!!!    INTEGER,           DIMENSION(:),  ALLOCATABLE:: CRP              ! Crop ID
!!!    LOGICAL,           DIMENSION(:),  ALLOCATABLE:: IRRCRP           ! TRUE IF THERE IS A CROP AND IT IS IRRIGATED
!!!    !TYPE(INT_1D_ARRAY),DIMENSION(:),  ALLOCATABLE:: ISTRM           ! LOC(:)   CONTAINS THE SFR STREAM REACH THAT IS ASSOCAITED WITH THE FARMS ROW/COL IN RC...NOTE IT IS ONE TO ONE MATCHING
!!!    DOUBLE PRECISION:: AREA                                          !FARM TOTAL AREA
!!!    DOUBLE PRECISION:: CRP_AREA                                      !WITHIN FARM AREA OF ICID.NE.0                 (includes fallowed land)
!!!    DOUBLE PRECISION:: IRR_AREA                                      !WITHIN FARM AREA OF ICID.NE.0 .AND. NONIRR=0  (includes fallowed land)
!!!    DOUBLE PRECISION:: IRR_CRP_AREA                                  !WITHIN FARM AREA OF ICID >  0 .AND. NONIRR=0  (only actively grown land that is irrigated)
!!!    CONTAINS
!!!    !
!!!    PROCEDURE, PASS(FMLOC):: INIT      => INITIALIZE_FARM_LOCATIONS  ! INIT(IFID, NF)
!!!    PROCEDURE, PASS(FMLOC):: SET_CROP  => SET_FARM_CROP_ID           ! SET_CROP(FMLOC,ICID,CU)
!!!    PROCEDURE, PASS(FMLOC):: CALC_AREA => FARM_AREA_CALC             ! AREA_INIT(LAND_SURFACE_AREA)
!!!    !
!!!  END TYPE
!!!  !
!!!  CONTAINS
!!!  !
!!!  PURE SUBROUTINE INITIALIZE_FARM_LOCATIONS(FMLOC, IFID, NF)
!!!  CLASS(FARMLOCATION),INTENT(INOUT)::FMLOC
!!!  INTEGER,DIMENSION(:,:), INTENT(IN):: IFID         !FARM LOCATION ARRAY
!!!  INTEGER, INTENT(IN):: NF                          !FARM TO GET LOCATION FROM
!!!  INTEGER:: I, IR, IC
!!!  !
!!!  FMLOC%NF=NF
!!!  FMLOC%Count=COUNT(NF.EQ.IFID)                                 !COUNT THE NUMBER OF CELLS THAT CONTAIN FARM NF
!!!  !
!!!  IF ( FMLOC%Count > 0 ) THEN
!!!    IF (ALLOCATED(FMLOC%RC)) DEALLOCATE(FMLOC%RC)
!!!        ALLOCATE( FMLOC%RC(2,FMLOC%Count) )
!!!    !
!!!    IF (ALLOCATED(FMLOC%CRP)) DEALLOCATE(FMLOC%CRP)
!!!        ALLOCATE( FMLOC%CRP(FMLOC%Count) )
!!!    !
!!!    IF (ALLOCATED(FMLOC%NONIRR)) DEALLOCATE(FMLOC%NONIRR)
!!!        ALLOCATE( FMLOC%NONIRR(FMLOC%Count) )
!!!    !
!!!    IF (ALLOCATED(FMLOC%IRRCRP)) DEALLOCATE(FMLOC%IRRCRP)
!!!        ALLOCATE( FMLOC%IRRCRP(FMLOC%Count) )
!!!    !
!!!    I=0                                                             !COUNTER FOR NUMBER OR RC READS
!!!    DO IR=1, UBOUND(IFID,2)                                         !SEARCH FOR FARM NF IN MODEL AND STORE ITS ROW/COL LOCATION  ==> IFID(NCOL,NROW)
!!!    DO IC=1, UBOUND(IFID,1)
!!!      IF(IFID(IC,IR).EQ.NF) THEN
!!!       I=I+1
!!!       FMLOC%RC(1,I)=IR
!!!       FMLOC%RC(2,I)=IC
!!!      END IF
!!!    END DO
!!!    END DO
!!!  END IF
!!!  !
!!!  END SUBROUTINE
!!!  !
!!!  !PURE SUBROUTINE INITIALIZE_SFR_LOCATIONS(FMLOC, ISTRM)
!!!  !CLASS(FARMLOCATION),INTENT(INOUT)::FMLOC
!!!  !INTEGER,DIMENSION(:,:), INTENT(IN):: ISTRM        !SFR ISTRM ARRAY
!!!  !INTEGER, DIMENSION(:), ALLOCATABLE:: STRM1, STRM2
!!!  !INTEGER:: I, L, N, IR, IC
!!!  !!
!!!  !IF(.NOT.ALLOCATED(FMLOC%ISTRM)) ALLOCATE(FMLOC%ISTRM(FMLOC%Count))
!!!  !!
!!!  !N = UBOUND(ISTRM,2)
!!!  !DO I=1,FMLOC%Count                                            !seb removed double do loop
!!!  !  IR=FMLOC%RC(1,I)
!!!  !  IC=FMLOC%RC(2,I)
!!!  !  ALLOCATE(STRM1(1))
!!!  !  !
!!!  !  DO L=1, N
!!!  !    IF( ISTRM(2,L).EQ.IR .AND. ISTRM(3,L).EQ.IC ) THEN
!!!  !        IF(  ALLOCATED(STRM1) ) THEN
!!!  !              ALLOCATE(STRM2, SOURCE=[STRM1,L])
!!!  !            DEALLOCATE(STRM1)
!!!  !        ELSE
!!!  !              ALLOCATE(STRM1, SOURCE=[STRM2,L])
!!!  !            DEALLOCATE(STRM2)
!!!  !        END IF
!!!  !    END IF
!!!  !  END DO
!!!  !  !
!!!  !  IF(  ALLOCATED(STRM1) ) THEN
!!!  !    IF(SIZE(STRM1)>1) ALLOCATE(FMLOC%ISTRM(I)%V, SOURCE=STRM1(2:))
!!!  !    DEALLOCATE(STRM1)
!!!  !  ELSE
!!!  !    IF(SIZE(STRM2)>1) ALLOCATE(FMLOC%ISTRM(I)%V, SOURCE=STRM2(2:))
!!!  !    DEALLOCATE(STRM2)
!!!  !  END IF
!!!  !  !
!!!  !END DO
!!!  !!
!!!  !END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE SET_FARM_CROP_ID(FMLOC,ICID,CU) !DELR,DELC
!!!    CLASS(FARMLOCATION),     INTENT(INOUT)::FMLOC
!!!    INTEGER,          DIMENSION(:,:), CONTIGUOUS, INTENT(IN):: ICID
!!!    DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(IN):: CU
!!!    INTEGER:: I, IR, IC
!!!    !
!!!    DO I=1,FMLOC%Count                                            !seb removed double do loop
!!!      IR=FMLOC%RC(1,I)
!!!      IC=FMLOC%RC(2,I)
!!!      FMLOC%CRP(I)   = ICID(IC,IR)
!!!      !
!!!      IF     (ICID(IC,IR) >  0) THEN
!!!                                    FMLOC%NONIRR(I) = INT(CU(3,ICID(IC,IR)))
!!!      ELSEIF (ICID(IC,IR) == 0) THEN
!!!                                    FMLOC%NONIRR(I) = 1
!!!      ELSE
!!!                                    FMLOC%NONIRR(I) = 0
!!!      END IF
!!!      !
!!!      IF (FMLOC%CRP(I) > 0 .AND. FMLOC%NONIRR(I) == 0) THEN
!!!          FMLOC%IRRCRP(I) = .TRUE.
!!!      ELSE
!!!          FMLOC%IRRCRP(I) = .FALSE.
!!!      END IF
!!!    END DO
!!!    !
!!!  END SUBROUTINE
!!!  !
!!!  PURE SUBROUTINE FARM_AREA_CALC(FMLOC,LAND_SURFACE_AREA) !DELR,DELC
!!!  CLASS(FARMLOCATION),     INTENT(INOUT)::FMLOC
!!!  DOUBLE PRECISION, DIMENSION(:,:), CONTIGUOUS, INTENT(IN):: LAND_SURFACE_AREA
!!!  !REAL,             DIMENSION(:),   INTENT(IN):: DELR, DELC
!!!  !
!!!  INTEGER:: I, IR, IC
!!!  DOUBLE PRECISION:: AREA, Z
!!!  !
!!!  Z = 0D0
!!!  FMLOC%AREA     = Z
!!!  FMLOC%CRP_AREA = Z
!!!  FMLOC%IRR_AREA = Z
!!!  FMLOC%IRR_CRP_AREA = Z
!!!  DO I=1,FMLOC%Count                                            !seb removed double do loop
!!!    IR=FMLOC%RC(1,I)
!!!    IC=FMLOC%RC(2,I)
!!!    AREA = LAND_SURFACE_AREA(IC,IR) !DBLE(DELR(IC)*DELC(IR))
!!!    !
!!!    FMLOC%AREA = FMLOC%AREA + AREA
!!!    !
!!!    IF( FMLOC%CRP(I).NE.0 ) THEN
!!!        FMLOC%CRP_AREA = FMLOC%CRP_AREA + AREA
!!!        !
!!!        IF( FMLOC%NONIRR(I).EQ.0 ) FMLOC%IRR_AREA = FMLOC%IRR_AREA + AREA   !NOTE AREA INCLUDES FALLOWED LAND WHICH IS ASSUMED TO BE IRRIGATED BUT HAS NO TDR
!!!    END IF
!!!    !
!!!    IF( FMLOC%CRP(I) > 0 .AND. FMLOC%NONIRR(I).EQ.0 ) FMLOC%IRR_CRP_AREA = FMLOC%IRR_CRP_AREA + AREA  !AREA CONTAINS CROP AND HAS APPLIED IRRIGATION 
!!!  ENDDO
!!!  !
!!!  END SUBROUTINE
!!!END MODULE
!
!######################################################################################################################
!######################################################################################################################
!######################################################################################################################
!
!MODULE FARM_HIERARCHY_INTERFACE  --OLD VERSION
!  IMPLICIT NONE
!  PRIVATE
!  !
!  TYPE FARM_RELATIONSHIP
!      INTEGER:: PROJ=0
!      INTEGER:: DIST=0
!      INTEGER:: UNIT=0
!  END TYPE
!  !
!  TYPE UNIT_RELATIONSHIP
!      INTEGER:: N=0
!      INTEGER,DIMENSION(:),ALLOCATABLE:: F
!  END TYPE
!  !
!  TYPE DIST_RELATIONSHIP
!      INTEGER:: N=0
!      INTEGER,DIMENSION(:),ALLOCATABLE:: U
!  END TYPE
!  !
!  TYPE PROJ_RELATIONSHIP
!      INTEGER:: N=0
!      INTEGER,DIMENSION(:),ALLOCATABLE:: D
!  END TYPE
!  !
!  TYPE FARM_HIERARCHY
!      !
!      INTEGER:: NFARM, NPROJ, NDIST, NUNIT
!      !
!      TYPE (FARM_RELATIONSHIP),DIMENSION(:), ALLOCATABLE:: F
!      !
!      TYPE (PROJ_RELATIONSHIP),DIMENSION(:), ALLOCATABLE:: P
!      TYPE (DIST_RELATIONSHIP),DIMENSION(:), ALLOCATABLE:: D
!      TYPE (UNIT_RELATIONSHIP),DIMENSION(:), ALLOCATABLE:: U
!      !
!      CONTAINS
!      PROCEDURE, PASS(FH):: INIT => INITIALIZE_HIERARCHY     !(NFARMS,NPROJ,NDIST,NUNIT,IDX)
!      PROCEDURE, PASS(FH):: SETUP=> FARM_HIERARCHY_POINTERS  !(IDX)
!  END TYPE
!  !
!  CONTAINS
!  !
!  PURE SUBROUTINE INITIALIZE_HIERARCHY(FH, NFARM, NPROJ, NDIST, NUNIT, IDX)
!  CLASS(FARM_HIERARCHY),  INTENT(INOUT):: FH
!  INTEGER,                INTENT(IN   ):: NFARM, NPROJ, NDIST, NUNIT
!  INTEGER,DIMENSION(:,:), INTENT(IN   ), OPTIONAL:: IDX
!  !
!  FH%NFARM = NFARM
!  FH%NPROJ = NPROJ
!  FH%NDIST = NDIST
!  FH%NUNIT = NUNIT
!  !
!  ALLOCATE( FH%F(NFARM) )
!  !
!  ALLOCATE( FH%P(NPROJ) )
!  ALLOCATE( FH%D(NDIST) )
!  ALLOCATE( FH%U(NUNIT) )
!  !
!  IF(PRESENT(IDX)) CALL FARM_HIERARCHY_POINTERS(FH, IDX)
!  !
!  END SUBROUTINE
!  !
!  PURE SUBROUTINE FARM_HIERARCHY_POINTERS(FH, IDX)
!  CLASS(FARM_HIERARCHY),  INTENT(INOUT):: FH
!  INTEGER,DIMENSION(:,:), INTENT(IN   ):: IDX
!  !
!  INTEGER:: I, J, N
!  INTEGER, DIMENSION(:), ALLOCATABLE:: TEMP
!  !
!  N=MAX(FH%NPROJ, FH%NDIST, FH%NUNIT, FH%NFARM)
!  ALLOCATE( TEMP(N) )   !TEMPORARY STORAGE BASED ON MAXIMUM NUMBER OF PROJ
!  !
!  DO I=1, FH%NFARM
!      FH%F(I)%PROJ = IDX(I,2)
!      FH%F(I)%DIST = IDX(I,3)
!      FH%F(I)%UNIT = IDX(I,4)    
!  END DO
!  !
!  DO J=1, FH%NPROJ
!    TEMP=0
!    N=0
!    DO I=1, FH%NFARM
!      IF(FH%F(I)%PROJ==J) THEN
!          IF( ALL(FH%F(I)%DIST/=TEMP) ) THEN
!              N=N+1
!              TEMP(N)=FH%F(I)%DIST
!          END IF
!      END IF
!    END DO
!    IF (N>0) THEN
!        FH%P(J)%N=N
!        ALLOCATE( FH%P(J)%D(N), SOURCE=TEMP(:N) )
!        CALL SORT_INT_ARRAY(N,FH%P(J)%D)
!    END IF 
!  END DO
!  !
!  DO J=1, FH%NDIST
!    TEMP=0
!    N=0
!    DO I=1, FH%NFARM
!      IF(FH%F(I)%DIST==J) THEN
!          IF( ALL(FH%F(I)%UNIT/=TEMP) ) THEN
!              N=N+1
!              TEMP(N)=FH%F(I)%UNIT
!          END IF
!      END IF
!    END DO
!    IF (N>0) THEN
!        FH%D(J)%N=N
!        ALLOCATE( FH%D(J)%U(N), SOURCE=TEMP(:N) )
!        CALL SORT_INT_ARRAY(N,FH%D(J)%U)
!    END IF 
!  END DO
!  !
!  DO J=1, FH%NUNIT
!    N = COUNT( J==IDX(:,4) )
!    IF (N>0) THEN
!        FH%U(J)%N=N
!        ALLOCATE( FH%U(J)%F(N), SOURCE=0 )
!        N=0
!        DO I=1, FH%NFARM
!          IF(FH%F(I)%UNIT==J) THEN
!             N=N+1
!             FH%U(J)%F(N)=I
!          END IF
!        END DO
!    END IF 
!  END DO
!  !
!  END SUBROUTINE
!  !
!  PURE SUBROUTINE SORT_INT_ARRAY(N,ARR)
!  INTEGER,              INTENT(IN):: N
!  INTEGER, DIMENSION(N),INTENT(INOUT):: ARR
!  INTEGER:: I, J, VAL
!  !
!  DO I=2, N
!      VAL=ARR(I)
!      J=I
!      DO WHILE ( J > 1 )
!                         IF (ARR(J-1) < VAL) EXIT
!                         ARR(J) = ARR(J-1)
!                         J=J-1
!      END DO
!      ARR(J) = VAL
!  END DO
!  !
!  END SUBROUTINE
!  !
!    END MODULE
!
!######################################################################################################################
!######################################################################################################################
!######################################################################################################################
!

!MODULE OFE_INTERFACE
!  USE GLOBAL,                           ONLY: LSTCHK
!  USE UTIL_INTERFACE,                         ONLY: NUM2STR
!  USE UTIL_INTERFACE,                      ONLY: READ_TO_DATA, FILE_IO_ERROR, STOP_ERROR
!  USE GENERIC_BLOCK_READER_INSTRUCTION, ONLY: GENERIC_BLOCK_READER
!  USE GENERIC_OUTPUT_FILE_INSTRUCTION,    ONLY: GENERIC_OUTPUT_FILE
!  USE GENERIC_INPUT_FILE_INSTRUCTION,     ONLY: GENERIC_INPUT_FILE
!   IMPLICIT NONE
!   TYPE OFE_DATA
!        DOUBLE PRECISION,DIMENSION(:,:),ALLOCATABLE:: ARR
!   END TYPE
!   CONTAINS
!    
!   
!END MODULE
    