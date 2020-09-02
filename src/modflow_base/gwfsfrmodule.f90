      MODULE GWFSFRMODULE
        USE TABLEFILE_INTERFACE,              ONLY:TABFILETYPE1IDX                    !seb
        USE LINE_FEEDER,                      ONLY: LINE_FEED
        USE GENERIC_OUTPUT_FILE_INSTRUCTION,  ONLY: GENERIC_OUTPUT_FILE
        USE TIME_SERIES_INSTRUCTION,          ONLY: TIME_SERIES_FILE_GROUP
        USE WARNING_TYPE_INSTRUCTION,         ONLY: WARNING_TYPE
        !USE BUDGET_GROUP_INTERFACE,           ONLY: BUDGET_GROUP
        !
        PRIVATE:: TABFILETYPE1IDX, LINE_FEED, GENERIC_OUTPUT_FILE, WARNING_TYPE
        !
        LOGICAL, SAVE:: SFR_IN_USE = .FALSE.
        !
        CHARACTER(LEN=64), SAVE:: Version_sfr
        DOUBLE PRECISION, PARAMETER:: NEARZERO=1.0D-30
        REAL,             PARAMETER:: CLOSEZERO=1.0E-15
        DOUBLE PRECISION, SAVE,                    POINTER:: HD_RELAX
        INTEGER,          SAVE,                    POINTER :: STRHC1KHFLAG, STRHC1KVFLAG
        DOUBLE PRECISION, SAVE,                    POINTER:: THETAB, FLUXB, FLUXHLD2    !seb added POINTER
        INTEGER,          SAVE,                    POINTER:: Nfoldflbt, NUMTAB, ROWTAB  !seb added POINTER
        INTEGER,SAVE,                              POINTER:: NFLOWTYPE
        CHARACTER*16,     SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: FLOWTYPE
        INTEGER,          SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: DVRCH   !(diverted recharge flag; then reharge cell count)
        INTEGER,          SAVE,  DIMENSION(:,:,:), POINTER, CONTIGUOUS:: DVRCELL !(store cells to apply diverted recharge)
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: RECHSAVE  !(store original recharge values)
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: DVRPERC  !(Percentage of diversion applied to each cell)
        REAL,             SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: DVEFF  !(store efficiency factor)
        INTEGER,          SAVE,                    POINTER:: NSS, NSTRM, NSFRPAR, ISTCB1, ISTCB2
        INTEGER,          SAVE,                    POINTER:: IUZT, MAXPTS, IRTFLG, NUMTIM, NSEGDIM
        INTEGER,          SAVE,                    POINTER:: ISFROPT, NSTRAIL, ISUZN, NSFRSETS
        INTEGER,          SAVE,                    POINTER:: NUZST, NSTOTRL, NUMAVE, NSFRAUX
        INTEGER,          SAVE,                    POINTER:: ITMP, IRDFLG, IPTFLG, NP
        REAL,             SAVE,                    POINTER:: CONST, DLEAK, WEIGHT, SFRRATIN, SFRRATOUT
        REAL,             SAVE,                    POINTER:: FLWTOL, STRMDELSTOR_CUM, STRMDELSTOR_RATE
        REAL,             SAVE,                    POINTER:: FACTORKH,FACTORKV
        REAL,             SAVE,                    POINTER:: FACTOR, SFRUZINFIL,SFRUZDELSTOR,SFRUZRECH
        DOUBLE PRECISION, SAVE,                    POINTER:: TOTSPFLOW
        INTEGER,          SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: IOTSG, NSEGCK
        INTEGER,          SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: ITRLSTH
        INTEGER,          SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: ISEG, IDIVAR, ISTRM
        INTEGER,          SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: LTRLIT, LTRLST
        INTEGER,          SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: ITRLIT, ITRLST, NWAVST
        REAL,             SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: STRIN, STROUT, FXLKOT
        REAL,             SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: UHC, SGOTFLW, DVRSFLW
        REAL,             SAVE,  DIMENSION(:),     POINTER, CONTIGUOUS:: SFRUZBD
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: SEG, STRM,  SFRQ
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: HWDTH, HWTPRM
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: QSTAGE, XSEC
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: AVDPT, AVWAT, WAT1
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: CONCQ, CONCRUN, CONCPPT
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: TABFLOW, TABTIME   !Reading Spedified inflow
        REAL,             SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: FNETSEEP           !writing net seepage in UZF
        INTEGER,          SAVE,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: ISFRLIST           !Reading Spedified inflow
        INTEGER,          SAVE,                    POINTER:: NINTOT,ITRFLG      !for LMT, total # of possible inflows edm 7/30/13
        DOUBLE PRECISION, SAVE, DIMENSION(:),      POINTER, CONTIGUOUS:: THTS, THTR, EPS
        DOUBLE PRECISION, SAVE, DIMENSION(:),      POINTER, CONTIGUOUS:: FOLDFLBT, THTI
        DOUBLE PRECISION, SAVE, DIMENSION(:),      POINTER, CONTIGUOUS:: SUMLEAK, SUMRCH
        DOUBLE PRECISION, SAVE, DIMENSION(:),      POINTER, CONTIGUOUS:: HLDSFR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZFLWT, UZSTOR
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZWDTH, UZSEEP
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: DELSTOR, WETPER
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZDPIT, UZDPST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZTHIT, UZTHST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZSPIT, UZSPST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZFLIT, UZFLST
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: UZOLSFLX, HSTRM
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: QSTRM, SLKOTFLW
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: DLKOTFLW, DLKSTAGE
        DOUBLE PRECISION, SAVE, DIMENSION(:,:),    POINTER, CONTIGUOUS:: FINF, RECH
        LOGICAL,          SAVE,                    POINTER:: SWO_ENABLED
        LOGICAL,          SAVE,                    POINTER:: DO_FM_BD
        LOGICAL,          SAVE,                    POINTER:: SFR_FIX_BOT
        LOGICAL,          SAVE,                    POINTER:: SFR_AUTO_NEG_ITMP
        TYPE(WARNING_TYPE),                  SAVE, POINTER:: CNVG_WRN
        !
        CHARACTER(LEN=16), SAVE, DIMENSION(:),     POINTER, CONTIGUOUS:: SFRAUX
        INTEGER,                   SAVE,           POINTER:: IOUT, LOUT
        TYPE(TABFILETYPE1IDX),     SAVE,           POINTER:: SFRTABFILE                 !seb ADDED TABFILE SUPPORT FROM TABFILE_INTERFACE
        TYPE(LINE_FEED),           SAVE,           POINTER:: SFRFEED                    !seb ADDED LINEFEED SUPPORT FROM LINE_FEEDER
        TYPE(GENERIC_OUTPUT_FILE), SAVE,           POINTER:: DBFILE
        TYPE(TIME_SERIES_FILE_GROUP),SAVE,         POINTER:: TIME_SERIES
        INTEGER,    DIMENSION(:),  SAVE,           POINTER, CONTIGUOUS:: SEG_NSTRM
        LOGICAL,                   SAVE,           POINTER:: SFR_PRNT
        !
        INTEGER,                        POINTER,SAVE::SFR_FRES_OUTER
        INTEGER,                        POINTER,SAVE::SFR_FRES_NTERM
        TYPE(GENERIC_OUTPUT_FILE),      POINTER,SAVE::SFR_FRES
        INTEGER,DIMENSION(:),           POINTER,SAVE, CONTIGUOUS:: SFR_FRES_LRC
        DOUBLE PRECISION,DIMENSION(:),  POINTER,SAVE, CONTIGUOUS:: SFR_FRES_DIF
         
      TYPE GWFSFRTYPE
        DOUBLE PRECISION,                    POINTER:: HD_RELAX
        INTEGER,                             POINTER :: STRHC1KHFLAG, STRHC1KVFLAG
        DOUBLE PRECISION,                    POINTER:: THETAB, FLUXB, FLUXHLD2    !seb added POINTER
        INTEGER,                             POINTER:: Nfoldflbt, NUMTAB, ROWTAB  !seb added POINTER
        INTEGER,                             POINTER:: NFLOWTYPE
        CHARACTER*16,      DIMENSION(:),     POINTER, CONTIGUOUS:: FLOWTYPE
        INTEGER,           DIMENSION(:),     POINTER, CONTIGUOUS:: DVRCH      !Diversions to recharge
        INTEGER,           DIMENSION(:,:,:), POINTER, CONTIGUOUS:: DVRCELL  !Diversions to recharge
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: RECHSAVE  !Diversions to recharge
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: DVRPERC  !Diversions to recharge
        REAL,              DIMENSION(:),     POINTER, CONTIGUOUS:: DVEFF  !Diversions to recharge
        INTEGER,                             POINTER:: NSS, NSTRM, NSFRPAR, ISTCB1, ISTCB2
        INTEGER,                             POINTER:: IUZT, MAXPTS, IRTFLG, NUMTIM, NSEGDIM
        INTEGER,                             POINTER:: ISFROPT, NSTRAIL, ISUZN, NSFRSETS
        INTEGER,                             POINTER:: NUZST, NSTOTRL, NUMAVE, NSFRAUX
        INTEGER,                             POINTER:: ITMP, IRDFLG, IPTFLG, NP
        REAL,                                POINTER:: CONST, DLEAK, WEIGHT, SFRRATIN, SFRRATOUT
        REAL,                                POINTER:: FLWTOL, STRMDELSTOR_CUM, STRMDELSTOR_RATE
        REAL,                                POINTER:: FACTORKH,FACTORKV
        REAL,                                POINTER:: FACTOR, SFRUZINFIL,SFRUZDELSTOR,SFRUZRECH
        DOUBLE PRECISION,                    POINTER:: TOTSPFLOW
        INTEGER,           DIMENSION(:),     POINTER, CONTIGUOUS:: IOTSG, NSEGCK
        INTEGER,           DIMENSION(:),     POINTER, CONTIGUOUS:: ITRLSTH
        INTEGER,           DIMENSION(:,:),   POINTER, CONTIGUOUS:: ISEG, IDIVAR, ISTRM
        INTEGER,           DIMENSION(:,:),   POINTER, CONTIGUOUS:: LTRLIT, LTRLST
        INTEGER,           DIMENSION(:,:),   POINTER, CONTIGUOUS:: ITRLIT, ITRLST, NWAVST
        REAL,              DIMENSION(:),     POINTER, CONTIGUOUS:: STRIN, STROUT, FXLKOT
        REAL,              DIMENSION(:),     POINTER, CONTIGUOUS:: UHC, SGOTFLW, DVRSFLW
        REAL,              DIMENSION(:),     POINTER, CONTIGUOUS:: SFRUZBD
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: SEG, STRM, SFRQ
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: HWDTH, HWTPRM
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: QSTAGE, XSEC
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: AVDPT, AVWAT, WAT1
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: CONCQ, CONCRUN, CONCPPT
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: TABFLOW, TABTIME  ! Reading SPecified inflow
        REAL,              DIMENSION(:,:),   POINTER, CONTIGUOUS:: FNETSEEP          !writing net seepage in UZF
        INTEGER,           DIMENSION(:,:),   POINTER, CONTIGUOUS:: ISFRLIST
        INTEGER,                             POINTER:: NINTOT,ITRFLG      !for LMT, total # of possible inflows edm 7/30/13
        DOUBLE PRECISION,  DIMENSION(:),     POINTER, CONTIGUOUS:: THTS, THTR, EPS
        DOUBLE PRECISION,  DIMENSION(:),     POINTER, CONTIGUOUS:: FOLDFLBT, THTI
        DOUBLE PRECISION,  DIMENSION(:),     POINTER, CONTIGUOUS:: SUMLEAK, SUMRCH
        DOUBLE PRECISION,  DIMENSION(:),     POINTER, CONTIGUOUS:: HLDSFR
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZFLWT, UZSTOR
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZWDTH, UZSEEP
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: DELSTOR, WETPER
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZDPIT, UZDPST
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZTHIT, UZTHST
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZSPIT, UZSPST
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZFLIT, UZFLST
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: UZOLSFLX, HSTRM
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: QSTRM, SLKOTFLW
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: DLKOTFLW, DLKSTAGE
        DOUBLE PRECISION,  DIMENSION(:,:),   POINTER, CONTIGUOUS:: FINF, RECH
        LOGICAL,                             POINTER:: SWO_ENABLED
        LOGICAL,                             POINTER:: DO_FM_BD
        LOGICAL,                             POINTER:: SFR_FIX_BOT
        LOGICAL,                             POINTER:: SFR_AUTO_NEG_ITMP
        TYPE(WARNING_TYPE),                  POINTER:: CNVG_WRN
        !
        CHARACTER(LEN=16), DIMENSION(:),     POINTER, CONTIGUOUS:: SFRAUX
        INTEGER,                             POINTER:: IOUT, LOUT
        TYPE(TABFILETYPE1IDX),               POINTER:: SFRTABFILE                 !seb ADDED TABFILE SUPPORT FROM TABFILE_INTERFACE
        TYPE(LINE_FEED),                     POINTER:: SFRFEED                    !seb ADDED LINEFEED SUPPORT FROM LINE_FEEDER
        TYPE(GENERIC_OUTPUT_FILE),           POINTER:: DBFILE
        TYPE(TIME_SERIES_FILE_GROUP),        POINTER:: TIME_SERIES
        INTEGER,    DIMENSION(:),            POINTER, CONTIGUOUS:: SEG_NSTRM
        LOGICAL,                             POINTER:: SFR_PRNT
        !
        INTEGER,                        POINTER::SFR_FRES_OUTER
        INTEGER,                        POINTER::SFR_FRES_NTERM
        TYPE(GENERIC_OUTPUT_FILE),      POINTER::SFR_FRES
        INTEGER,DIMENSION(:),           POINTER, CONTIGUOUS:: SFR_FRES_LRC
        DOUBLE PRECISION,DIMENSION(:),  POINTER, CONTIGUOUS:: SFR_FRES_DIF
      END TYPE
      TYPE(GWFSFRTYPE), SAVE:: GWFSFRDAT(10)
      END MODULE GWFSFRMODULE
