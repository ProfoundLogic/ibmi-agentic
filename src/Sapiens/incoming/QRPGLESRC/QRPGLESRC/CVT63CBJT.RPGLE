     H*  (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 1998
     H*
     H/TITLE CVT63CBJT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    CREATE A TABLE ENTRY FOR ALL THE NEW COMPANIES CREATED FOR *
     H*    PCMS                                                       *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  04/28/06 C:168  PROJECT:63CBJ                                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F*CTCEP   IF  E           K        DISK                      A
     F*CTPRP   IF  E           K        DISK                      A
     F*CTCRP   IF  E           K        DISK                      A
     F*CTPSP   IF  E           K        DISK                      A
     F*CMPDL   IF  E           K        DISK                      A
     F*CTCVP   IF  E           K        DISK                      A
     F*CTRCP   IF  E           K        DISK                      A
     F*CMSLL   IF  E           K        DISK                      A
     F*CTCYP   IF  E           K        DISK                      A
     F*CTRTP   IF  E           K        DISK                      A
     F*CTACP   IF  E           K        DISK                      A
     F*CTDEP   IF  E           K        DISK                      A
     F*CTSPP   IF  E           K        DISK                      A
     F*CTAPP   IF  E           K        DISK                      A
     F*CTDSP   IF  E           K        DISK                      A
     F*CTTRP   IF  E           K        DISK                      A
     F*CTATP   IF  E           K        DISK                      A
     F*CTIAP   IF  E           K        DISK                      A
     F*CTTTP   IF  E           K        DISK                      A
     F*CTBCP   IF  E           K        DISK                      A
     F*CTNRP   IF  E           K        DISK                      A
     F*CTPCP   IF  E           K        DISK                      A
     FCMCCL     IF A E           K DISK
     FCMGCL     IF A E           K DISK
     FCTCCL     IF A E           K DISK
     F                                     RENAME(PCC01:PCC02)
     F                                     PREFIX(X)
     FCTABL     IF A E           K DISK
     F*
     F*
     C     *LIKE         DEFINE    CCCO#         XXCO#
     C                   EVAL      XXCO#=393
     C*
     C*
     C*  File ( CMCCP )
     C*
     C     XXCO#         CHAIN     PCC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      CCCO#=7
     C                   WRITE     PCC01
     C                   EVAL      CCCO#=9
     C                   WRITE     PCC01
     C                   EVAL      CCCO#=10
     C                   WRITE     PCC01
     C                   EVAL      CCCO#=13
     C                   WRITE     PCC01
     C     XXCO#         READE     PCC01                                  99
     C                   ENDDO
     C*
     C*  File ( CTCEP )
     C*
     C     XXCO#         CHAIN     PCE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      CECO#=7
     C                   WRITE     PCE01
     C                   EVAL      CECO#=9
     C                   WRITE     PCE01
     C                   EVAL      CECO#=10
     C                   WRITE     PCE01
     C                   EVAL      CECO#=13
     C                   WRITE     PCE01
     C     XXCO#         READE     PCE01                                  99
     C                   ENDDO
     C*
     C*  File ( CTPRP )
     C*
     C     XXCO#         CHAIN     PPR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      PRCO#=7
     C                   WRITE     PPR01
     C                   EVAL      PRCO#=9
     C                   WRITE     PPR01
     C                   EVAL      PRCO#=10
     C                   WRITE     PPR01
     C                   EVAL      PRCO#=13
     C                   WRITE     PPR01
     C     XXCO#         READE     PPR01                                  99
     C                   ENDDO
     C*
     C*  File ( CMGCP )
     C*
     C     XXCO#         CHAIN     PGC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      GCCO#=7
     C                   WRITE     PGC01
     C                   EVAL      GCCO#=9
     C                   WRITE     PGC01
     C                   EVAL      GCCO#=10
     C                   WRITE     PGC01
     C                   EVAL      GCCO#=13
     C                   WRITE     PGC01
     C     XXCO#         READE     PGC01                                  99
     C                   ENDDO
     C*
     C*  File ( CTCRP )
     C*
     C     XXCO#         CHAIN     PCR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      CRCO#=7
     C                   WRITE     PCR01
     C                   EVAL      CRCO#=9
     C                   WRITE     PCR01
     C                   EVAL      CRCO#=10
     C                   WRITE     PCR01
     C                   EVAL      CRCO#=13
     C                   WRITE     PCR01
     C     XXCO#         READE     PCR01                                  99
     C                   ENDDO
     C*
     C*  File ( CTPSP )
     C*
     C     XXCO#         CHAIN     PPS01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      PSCO#=7
     C                   WRITE     PPS01
     C                   EVAL      PSCO#=9
     C                   WRITE     PPS01
     C                   EVAL      PSCO#=10
     C                   WRITE     PPS01
     C                   EVAL      PSCO#=13
     C                   WRITE     PPS01
     C     XXCO#         READE     PPS01                                  99
     C                   ENDDO
     C*
     C*  File ( CMPDP )
     C*
     C*          XXCO#     CHAINPPD01                99
     C*          *IN99     DOWEQ*OFF
     C*                    EVAL 7         PDCO#
     C*                    WRITEPPD01
     C*                    EVAL 9         PDCO#
     C*                    WRITEPPD01
     C*                    EVAL 10        PDCO#
     C*                    WRITEPPD01
     C*                    EVAL 13        PDCO#
     C*                    WRITEPPD01
     C*          XXCO#     READEPPD01                    99
     C*                    ENDDO
     C*
     C*  File ( CTCVP )
     C*
     C     XXCO#         CHAIN     PCV01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      CVCO#=7
     C                   WRITE     PCV01
     C                   EVAL      CVCO#=9
     C                   WRITE     PCV01
     C                   EVAL      CVCO#=10
     C                   WRITE     PCV01
     C                   EVAL      CVCO#=13
     C                   WRITE     PCV01
     C     XXCO#         READE     PCV01                                  99
     C                   ENDDO
     C*
     C*  File ( CTRCP )
     C*
     C     XXCO#         CHAIN     PRC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      RCCO#=7
     C                   WRITE     PRC01
     C                   EVAL      RCCO#=9
     C                   WRITE     PRC01
     C                   EVAL      RCCO#=10
     C                   WRITE     PRC01
     C                   EVAL      RCCO#=13
     C                   WRITE     PRC01
     C     XXCO#         READE     PRC01                                  99
     C                   ENDDO
     C*
     C*  File ( CMSLP )
     C*
     C*          XXCO#     CHAINPSL01                99
     C*          *IN99     DOWEQ*OFF
     C*                    EVAL 7         SLCO#
     C*                    WRITEPSL01
     C*                    EVAL 9         SLCO#
     C*                    WRITEPSL01
     C*                    EVAL 10        SLCO#
     C*                    WRITEPSL01
     C*                    EVAL 13        SLCO#
     C*                    WRITEPSL01
     C*          XXCO#     READEPSL01                    99
     C*                    ENDDO
      *
     C*
     C*  File ( CTCYP )
     C*
     C     XXCO#         CHAIN     PCY01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      CYCO#=7
     C                   WRITE     PCY01
     C                   EVAL      CYCO#=9
     C                   WRITE     PCY01
     C                   EVAL      CYCO#=10
     C                   WRITE     PCY01
     C                   EVAL      CYCO#=13
     C                   WRITE     PCY01
     C     XXCO#         READE     PCY01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTRTP )
     C*
     C     XXCO#         CHAIN     PRT01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      RTCO#=7
     C                   WRITE     PRT01
     C                   EVAL      RTCO#=9
     C                   WRITE     PRT01
     C                   EVAL      RTCO#=10
     C                   WRITE     PRT01
     C                   EVAL      RTCO#=13
     C                   WRITE     PRT01
     C     XXCO#         READE     PRT01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTACP )
     C*
     C     XXCO#         CHAIN     PAC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      ACCO#=7
     C                   WRITE     PAC01
     C                   EVAL      ACCO#=9
     C                   WRITE     PAC01
     C                   EVAL      ACCO#=10
     C                   WRITE     PAC01
     C                   EVAL      ACCO#=13
     C                   WRITE     PAC01
     C     XXCO#         READE     PAC01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTDEP )
     C*
     C     XXCO#         CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      DECO#=7
     C                   WRITE     PDE01
     C                   EVAL      DECO#=9
     C                   WRITE     PDE01
     C                   EVAL      DECO#=10
     C                   WRITE     PDE01
     C                   EVAL      DECO#=13
     C                   WRITE     PDE01
     C     XXCO#         READE     PDE01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTSPP )
     C*
     C     XXCO#         CHAIN     PSP01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      SPCO#=7
     C                   WRITE     PSP01
     C                   EVAL      SPCO#=9
     C                   WRITE     PSP01
     C                   EVAL      SPCO#=10
     C                   WRITE     PSP01
     C                   EVAL      SPCO#=13
     C                   WRITE     PSP01
     C     XXCO#         READE     PSP01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTAPP )
     C*
     C     XXCO#         CHAIN     PAP01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      APCO#=7
     C                   WRITE     PAP01
     C                   EVAL      APCO#=9
     C                   WRITE     PAP01
     C                   EVAL      APCO#=10
     C                   WRITE     PAP01
     C                   EVAL      APCO#=13
     C                   WRITE     PAP01
     C     XXCO#         READE     PAP01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTDSP )
     C*
     C     XXCO#         CHAIN     PDS01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      DSCO#=7
     C                   WRITE     PDS01
     C                   EVAL      DSCO#=9
     C                   WRITE     PDS01
     C                   EVAL      DSCO#=10
     C                   WRITE     PDS01
     C                   EVAL      DSCO#=13
     C                   WRITE     PDS01
     C     XXCO#         READE     PDS01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTTRP )
     C*
     C     XXCO#         CHAIN     PTR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      TRCO#=7
     C                   WRITE     PTR01
     C                   EVAL      TRCO#=9
     C                   WRITE     PTR01
     C                   EVAL      TRCO#=10
     C                   WRITE     PTR01
     C                   EVAL      TRCO#=13
     C                   WRITE     PTR01
     C     XXCO#         READE     PTR01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTATP )
     C*
     C     XXCO#         CHAIN     PAT01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      ATCO#=7
     C                   WRITE     PAT01
     C                   EVAL      ATCO#=9
     C                   WRITE     PAT01
     C                   EVAL      ATCO#=10
     C                   WRITE     PAT01
     C                   EVAL      ATCO#=13
     C                   WRITE     PAT01
     C     XXCO#         READE     PAT01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTIAP )
     C*
     C     XXCO#         CHAIN     PIA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      IACO#=7
     C                   WRITE     PIA01
     C                   EVAL      IACO#=9
     C                   WRITE     PIA01
     C                   EVAL      IACO#=10
     C                   WRITE     PIA01
     C                   EVAL      IACO#=13
     C                   WRITE     PIA01
     C     XXCO#         READE     PIA01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTTTP )
     C*
     C     XXCO#         CHAIN     PTT01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      TTCO#=7
     C                   WRITE     PTT01
     C                   EVAL      TTCO#=9
     C                   WRITE     PTT01
     C                   EVAL      TTCO#=10
     C                   WRITE     PTT01
     C                   EVAL      TTCO#=13
     C                   WRITE     PTT01
     C     XXCO#         READE     PTT01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTBCP )
     C*
     C     XXCO#         CHAIN     PBC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      BCCO#=7
     C                   WRITE     PBC01
     C                   EVAL      BCCO#=9
     C                   WRITE     PBC01
     C                   EVAL      BCCO#=10
     C                   WRITE     PBC01
     C                   EVAL      BCCO#=13
     C                   WRITE     PBC01
     C     XXCO#         READE     PBC01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTNRP )
     C*
     C     XXCO#         CHAIN     PNR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      NRCO#=7
     C                   WRITE     PNR01
     C                   EVAL      NRCO#=9
     C                   WRITE     PNR01
     C                   EVAL      NRCO#=10
     C                   WRITE     PNR01
     C                   EVAL      NRCO#=13
     C                   WRITE     PNR01
     C     XXCO#         READE     PNR01                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTCCP )
     C*
     C     XXCO#         CHAIN     PCC02                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      XCCCO#=7
     C                   WRITE     PCC02
     C                   EVAL      XCCCO#=9
     C                   WRITE     PCC02
     C                   EVAL      XCCCO#=10
     C                   WRITE     PCC02
     C                   EVAL      XCCCO#=13
     C                   WRITE     PCC02
     C     XXCO#         READE     PCC02                                  99
     C                   ENDDO
      *
     C*
     C*  File ( CTPCP )
     C*
     C     XXCO#         CHAIN     PPC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   EVAL      PCCO#=7
     C                   WRITE     PPC01
     C                   EVAL      PCCO#=9
     C                   WRITE     PPC01
     C                   EVAL      PCCO#=10
     C                   WRITE     PPC01
     C                   EVAL      PCCO#=13
     C                   WRITE     PPC01
     C     XXCO#         READE     PPC01                                  99
     C                   ENDDO
      *
      *
     C* END PROGRAM
     C*
     C                   SETON                                        LR
     C*
