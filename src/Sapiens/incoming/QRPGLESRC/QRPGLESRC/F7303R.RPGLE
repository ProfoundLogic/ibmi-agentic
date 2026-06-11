     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                              *
     H****************************************************************
     H* PROGRAM: F7303R  WCMC AUTO CHECK ISSUANCE INFORMATION        *
     H* DESCRIPTION:                                                 *
     H*   1. IT IS RUN AFTER A BILL HAS BEEN AJUDICATED.  IT IS      *
     H*      USED TO CHANGE THE REMARK OR STATUS CODES OF THE        *
     H*      CHECK REQUEST.                                          *
     H*                                                              *
     H****************************************************************
     H* INDICATORS:                                                  *
     H*                                                              *
     H*                                                              *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 04/12/02 C:062 PROJECT: MC025 31901                          *
     H*                ONLY ACCEPT PAYMENT TYPE TRANSACTION CODES    *
     H*                                                              *
     H*  02/02/00  C:062 PROJECT: MC221                              *
     H*                  DON'T UPDATE EOUSER.                        *
     H*                                                              *
     H* 09/21/99 C:62 PROJ: MC025 18159                              *
     H*               RELEASE PREVIOUS CHAINS TO FAEOP/FAEAP         *
     H*                                                              *
     H* 05/19/99 C:62 PROJ: MC025 18159                              *
     H*          CHANGE EDIT OF TRANSACTION CODE TO CHECK            *
     H*          AGAINST TABLE FILE FTPCP.                           *
     H*                                                              *
     H* 02/17/99 C:94 PROJ: 25MKL                                    *
     H*          CHANGE PROGRAM SO THAT IF PROVIDER BILLS IS         *
     H*          INSTALLED, THEN USE FAEAP                           *
     H*                                                              *
     H* 02/03/99 C:94 PROJ: 25MKL                                    *
     H*          CHANGE PROGRAM SO THAT CIKEY IS USING THE           *
     H*          CORRECT INFORMATION.                                *
     H*                                                              *
     H* 05/26/98 C:53 PROJ: MC152                                    *
     H*          EOMB CHECK MISMATCH                                 *
     H*                                                              *
     H* 02/25/97 C:32 PROJ: 31MAD                                    *
     H*          COPIED VER FROM MCMOD3                              *
     H*          REMOVED REFERENCE TO NAMBP SYS TYPE 'M'             *
     H****************************************************************
     FF7303F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FFAEOLG    UF A E           K DISK
     F*
     FWMCIL     IF   E           K DISK
     F*
     FFMPDL     IF   E           K DISK
     F*
     FWTPSL     IF   E           K DISK
     F*
     FWTABL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     F*
     FFAE1L     UF A E           K DISK
     F*
     F*** BEG CHG *** 02/17/99 ***************************************
     FFAEAL1    UF   E           K DISK
     F**********FAEAL1  IF  E           K        DISK
     F*** END CHG *** 02/17/99 ***************************************
     F*
     F*
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Auto Check Issu-
     D                                     ance Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D                 DS
     D  TEMPA1                 1     20
     D  PENDST                13     14

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @1EOB#            9
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@SYS             1
     C                   PARM                    @@PRTY            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CIKEY         KLIST
     C                   KFLD                    KKCO#
     C*** BEG CHG *** 10/21/99 ***************************************
     C********             KFLD           KKFND
     C                   KFLD                    KKWCF#
     C*** END CHG *** 10/21/99 ***************************************
     C*** BEG CHG *** 02/03/99 ***************************************
     C                   KFLD                    KKTYPE
     C**********           KFLD           @@PRTY
     C*** END CHG *** 02/03/99 ***************************************
     C                   KFLD                    @@SYS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C     EOKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C*
     C     EAKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    KKRCN
     C*
     C     E1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    @@EOB#
     C*
     C     TABLKY        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    PSKEY         KKKEY
     C     *LIKE         DEFINE    EARCN         KKRCN
     C*** BEG CHG *** 10/21/99 ***************************************
     C     *LIKE         DEFINE    CIFND         KKWCF#
     C*** END CHG *** 10/21/99 ***************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEGIN CHG *** 05/19/99 **************************************
     C********** SCTRCD    IFEQ *BLANKS
     C**********           SETON                     907176
     C**********           ENDIF
     C                   MOVEL(P)  SCTRCD        KKKEY
     C*
     C* TRANSACTION CODE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     TABLKY        CHAIN     PPC02                              99
     C*
     C   99              SETON                                        907176
     C*
     C                   ENDIF
     C*** END   CHG *** 05/19/99 **************************************
     C*** BEGIN ADD *** 04/12/02 **************************************
     C*
     C* TRANSACTION CODE MUST BE A PAYMENT TYPE
     C*
     C     *IN90         IFEQ      *OFF
     C     PCTRTP        ANDNE     'P'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*** END   ADD *** 04/12/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C***** SEARCH ON TRANSANCTION CODE *****
     C*
     C     HFLD          WHENEQ    'SCTRCD'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY            10
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9071
     C                   ELSE
     C                   MOVEL     ##KEY         SCTRCD
     C                   MOVEL(P)  ##KEY         KKKEY
     C                   SETON                                        9072
     C*
     C     TABLKY        CHAIN     PPC02                              99
     C                   MOVEL     PCDESC        SCTDSC
     C                   ENDIF
     C*
     C***** SEARCH ON REMARK CODE 1 *****
     C*
     C     HFLD          WHENEQ    'SCRMK1'
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY            10
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9073
     C                   ELSE
     C                   MOVEL     ##KEY         SCRMK1
     C                   MOVEL(P)  ##KEY         KKKEY
     C                   SETON                                        9074
     C*
     C     TABLKY        CHAIN     PRC01                              99
     C                   MOVEL     RCDESC        SCR1DS
     C                   ENDIF
     C*
     C***** SEARCH ON REMARK CODE 2 *****
     C*
     C     HFLD          WHENEQ    'SCRMK2'
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY            10
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9074
     C                   ELSE
     C                   MOVEL     ##KEY         SCRMK2
     C                   MOVEL(P)  ##KEY         KKKEY
     C                   SETON                                        9075
     C*
     C     TABLKY        CHAIN     PRC01                              99
     C                   MOVEL     RCDESC        SCR2DS
     C                   ENDIF
     C*
     C***** SEARCH ON REMARK CODE 3 *****
     C*
     C     HFLD          WHENEQ    'SCRMK3'
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY            10
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9075
     C                   ELSE
     C                   MOVEL     ##KEY         SCRMK3
     C                   MOVEL(P)  ##KEY         KKKEY
     C                   SETON                                        9071
     C*
     C     TABLKY        CHAIN     PRC01                              99
     C                   MOVEL     RCDESC        SCR3DS
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C***** INITIALIZE ALL SCREEN FIELDS *****
     C*
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   MOVEL     *BLANKS       SCPNAM
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   Z-ADD     *ZEROS        SCPAY
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTDSC
     C                   MOVEL     *BLANKS       SCPSTS
     C                   MOVEL     *BLANKS       SCRMDS
     C                   MOVEL     *BLANKS       SCRMK1
     C                   MOVEL     *BLANKS       SCR1DS
     C                   MOVEL     *BLANKS       SCRMK2
     C                   MOVEL     *BLANKS       SCR2DS
     C                   MOVEL     *BLANKS       SCRMK3
     C                   MOVEL     *BLANKS       SCR3DS
     C*
     C*** LOADING THE SCREEN HEADING NOT INCLUDED IN API
     C*
     C                   MOVE      @@FEIN        SCFEIN
     C                   MOVE      @@MOD         SCMOD
     C*
     C     PDKEY         CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PDNAME        SCPNAM
     C                   ENDIF
     C*
     C                   MOVEL     @@EOB#        SCEOB#
     C*
     C*** BEGIN CHG *** 09/21/99 **************************************
     C********** EOKEY     CHAINPEO01                99
     C     EOKEY         CHAIN(N)  PEO01                              99
     C*** END   CHG *** 09/21/99 **************************************
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EORCN         KKRCN
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 09/21/99 **************************************
     C********** EAKEY     CHAINPEA01                99
     C     EAKEY         CHAIN(N)  PEA01                              99
     C*** END   CHG *** 09/21/99 **************************************
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EAPAID        SCPAY
     C                   ENDIF
     C*
     C*** LOADING THE CHECK ISSUANCE INFORMATION
     C*
     C****** TRANSACTION CODE *****
     C*
     C*** BEG ADD *** 02/03/99 ***************************************
     C                   MOVE      'P'           KKTYPE            1
     C*** END ADD *** 02/03/99 ***************************************
     C     CMCVFL        IFNE      'Y'
     C*
     C     CIKEY         CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CITRCD        SCTRCD
     C                   ENDIF
     C                   ELSE
     C     CIKEY         CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CITRCV        SCTRCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCTRCD        KKKEY
     C*
     C     TABLKY        CHAIN     PPC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PCDESC        SCTDSC
     C                   ENDIF
     C*
     C****** PEND STATUS CODE *****
     C*
     C                   MOVEL     CISTS         SCPSTS
     C                   MOVEL(P)  SCPSTS        KKKEY
     C*
     C     TABLKY        CHAIN     PPS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      PSDESC        SCPDSC
     C                   ENDIF
     C*
     C****** REMARKS AND REMARKS CODE *****
     C*
     C                   MOVEL     EORMDS        SCRMDS
     C                   MOVEL     EORMK1        SCRMK1
     C                   MOVEL     EORMK2        SCRMK2
     C                   MOVEL     EORMK3        SCRMK3
     C*
     C                   MOVEL(P)  EORMK1        KKKEY
     C*
     C     TABLKY        CHAIN(N)  PRC01                              99
     C                   MOVEL     RCDESC        SCR1DS
     C*
     C                   MOVEL(P)  EORMK2        KKKEY
     C*
     C     TABLKY        CHAIN(N)  PRC01                              99
     C                   MOVEL     RCDESC        SCR2DS
     C*
     C                   MOVEL(P)  EORMK3        KKKEY
     C*
     C     TABLKY        CHAIN(N)  PRC01                              99
     C                   MOVEL     RCDESC        SCR3DS
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCTRCD        EOTRCD
     C                   MOVEL     SCRMDS        EORMDS
     C                   MOVEL     SCRMK1        EORMK1
     C                   MOVEL     SCRMK2        EORMK2
     C                   MOVEL     SCRMK3        EORMK3
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** BEG ADD *** 02/17/99 ***************************************
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILLS
     C*** END ADD *** 02/17/99 ***************************************
     C*
     C***** INITIALIZE ALL SCREEN FIELDS *****
     C*
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C                   MOVEL     *BLANKS       SCPNAM
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   Z-ADD     *ZEROS        SCPAY
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTDSC
     C                   MOVEL     *BLANKS       SCPSTS
     C                   MOVEL     *BLANKS       SCRMDS
     C                   MOVEL     *BLANKS       SCRMK1
     C                   MOVEL     *BLANKS       SCR1DS
     C                   MOVEL     *BLANKS       SCRMK2
     C                   MOVEL     *BLANKS       SCR2DS
     C                   MOVEL     *BLANKS       SCRMK3
     C                   MOVEL     *BLANKS       SCR3DS
     C*
     C                   MOVE      @1EOB#        @@EOB#            9 0
     C*
     C* GET CLAIM MASTER RECORD.
     C*
     C*** BEGIN CHG *** 09/21/99 **************************************
     C********** @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*** END   CHG *** 09/21/99 **************************************
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C*
     C                   MOVE      @@EOB#        KKEOB#
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
     C*** BEGIN ADD *** 10/21/99 **************************************
     C*
     C* GET THE WORKERS COMP GROUP NUMBER
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C     ##RTCD        IFEQ      ' '
     C*
     C                   MOVE      #1WCF#        KKWCF#
     C*
     C                   ENDIF
     C*** END   ADD *** 10/21/99 **************************************
      *
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C*** BEG CHG *** 02/17/99 ***************************************
     C*
     C     @@PRVB        IFEQ      'Y'
     C*
     C***** UPDATE FAEAP WITH PEND STATUS *****
     C     EAKEY         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCTRCD        EATRCD
     C                   MOVEL     SCRMDS        EARMDS
     C                   MOVEL     SCRMK1        EARMK1
     C                   MOVEL     SCRMK2        EARMK2
     C                   MOVEL     SCRMK3        EARMK3
     C*
     C                   MOVEL     SCPSTS        EACKST
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        EATIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C***** UPDATE FAE1P WITH PEND STATUS *****
     C*
     C     E1KY          CHAIN     PE101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     E120A1        TEMPA1
     C                   MOVEL     SCPSTS        PENDST
     C                   MOVEL     TEMPA1        E120A1
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     Q1PGM         E1PGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        E1TIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C                   UPDATE    PE101
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END CHG *** 02/17/99 ***************************************
     C*
     C     EOKEY         CHAIN     PEO01                              99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*** BEGIN DEL *** 02/02/00 **************************************
     C**********           MOVELQ1USER    EOUSER
     C*** END   DEL *** 02/02/00 **************************************
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        EOTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C     EOKEY         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EOIUSR
     C                   MOVEL     Q1PGM         EOIPGM
     C                   TIME                    DFDTTM
     C                   MOVEL     DFTIME        EOITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C                   WRITE     PEO01
     C                   ENDIF
     C*
     C***** WRITE  FAE1P WITH PEND STATUS *****
     C*
     C     E1KY          CHAIN     PE101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     E120A1        TEMPA1
     C                   MOVEL     SCPSTS        PENDST
     C                   MOVEL     TEMPA1        E120A1
     C                   MOVEL     Q1USER        E1IUSR
     C                   TIME                    DFDTTM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1IDT
     C                   WRITE     PE101
     C                   ENDIF
     C*
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FMCMP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
