     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0620R02 - PAYMENT INFORMATION
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO VIEW A SELECTED *
     H*               CHECK.                                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0620R - WORK WITH PAYMENTS                                 *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:33    CREATED: 08/05/97   PROJECT: WC660        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Load SCPODT with PARQDT if PARQDT not zero.    *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 05/19/00 C:15  Support: WC034/20089                           *
     H*                Added the TRDT to key list for WDRPP.          *
     H*                                                               *
     H* 04/12/99 C:90  PROJECT: WC889                                 *
     H*                IF DBA FLAG = Y, LOAD PDADR2, ELSE LOAD PDADR1.*
     H*                                                               *
     H* 09/08/98 C:15  Support: WC021/13581                           *
     H*                Chgd pgm to prevent a record lock on WDPAP.    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   51  IF WEEKS AND DAYS ARE ZERO, NON-COMP PAYMENT, DON'T     *
     H*       DISPLAY                                                 *
     H*   52  IF PAYEE IS ZERO, COMP PAYMENT, DON'T DISPLAY PAYEE     *
     H*   53  IF ALL REMARKS ARE BLANK, DON'T DISPLAY                 *
     H*****************************************************************
     FW0620F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWDPAP     UF A E             DISK
     F                                     RENAME(PPA01:DBRCDF)
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWDRPL     IF   E           K DISK
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** BEG ADD *** 10/20/03 **********************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 **********************************
      /COPY SCOPYMT01D

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D CD              S              1    DIM(6) CTDATA PERRCD(1) ASCEND       OTHER TYPE CODE/DESC
     D DS              S             15    DIM(6) ALT(CD)
     D*** BEG ADD *** 10/20/03 *******************************************  ****
     D NF              C                   CONST('NOT FOUND')
     D*** END ADD *** 10/20/03 *******************************************  ****
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Payment Information')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE GROUP
     C*
     C     FNKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXKEY
     C*
     C*  DETAIL PAYMENT FILE KEY LIST
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    PATRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*** End add ***** 05/22/03 **************************************
     C*
     C*  EMPLOYER MASTER FILE KEY LIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*** BEG ADD *** 10/20/03 **********************************
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C*** END ADD *** 10/20/03 **********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    B2KEY         XXKEY
     C*** BEG ADD *** 10/20/03 **********************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 10/20/03 **********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      PACHK#        SCCHK#
     C                   MOVEL     PAFYR         SCCASE
     C                   MOVE      PACASE        SCCASE
     C*
     C     PAPGTP        IFNE      'G'
     C*
     C                   MOVE      PAAMT         SCAMT
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVFR
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVTO
     C*
     C                   MOVE      PATRCD        SCTRCD
     C                   MOVE      PATYPE        SCTYPE
     C                   MOVEL     PARECD        SCRECD
     C                   MOVE      PARETP        SCRETP
     C                   MOVE      PAWKS         SCWKS
     C                   MOVE      PADAYS        SCDAY
     C                   MOVEL     PADESC        SCDES
      *
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C**************       Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   CLEAR                   SCDES2
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C  N60              MOVEL     B2DESC        SCDES2
     C*
     C                   MOVEL     PABR2         XXBR2             4
     C                   MOVE      ' -'          XXBR2
     C                   MOVE      XXBR2         SCBR2
     C*
     C                   MOVE      PAFEIN        SCFEIN
     C                   MOVE      PAMOD         SCMOD
     C*
     C                   Z-ADD     PAEOB#        SCEOB#
     C*
     C                   MOVEL     *BLANKS       SCPYNM
     C     PDKY          CHAIN     PPD01                              60
     C  N60              MOVEL     PDNAME        SCPYNM
     C*
     C*** Begin dlt *** 05/22/03 **************************************
     C********** PATYPE    IFEQ 'P'
     C********** PATYPE    OREQ 'M'
     C*** End dlt ***** 05/22/03 **************************************
     C*
     C*** Begin add *** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              60
     C     *IN60         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C     RPKY          CHAIN     PRP01                              60
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     RPPNAM        SCCKNM
     C                   MOVEL     RPADR1        SCADR1
     C                   MOVEL     RPADR2        SCADR2
     C                   MOVE      RPZIP         XXZIP
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL(P)  RPST          XXST             13
     C                   MOVEA(P)  XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     XXZIP2        IFNE      *ZERO
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF                                                  XXZIP2<>*ZERO
     C*
     C                   MOVEA(P)  SZ            XXSZ             13
     C                   MOVEL(P)  RPCTY         XXLAST           56
     C                   MOVEL(P)  XXSZ          XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   MOVEL(P)  XXLAST        SCCSZ
     C*
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     RPCTRY        IFNE      *BLANK
     C     RPCTRY        ANDNE     'USA'
     C                   MOVEL(P)  RPADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  RPCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  RPCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C*** BEG CHG *** 10/20/03 *******************************************  ****
     C********** RPADR2    IFEQ *BLANKS
     C  N05RPADR2        IFEQ      *BLANKS
     C*** END CHG *** 10/20/03 *******************************************  ****
     C                   MOVEL     SCCSZ         SCADR2
     C                   MOVEL     *BLANKS       SCCSZ
     C                   ENDIF                                                  RPADR2=*BLANKS
     C*
     C                   MOVEL(P)  RPRMDS        SCRMDS
     C*
     C     RPRMK1        IFNE      *BLANK
     C                   MOVEL     *BLANKS       SCRMK1
     C                   MOVEL(P)  RPRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDESC        SCRMK1
     C                   ENDIF                                                  RPRMK1<>*BLANK
     C*
     C     RPRMK2        IFNE      *BLANK
     C                   MOVEL     *BLANKS       SCRMK2
     C                   MOVEL(P)  RPRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDESC        SCRMK2
     C                   ENDIF                                                  RPRMK2<>*BLANK
     C*
     C     RPRMK3        IFNE      *BLANK
     C                   MOVEL     *BLANKS       SCRMK3
     C                   MOVEL(P)  RPRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDESC        SCRMK3
     C                   ENDIF                                                  RPRMK3<>*BLANK
     C*
     C     RPCKCN        IFNE      *BLANK
     C                   MOVEL     *BLANKS       SCCKCN
     C                   MOVEL(P)  RPCKCN        XXKEY
     C     TBKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDESC        SCCKCN
     C                   ENDIF                                                  RPCKCN<>*BLANK
     C*
     C                   ELSE
     C*
     C                   MOVE      CMSS#         XXSS#             9 0
     C*
     C     PAFEIN        IFEQ      *ZERO
     C     PAFEIN        OREQ      XXSS#
     C     PARETP        OREQ      'C'
     C*
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  CDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL     SCCNAM        SCCKNM
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVE      CDZIP         XXZIP
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL(P)  CDST          XXST             13
     C                   MOVEA(P)  XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     XXZIP2        IFNE      *ZERO
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF                                                  XXZIP2<>*ZERO
     C*
     C                   MOVEA(P)  SZ            XXSZ
     C                   MOVEL(P)  CDCTY         XXLAST
     C                   MOVEL(P)  XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   MOVEL(P)  XXLAST        SCCSZ
     C*
     C*** BEG CHG *** 10/20/03 *******************************************  ****
     C********** CDADR2    IFEQ *BLANKS
     C  N05CDADR2        IFEQ      *BLANKS
     C*** END CHG *** 10/20/03 *******************************************  ****
     C                   MOVEL(P)  SCCSZ         SCADR2
     C                   MOVEL     *BLANKS       SCCSZ
     C                   ENDIF                                                  CDADR2=*BLANKS
     C*
     C                   ELSE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     PDNAME        SCCKNM
      **** BEGIN ADD **** 04/12/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR2        SCADR1
     C                   ELSE
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVEL     PDADR1        SCADR1
      **** BEGIN ADD **** 04/12/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/12/99 ************************************
     C                   MOVE      PDZIP         XXZIP
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL(P)  PDST          XXST
     C                   MOVEA(P)  XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     XXZIP2        IFNE      *ZERO
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF                                                  XXZIP2<>*ZERO
     C*
     C                   MOVEA(P)  SZ            XXSZ
     C                   MOVEL(P)  PDCTY         XXLAST
     C                   MOVEL(P)  XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCADR2
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  PAFEIN=*ZERO/XXSS#
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** Begin dlt *** 05/22/03 **************************************
     C**********           ELSE
     C**********
     C*  IF TRANSACTION IS A NON-PAYMENT TYPE SHOW THE DESCRIPTION
     C*  (VOID, ADJUSTMENT, STOP PAYMENT, RECOVERY, REVERSAL, GROUP)
     C**********
     C**********           Z-ADD1         X       30
     C********** PATYPE    LOKUPCD,X                     60
     C***60*****           MOVELDS,X      SCADR1
     C**********
     C**********           ENDIF                           PATRCD=P/M
     C*** End dlt ***** 05/22/03 **************************************
     C*
     C                   MOVE      *OFF          *IN51
     C     SCWKS         IFEQ      *ZEROS
     C     SCDAY         ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN51
     C                   ENDIF                                                  SCWKS=*ZEROS
     C*
     C                   MOVE      *OFF          *IN52
     C     SCFEIN        IFEQ      *ZEROS
     C     SCMOD         ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN52
     C                   ENDIF                                                  SCFEIN=*ZEROS
     C*
     C                   MOVE      *OFF          *IN53
     C     SCRMK1        IFEQ      *BLANKS
     C     SCRMK2        ANDEQ     *BLANKS
     C     SCRMK3        ANDEQ     *BLANKS
     C     SCCKCN        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN53
     C                   ENDIF                                                  SCRMK1=*BLANKS
     C*
     C                   ELSE
     C*
     C*  IF CHECK IS A GROUP CHECK DISPLAY GROUP
     C*
     C                   Z-ADD     1             X                 3 0
     C     'G'           LOOKUP    CD(X)                                  60
     C   60              MOVEL     DS(X)         SCADR1
     C*
     C                   ENDIF                                                  PAPGTP<>'G'
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C*** BEGIN CHG *** 09/08/98 **************************************
     C********** @@RRN     CHAINWDPAP                60
     C     @@RRN         CHAIN(N)  WDPAP                              60
     C*** END CHG ***** 09/08/98 **************************************
     C*
     C*  GET THE GROUP RECORD
     C*
     C                   Z-ADD     PAFND         SCFND
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  GET THE CLAIM RECORD
     C*
     C     CMKY          CHAIN     PCM01                              61
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       SCSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      *****  End   Chg  ***  06/09/20  **********************
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C*
     C*  GET THE EMPLOYER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              61
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  LOAD THE CLAIMANT NAME
     C*
     C                   MOVEL     *BLANKS       SCCNAM
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CDLSN         QQLAST           56
     C                   MOVEL(P)  CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C*
     C                   CALL      'SRFRLS'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C*
     C                   MOVEL     QQLAST        SCCNAM
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WDPAP
     C*
     C                   ENDSR                                                  WRITER
     C*
**  NON-PAYMENT CODES/DESCRIPTIONS
AADJUSTMENT
GGROUP CHECK
RRECOVERY
SSTOP PAYMENT
VVOID
ZREVERSAL
