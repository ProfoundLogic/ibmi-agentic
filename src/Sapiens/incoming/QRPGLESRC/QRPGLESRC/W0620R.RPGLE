     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0620R - WORK WITH PAYMENTS
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                PAYMENTS FOR A CLIAM BY TRANSACTION DATE,      *
     H*                FEDERAL ID #, OR CHECK #.  EACH PAYMENT CAN BE *
     H*                ACCESSED FOR DISPLAY OR SELECTION DEPENDING    *
     H*                UPON THE CALLING PROGRAM.                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0620R02 - PAYMENT INFORMATION (OPT 2/5)                   *
     H*                                                               *
     H*  CREATED: 08/01/97  PROGRAMMER: C:33  PROJECT: WC660          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 04/18/07 C:15 Support: WC139/55492                            *
     H*               Added code to use W0576R02 instead of W0620R02. *
     H*                                                               *
     H* 10/29/02 C:15 Support: WC095/34712                            *
     H*               Added code to display records for adjustments   *
     H*               even if the cleared date is not blank.          *
     H*                                                               *
     H* 01/20/00 C:15 Support: WC031/21360                            *
     H*               Removed reference to Bank Info Trust flag when  *
     H*               determining if a "cleared" pmt is allowed to be *
     H*               voided or stopped.                              *
     H*                                                               *
     H* 04/30/98 C:33 SEQ#: WC100/17914                               *
     H*               USE CORRECT KLIST FOR SETLL & SETGT             *
     H*                                                               *
     H* 05/22/98 C:15 Support: WC082/11029                            *
     H*              -Fixed the routine that determines which trans-  *
     H*               actions appear on the screen.                   *
     H*              -Changed the key to the check recon file so it   *
     H*               will be unique.                                 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  21 - Descending transaction date sort                        *
     H*  22 - Federal Id # sort                                       *
     H*  23 - Check # sort                                            *
     H*  30 - Called as a search                                      *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  45 - Display passed transaction code/type                    *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FW0620F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDPAP     IF   E             DISK
     F                                     RENAME(PPA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDPAL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPA01:DBRCDF)
     FWDPALC    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPA01:DBRCDF2)
     FWDPALB    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPA01:DBRCDF3)
     FWMCMP     IF   E             DISK
     FWMCDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*WMBIL   IF  E           K        DISK
      *_________________________________________________________________
     F*** BEGIN DLT *** 01/20/00 **************************************
      *  Bank Info File       key = CO#/APPL/GRP/EFDT  Omit BISTAT='I'
     F********WMBIL7  IF  E           K        DISK
     F*** END DLT ***** 01/20/00 **************************************
      *_________________________________________________________________
     F*** BEGIN DLT *** 05/22/98 **************************************
     F******WDCRL   IF  E           K        DISK
     F*** END DLT ***** 05/22/98 **************************************
     F*** BEGIN ADD *** 05/22/98 **************************************
     FWDCRL5    IF   E           K DISK
     F*** END ADD ***** 05/22/98 **************************************
      /COPY SCOPYWW01D
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D DTA             S              1    DIM(32)                              F11
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*** BEGIN DLT *** 05/22/98 **************************************
     D********** IDS
     D**************************************  1  15 X1CRKY
     D**************************************  1   30X1FYR
     D**************************************  4  100X1CASE
     D************************************** 11  150X1SEQ
     D*** END DLT ***** 05/22/98 **************************************
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Payments')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D*** Begin chg *** 04/18/07 **************************************
     D************  'W0620R02'            C         ACDPGM
     D ACDPGM          C                   CONST('W0576R02')
     D*** End chg ***** 04/18/07 **************************************
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(12)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@TRCD            2
     C*
     C*  NOTE:  @@RRN COMING INTO THE PROGRAM NEEDS TO BE THE RELETIVE
     C*         RECORD # OF THE CLAIM & RETURNING TO THE CALLING
     C*         PROGRAM WILL BE THE RELETIVE RECORD # OF THE PAYMENT.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AFYR             3
     C                   PARM                    #ACASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     KEY1A         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKSEQ#
     C*
     C     KEY2A         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKTRDT
     C*
     C     KEY1B         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKSEQ#
     C*
     C     KEY1C         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKSEQ#
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
      *  BANK INFO FILE
      *
     C***        BIKY      KLIST
     C***                  KFLD           CMCO#
     C***                  KFLD           CMFND
      *
     C*** BEGIN DLT *** 01/20/00 **************************************
     C********** BIKY1     KLIST
     C**********           KFLD           CMCO#
     C**********           KFLD           XXAPPL  2
     C**********           KFLD           CMFND
     C**********           KFLD           XXDT
     C**********
     C********** BIKY2     KLIST
     C**********           KFLD           CMCO#
     C**********           KFLD           XXAPPL
     C**********           KFLD           CMFND
     C*** END DLT ***** 01/20/00 **************************************
     C*
     C*  CHECK RECONCILIATION FILE
     C*
     C     CRKY1         KLIST
     C                   KFLD                    CMCO#
     C*** BEGIN ADD *** 05/22/98 **************************************
     C                   KFLD                    PAACCT
     C                   KFLD                    PACHK#
     C*** END ADD ***** 05/22/98 **************************************
     C*** BEGIN DLT *** 05/22/98 **************************************
     C******************** KFLD           X1CRKY
     C*** END DLT ***** 05/22/98 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PATRDT        KKTRDT
     C     *LIKE         DEFINE    PAFEIN        KKFEIN
     C     *LIKE         DEFINE    PAMOD         KKMOD
     C     *LIKE         DEFINE    PACHK#        KKCHK#
     C     *LIKE         DEFINE    PASEQ#        KKSEQ#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *HIVAL        KKTRDT
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C                   MOVE      *LOVAL        KKCHK#
     C                   MOVE      *LOVAL        KKSEQ#
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPST2        KKFEIN
     C                   MOVE      SCPSTA        KKMOD
     C*
     C     *IN23         WHENEQ    *ON
     C                   MOVE      SCPST3        KKCHK#
     C*
     C                   ENDSL
     C*
     C                   MOVE      *LOVAL        KKSEQ#
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCDT          KKTRDT
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   Z-ADD     SCCHK#        KKCHK#
     C                   Z-ADD     SCSEQ#        KKSEQ#
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     PATRDT        SCDT
     C                   Z-ADD     PAFEIN        SCFEIN
     C                   Z-ADD     PAMOD         SCMOD
     C                   Z-ADD     PACHK#        SCCHK#
     C                   Z-ADD     PASEQ#        SCSEQ#
     C                   Z-ADD     PATRDT        SCTRDT
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZTRDT            8 0
     C*
     C                   MOVEL     ZZTRDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXTRDT           10
     C*
     C                   MOVE      PAFEIN        XXFEIN            9
     C*
     C                   MOVEL     PAMOD         XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXMOD             3
     C*
     C                   MOVEL     PACHK#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCHK#            7
     C*
     C                   CLEAR                   DTA
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVEA     XXTRDT        DTA(1)
     C                   MOVEA     XXCHK#        DTA(12)
     C                   MOVEA     XXFEIN        DTA(20)
     C                   MOVEA     XXMOD         DTA(30)
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVEA     XXFEIN        DTA(1)
     C                   MOVEA     XXMOD         DTA(11)
     C                   MOVEA     XXCHK#        DTA(15)
     C                   MOVEA     XXTRDT        DTA(23)
     C*
     C     *IN23         WHENEQ    *ON
     C                   MOVEA     XXCHK#        DTA(1)
     C                   MOVEA     XXFEIN        DTA(9)
     C                   MOVEA     XXMOD         DTA(19)
     C                   MOVEA     XXTRDT        DTA(23)
     C*
     C                   ENDSL
     C*
     C                   MOVEA     DTA           SCDATA
     C*
     C                   MOVEL     PATRCD        SCTRCD
     C                   MOVEL     PATYPE        SCTYPE
     C                   MOVEL     PARECD        SCRECD
     C                   MOVEL     PARETP        SCRETP
     C                   MOVEL     PADESC        SCDESC
     C                   Z-ADD     PAAMT         SCAMT
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C                   CLEAR                   @@RRN
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #ACO#
     C                   MOVE      CMFND         #AFND
     C                   MOVE      CMFYR         #AFYR
     C                   MOVE      CMCASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   Z-ADD     CMFND         SCFND
     C                   MOVEL     CMFYR         SCCASE
     C                   MOVE      CMCASE        SCCASE
      *****  Begin Chg  ***  06/09/20  **********************
     C*****              MOVE      CMSS#         SCSS#
      /Free
       SCSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      *****  End   Chg  ***  06/09/20  **********************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOI
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C                   CLEAR                   SCCNAM
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDLSN         SCCNAM
     C                   CAT       ',':0         SCCNAM
     C                   CAT       CDFSN:1       SCCNAM
     C                   CAT       CDMID:1       SCCNAM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   CLEAR                   SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     Q1PRMS        IFGE      5
     C     @@TYPE        IFNE      *BLANK
     C                   MOVE      *HIVAL        @INCC
     C                   ENDIF                                                  @@TYPE<>*BLANK
     C                   MOVEL     @@TYPE        SCTRTP
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C                   MOVE      *OFF          *IN45
     C     Q1PRMS        IFGE      6
     C     @@TRCD        COMP      *BLANK                             4545
     C                   MOVEL     @@TRCD        SCTCOD
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C   23KEY1          READE     DBRCDF3                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C   23KEY1          READPE    DBRCDF3                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C***  BEGIN CHG  ***  04/30/99  *********************************
     C***21***** KEY2A     SETGTDBRCDF               96
     C   21KEY1A         SETGT     DBRCDF                             96
     C***  END   CHG  ***  04/30/99  *********************************
     C   22KEY1B         SETGT     DBRCDF2                            96
     C   23KEY1C         SETGT     DBRCDF3                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C***  BEGIN CHG  ***  04/30/99  *********************************
     C***21***** KEY2A     SETLLDBRCDF               96
     C   21KEY1A         SETLL     DBRCDF                             96
     C***  END   CHG  ***  04/30/99  *********************************
     C   22KEY1B         SETLL     DBRCDF2                            96
     C   23KEY1C         SETLL     DBRCDF3                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     Q1PRMS        IFGE      5
     C*
     C*  IF REQUEST IS FOR VOID/STOP THE TYPE MUST BE 'P' FOR PAYMENY;
     C*  'M' FOR MANUAL; OR 'A' FOR ADJUSTMENT TO BE DISPLAYED.  ALSO,
     C*  THE ADJUSTMENT FLAG MUST BE BLANK.
     C*
     C     @@TYPE        IFEQ      'V'
     C     @@TYPE        OREQ      'S'
     C*** BEGIN ADD *** 05/22/98 **************************************
     C     @@TYPE        OREQ      'R'
     C*** END ADD ***** 05/22/98 **************************************
     C*
     C                   SELECT
     C*
     C*** BEGIN ADD *** 05/22/98 **************************************
     C     PAPGTP        WHENEQ    'G'
     C                   MOVE      *OFF          MEETSS
     C*** END ADD ***** 05/22/98 **************************************
     C*
     C     PATYPE        WHENNE    'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'A'
     C                   MOVE      *OFF          MEETSS
     C*
     C     PATYPE        WHENEQ    'A'
     C     PAADST        ANDNE     *BLANK
     C     PABASQ        ANDEQ     99999
     C                   MOVE      *OFF          MEETSS
     C*
     C     PAADST        WHENNE    *BLANK
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  @@TYPE=S/V
     C*
     C*  IF REQUEST IS FOR A REVERSAL OF A VOID/STOP THE TYPE MUST BE
     C*  'S' FOR STOP OR 'V' FOR VOID TO BE DISPLAYED.
     C*
     C     @@TYPE        IFEQ      'Z'
     C*
     C                   SELECT
     C*
     C*** BEGIN ADD *** 05/22/98 **************************************
     C     PAPGTP        WHENEQ    'G'
     C                   MOVE      *OFF          MEETSS
     C*** END ADD ***** 05/22/98 **************************************
     C*
     C     PATYPE        WHENNE    'S'
     C     PATYPE        ANDNE     'V'
     C                   MOVE      *OFF          MEETSS
     C*
     C     PAADST        WHENNE    *BLANK
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  @@TYPE=Z
     C*
     C*  IF REQUEST IS FOR AN ADJUSTMENT, ALL RECORDS WITH A BLANK
     C*  BLANK ADJUSTMENT FLAG WILL BE DISPLAYED
     C*
     C     @@TYPE        IFEQ      'A'
     C*
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C     PAADST        ORNE      *BLANK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  PATYPE=S/V
     C*
     C                   ENDIF                                                  @@TYPE=A
     C*
     C*** Begin dlt *** 10/29/02 **************************************
     C*  IF THE PAYMENT HAS BEEN CLEARED DO NOT DISPLAY
     C**********
     C********** PARCDT    IFNE *ZERO
     C**********           MOVE *OFF      MEETSS
     C**********           ENDIF                           PARCDT<>*ZERO
     C*** End dlt ***** 10/29/02 **************************************
     C*
     C*  IF THE TRUST FLAG IN THE BANK INFO FILE, WMBIP:BITRST, IS 'Y',
     C*  AND THE USER IS ATTEMPTING TO DO A VOID OR STOP, AND THE CHECK
     C*  RECONCILIATION FILE INDICATES THAT THE CHECK HAS BEEN CLEARED,
     C*  WDCRL:CRACTP=X, DO NOT DISPLAY
     C*
     C     @@TYPE        IFEQ      'V'
     C     @@TYPE        OREQ      'S'
     C***
     C***        BIKY      CHAINPBI01                60
     C***
      *
     C*** BEGIN DLT *** 01/20/00 **************************************
      *  Access Bank Info file to obtain current Account record
      *  NOTE:  Logical WMBIL7 has OMIT BISTAT = 'I'
      **********
     C**********           MOVE 'WC'      XXAPPL
     C********** BIKY1     SETGTPBI01
     C********** BIKY2     READEPBI01                    60
      **********
     C********** *IN60     IFEQ *OFF
     C********** BITRST    ANDEQ'Y'
     C*** END DLT ***** 01/20/00 **************************************
     C*
     C*** BEGIN DLT *** 05/22/98 **************************************
     C******************** CLEARX1CRKY
     C******************** Z-ADDCMFYR     X1FYR
     C******************** Z-ADDCMCASE    X1CASE
     C******************** Z-ADDPASEQ#    X1SEQ
     C*** END DLT ***** 05/22/98 **************************************
     C*
     C     CRKY1         CHAIN     PCR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CRACTP        ANDEQ     'X'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  *IN60=*OFF/CRACTP=X
     C*
     C*** BEGIN DLT *** 01/20/00 **************************************
     C******************** ENDIF                           *IN60=*OFF/BITRST=Y
     C*** END DLT ***** 01/20/00 **************************************
     C*
     C                   ENDIF                                                  @@TYPE=V/S
     C*
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C                   ENDSR
     C*
** Column headings
Opt Trans date Check # Fed Id #  Mod  Trns  Resv  Description          Amount
Opt Fed Id #  Mod Check # Trans date  Trns  Resv  Description          Amount
Opt Check # Fed Id #  Mod Trans date  Trns  Resv  Description          Amount
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           NNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
