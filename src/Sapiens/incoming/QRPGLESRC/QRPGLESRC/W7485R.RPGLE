     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/title CMS Log History
     H Bnddir('WTBNDDIR')
     H*====================================================================
     H*  Created: 07/28/09   C:65   Project: WC1819
     H*====================================================================
    H*  Revisions:
     H*
     H* 03/06/25 C:333  Project WC2998
     H*                 Changes to CMS Settlement Reporting.
     H*                 - Recompiled.
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*
     H* 04/21/16 C:65  PROJECT: WC2427
     H*                Remove option 2 since it only applied to CMS auto
     H*                letter log entries (array entry changed - no comment).
     H*
     H* 04/15/11 C:198 Support: 76576
     H*                Fixed page down issue in case next page first record has the
     H*                same key as current page last record.
     H*
     H* 02/02/11 C:235 Project: 114WXO
     H*                Claim Dept. Letter to Gather CMS information.
     H*
     H* 08/09/10 C:198 Project: WC1879 Priss 49
     H*                Corrected display of action "Add" in the subfile to be
     H*                left justified.
     H*
     H* 05/18/10 C:198 Project: WC1879
     H*                Populated re-submit flag.
     H*
     H* 01/28/10 C:198 PROJECT: WC1819 Phase 2
     H*                Changes to pick disposition code from WTDSP.
     H*
     H* 08/14/09 C:232 PROJECT: WC1819 Phase 5                        *
     H*                Medicare Query - WCMS.                         *
     H*====================================================================
     Fw7485f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
     Fwmcmp     if   e             disk
    F*  Physical file definition (for refreshing subfile record)
     Fwdd1p     if   e             disk    rename(pd101:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdd1l     if   e           k disk    rename(pd101:dbrcdf)
     F                                     infds(dbfds)
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWZEXL     IF   E           K DISK
     F*** Beg Chg *** 01/28/10 ***************************************
     F*******WTDML     IF   E           K DISK
     FWTDSL     IF   E           K DISK
     F*** End Chg *** 01/28/10 ***************************************
    D*
     D hd              s             78    dim(2) ctdata perrcd(1)
     D#ofOpts          c                   15
     D of              s              1    dim(#ofOpts) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOpts) alt(of)
     D*
     D*
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMS Log History'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D*** BEGIN CHG *** 08/14/09 **************************************
     D*** acdpgm          c                   'W7485R02'
     D*** END CHG ***** 08/14/09 **************************************
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   7
     D LO              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D @@call          s              1
     D kkexdt          s              7  0
     D*** Beg Add *** 04/15/11 ****************************************
     D svexdt          s                   like(d1exdt)
     D*** End Add *** 04/15/11 ****************************************
     D*** BEGIN ADD *** 08/14/09 **************************************
     D acdpgm          s             10a
     D*** END ADD ***** 08/14/09 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
    C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C*** Beg Add *** 08/14/09 ******************************************
     C                   parm      @@rrn         ##rrn2
     C*** End Add *** 08/14/09 ******************************************
    C*
     C     acdpl1        plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
     C                   kfld                    kkexdt
     C*
     C     key2          klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
     C*
     C* CMS Compliance code file.
     C*
     C     dmky          klist
     C                   kfld                    CMCO#
     C                   kfld                    D1CODE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  EMPLOYER Cross Reference File
     C*
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C*
     C*
     C                   endsr
     C*
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*** BEGIN ADD *** 08/14/09 **************************************
     C                   if        sctype = 'Q'
     C                   eval      acdpgm  = 'W7485R03'
     C                   else
     C                   eval      acdpgm  = 'W7485R02'
     C                   endif
     C*** END ADD ***** 08/14/09 **************************************
     C*** BEG ADD ***** 02/02/11 **************************************
     C                   if        sctype = 'I' AND (scsel='2' or scsel='5')
     C                   eval      acdpgm  = 'W7485R04'
     C                   endif
     C                   if        sctype <>'I' AND scsel='2'
     C                   eval      *in90=*on
     C                   endif
     C*** END ADD ***** 02/02/11 **************************************
     C                   endsr
     C*
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C*
     C                   endsr
     C*
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C*
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkexdt = 0
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     c                   if        scpst1 = *blanks
     c                   move      *loval        kkexdt
     c                   else
     C                   move      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKEXDT
     c                   endif
     C*
     C                   endsr
     C*
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkexdt = scexdt
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scexdt = d1exdt
     C                   eval      scrsdt = d1rsdt
     C                   eval      sccode = d1code
     C                   eval      sctype = d1type
     C*
     c                   clear                   xxdesc
     c*** Beg Chg *** 01/28/10 ****************************************
     c**** dmky          chain     pdm01                              60
     c     dmky          chain     pds01                              60
     c*** End Chg *** 01/28/10 ****************************************
     c                   if        *in60 = *off
     c*** Beg Chg *** 01/28/10 ****************************************
     c****               movel     dmdesc        xxdesc           30
     c                   movel     dsdesc        xxdesc           30
     c*** End Chg *** 01/28/10 ****************************************
     c                   endif
     C*
     C                   Z-ADD     D1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXEXDT            8 0
     C                   Z-ADD     D1RSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXRSDT            8 0
     C*
     c                   clear                   xxtype            6
     c                   select
     c                   when      d1type = '0'
     C*** Beg Chg *** 08/09/10 *********************************************
     c*********          eval      xxtype = '   Add'
     c                   eval      xxtype = 'Add   '
     C*** End Chg *** 08/09/10 *********************************************
     c                   when      d1type = '1'
     c                   eval      xxtype = 'Delete'
     c                   when      d1type = '2'
     c                   eval      xxtype = 'Update'
     C*** Beg Add *** 08/14/09 *********************************************
     c                   when      d1type = 'Q'
     c                   eval      xxtype = 'Query'
     C*** BEG ADD *** 02/02/11 *********************************************
     c                   when      d1type = 'I'
     c                   eval      xxtype = 'InfoRq'
     c                   eval      xxdesc = d1rspd
     C*** BEG ADD *** 02/02/11 *********************************************
     c                   endsl
     C*** End Add *** 08/14/09 *********************************************
     C*
     C                   select
     C                   when      info# = 1 and view# = 1
     C*
     c                   if        xxrsdt <> 0
     C                   eval      scData = %editc(xxexdt:'Y') + '  '
     C                                    + xxtype + '  '
     C                                    + %editc(xxrsdt:'Y') + '       '
     C*** Beg Add *** 05/18/10 *************************************************
     C                                    + d1rsbm + '         '
     C*** End Add *** 05/18/10 *************************************************
     C*** Beg Chg *** 05/18/10 *************************************************
     C******                              + d1code + '        '
     C                                    + d1code + '     '
     C*** End Chg *** 05/18/10 *************************************************
     C                                    + XXDESC
     c                   else
     C                   eval      scData = %editc(xxexdt:'Y') + '  '
     C*** Beg Chg *** 05/18/10 *************************************************
     C********                            + xxtype + '                   '
     C                                    + xxtype + '                  '
     C*** End Chg *** 05/18/10 *************************************************
     C*** Beg Add *** 05/18/10 *************************************************
     C                                    + d1rsbm + '         '
     C*** End Add *** 05/18/10 *************************************************
     C*** Beg Chg *** 05/18/10 *************************************************
     C********                            + d1code + '        '
     C                                    + d1code + '     '
     C*** End Chg *** 05/18/10 *************************************************
     C                                    + XXDESC
     c                   endif
     c*
     C                   endsl
     C*
     C                   endsr
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6 (Error Codes)
     C*===============================================================
     C     OPT6          BEGSR
     C*
     C                   MOVE      ' '           ##MODE
     C                   CALL      'W7486R'      ACDPL1                   99
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7 (Compliance Codes)
     C*===============================================================
     C     OPT7          BEGSR
     C*
     C                   MOVE      ' '           ##MODE
     C                   CALL      'W7487R'      ACDPL1                   99
     C*
     C                   ENDSR
      *
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
     C*
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C                   END
     C*
     C                   endsr
     C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
     C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     c*
     C*** BEGIN ADD *** 08/14/09 **************************************
     C                   eval      acdpgm = 'W7485R02'
     C*** END ADD ***** 08/14/09 **************************************
     C                   movel(p)  hd(2)         schdl2
     c*
     c     @@rrn         chain     pcm01
     c*
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     CMFND         SCFND
     C  N60              MOVEL     FNNAME        SCFNAM
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      ******  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ******  End Chg  ***  06/09/20  **********************
     C                   ENDIF                                                  CMSS# <> *BLANKS
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   ENDIF                                                  *IN60='0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF                                                  CDLSN <> ' '
      *
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
     c*
     C                   endsr
     C*
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key2          reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      *in21
     C     key2          readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C*** Beg Add *** 04/15/11 ****************************************
     C                   eval      svexdt = d1exdt
     C                   readp     dbrcdf
     C                   if        d1exdt <> svexdt
     C*** End Add *** 04/15/11 ****************************************
     C     key1          setgt     dbrcdf                             96
     C*** Beg Add *** 04/15/11 ****************************************
     C                   endif
     C*** End Add *** 04/15/11 ****************************************
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
     C*
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
     C*
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C*
     C                   endsr
     C*



** Column headings
Opt    Date     Action     Date of   Resubmit   Disp   Description
     Submitted   Type     Response     Flag     Code
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Error Codes            YYY
1077=Compliance Codes       YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YYY
3
