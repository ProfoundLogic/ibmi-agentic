     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H**************************************************************************************
     H/TITLE W4343R - Work with attorney orders                                            *
     H Bnddir('WTBNDDIR')
     H**************************************************************************************
     H*  Description: This program tracks claimant attorney orders                         *
     H*               including order date, start & end date, fee etc.                     *
     H*               In additioan, summary section displays total                         *
     H*               cap, total amount paid to date, and remaining                        *
     H*               amount                                                               *
     H**************************************************************************************
     H*  Created: 02/2010    Programmer C:247   Project: 25WPT                             *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 05/08/15  C:308  PROJECT: PHW33133                                                 *
     H*                  Corrected Attorney orders screen to show -ve value                *
     H*                  in remaining field.                                               *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw4343f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmaop     if   e             disk
     F                                     rename(pao01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmaol     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pao01:dbrcdf)
     FWMAOL1    IF   E           K DISK
     F                                     RENAME(PAO01:PAO01R)
     FWMAOL3    IF   E           K DISK
     F                                     RENAME(PAO01:PAO03R)
     F                                     prefix(x)
     Fsmcol     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmpdl     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwmupl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmcxl     if   e           k disk
     Fwdpal     if   e           k disk
     Fssylf     if   e           k disk
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Attorney Orders'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W4343R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   11
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
     D @@call          s              1
      *
     D scdate1         s              8  0
     D scstdt1         s              8  0
     D scendt1         s              8  0
     D scname1         s             30
     D kkco#           s                   like(aoco#)
     D kkfnd           s                   like(aofnd)
     D kkfyr           s                   like(aofyr)
     D kkcase          s                   like(aocase)
     D kkordt          s                   like(aoordt)
     D kkfein          s                   like(aofein)
     D kkseq#          s                   like(aoseq#)
     D xxss#           s                   like(cdss#)
     D xxfein          s                   like(aofein)
     D xxmod           s                   like(aomod)
     D xxordt          s                   like(aoordt)
     D xxseq#          s                   like(aoseq#)
      *
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C* Load the header fields
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    qqcall            1
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
     C                   parm                    qqseq#            5
     C                   parm                    qqrtcd            1
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
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
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkordt
     C                   kfld                    kkfein
      *
     C     key2          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
      *
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
      *
     C     cxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
      *
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     aoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqfyr         kkfyr
     C                   move      qqcase        kkcase
     C                   eval      kkordt = *loval
     C                   eval      kkfein = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqfyr         kkfyr
     C                   move      qqcase        kkcase
     C                   eval      kkordt = sxordt
     C                   eval      kkfein = sxfein
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C
     C                   Z-ADD     AOORDT        xxordt
     C                   movel     AOSEQ#        xxSEQ#
     C                   Z-ADD     AOORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scdate1
      *
     C                   Z-ADD     AOSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scstdt1
     C*
     C                   Z-ADD     AOENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scendt1
      *
     C                   MOVE      AOFEIN        XXFEIN
     C                   MOVE      AOMOD         XXMOD
     C                   EXSR      GETNM
     C                   MOVEL     XXATNM        scname1
      *
     C                   eval      scdata = %editc(scdate1:'Y') +
     c                                      '   ' + (scname1) +
     C                                      ' ' +
     C                                      %editc(scstdt1:'Y') +
     C                                      '   ' + %editc(scendt1:'Y')
     C                   eval      sxordt = aoordt
     C                   eval      sxfein = aofein
     C                   eval      sxseq# = aoseq#
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   if        *inkp = *on
     C                   if        keyord = 'A'
     C                   eval      keyord = 'D'
     C                   eval      kkordt = *hival
     C                   eval      kkfein = *hival
     C*                  eval      kkseq# = *hival
     C                   else
     C                   eval      keyord = 'A'
     C                   eval      kkordt = *loval
     C                   eval      kkfein = *loval
     C*                  eval      kkseq# = *loval
     C                   endif
     C                   exsr      clrget
     C                   endif
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     qqcall        comp      '1'                                    09    inq mode
     C                   move      qqfnd         scfnd
     C                   move      qqfyr         scfyr             3 0
     C                   move      qqcase        sccase            7 0
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqfyr         kkfyr
     C                   move      qqcase        kkcase
      *
     c     cmky          chain     pcm01                              60
     c                   move      cmss#         xxss#
     c                   z-add     cmacdt        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scacdt
      *
     c     cdky          chain     pcd01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C*
     C                   MOVE      '1'           QQTERM
     C                   MOVE      *BLANKS       QQLSN            56
     C                   MOVEL     CDLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         SCCLNM
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****              MOVE      CDSS#         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CDSS#':CDSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   MOVE      *ZEROS        SCSS#
     C                   END
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   END
      *
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C*   POPULATE TOTAL PAID TO SCREEN FIELD
     C                   Z-ADD     *ZEROS        ##TOTP           11 2
     C     CMKY          SETLL     WDPAL
     C     CMKY          READE     WDPAL                                  60
     C     *IN60         DOWEQ     *OFF
     C     CXKY          CHAIN     WMCXL                              62
     C     *IN62         IFEQ      *OFF
     C     PAADST        ANDEQ     *BLANKS
     C     PABR2         IFEQ      CXAPV1
     C     PABR2         OREQ      CXAPV2
     C     PABR2         OREQ      CXAPV3
     C     PABR2         OREQ      CXAPV4
     C     PABR2         OREQ      CXAPV5
     C     PAAMT         ADD       ##TOTP        ##TOTP
     C                   ENDIF
     C                   ENDIF
     C     CMKY          READE     WDPAL                                  60
     C                   ENDDO
     C                   Z-ADD     ##TOTP        SCPAID
     C                   movel     'A'           keyord            1
     c                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   if        keyord = 'A'
     C     key2          reade     dbrcdf                                 95
     C                   else
     C     key2          reade     pao01r                                 95
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     c                   if        keyord = 'A'
     C     key2          readpe    dbrcdf                                 95
     C                   else
     C     key2          readpe    pao01r                                 95
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C                   if        keyord = 'A'
     C     key1          setgt     dbrcdf                             96
     C                   else
     C     key1          setgt     pao01r                             96
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     c                   exsr      totcap
     C                   if        *in21 = *on
     C                   if        keyord = 'A'
     C     key1          setll     dbrcdf                             96
     C                   else
     C     key1          setll     pao01r                             96
     C                   endif
     C                   endif
     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr
    C*================================================================
    C* totcap - Calculate the total cap for the claim
    C*================================================================
     C     totcap        BEGSR
     C                   Z-ADD     *ZEROS        ##PTOP
     C     AOKY          SETLL     PAO03R
     C     AOKY          READE     PAO03R                                 95
     C     *IN95         DOWEQ     *OFF
     C                   ADD       xaoptop       ##PTOP           11 2
     C     AOKY          READE     PAO03R                                 95
     C                   ENDDO
     C                   Z-ADD     ##PTOP        SCTCAP
     C     ##PTOP        IFGT      *ZEROS
     C     ##PTOP        SUB       ##TOTP        SCREMN
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCREMN
     C                   ENDIF
      *
     C                   ENDSR
     C************************************************************
     C*  SUBROUTINE TO GET PHYSICIAN/HOSPITAL NAME AND ADDRESS   *
     C************************************************************
     C     GETNM         BEGSR
     C                   MOVEL     *BLANKS       XXATNM           30
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXATNM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         XXATNM
     C                   CAT       PDFSN:1       XXATNM
     C                   END
     C*
     C                   ENDIF
     C                   ENDSR
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   endsr
** Column headings
Opt   Order Date  Attorney Name                   Start Date - End Date
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYY
205F5=Refresh               NNN
206F6=Add                   YNN
212F12=Cancel               YYY
215F15=Asc/Desc             YNN
3
