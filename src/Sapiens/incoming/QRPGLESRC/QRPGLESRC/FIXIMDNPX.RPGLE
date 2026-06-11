      *====================================================================*
      * (C) Copyright Progressive Data Solutions, Inc., 1999               *
      *====================================================================*
     h/title FIXIMDNPX:
      *====================================================================*
      * Program Description:                                               *
      *  Deletes records prior to an input date to free up space at AMC.   *
      *                                                                    *
      * Called from: Command line                                          *
      * Calls to   : None                                                  *
      *                                                                    *
      * Created    : 12/08/99                                              *
      * Programmer : C:113                                                 *
      * Project    : DR091/87IHY                                           *
      *====================================================================*
      *====================================================================*
      * File Definitions                                                   *
      *====================================================================*
      ** Document Notes File
     FIMDNP     uf a e             disk
      ** Fax Transmission File
     FIAFTP     uf a e             disk

     Fqsysprt   O    f  132        printer oflind(*inof)
      *====================================================================*
      * Field Definitions                                                  *
      *====================================================================*
     D @@ckDate        s              8
     D @@Print         s              1
     D ChkDate         s              8  0 inz(19991001)
     D srdti           s              8  0
     D srffmt          s              8
     D srtfmt          s              8
     D srdto           s             10
     D srerr           s              1
     D svDate          s              8  0
     D xxcm75          s             75
     D xxDate6         s              6  0
     D xxDate8         s              8  0

     D                 ds
     D xxDate                         8  0
     D  xxcc                          2  0 overlay(xxDate:1)
     D  xxyy                          2  0 overlay(xxDate:3)

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D psds          esds                  extname(szq1p)
      *====================================================================*
      * PLIST definitions                                                  *
      *====================================================================*
      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@ckDate
     C                   parm                    @@Print

      ** Date Conversion parameters
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*YMD'        srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     dtblank       plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*YMD'        srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*YMD'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *====================================================================*
      * Main                                                               *
      *====================================================================*
     C                   time                    dsdttm

     C                   if        q1prms > 0
     C                   move      @@ckDate      srdti
     C                   call      'SRCVTDAT'    dtblank
     C                   movel     srdto         ChkDate
     C                   if        q1prms > 1
     C                   move      *on           *in70
     C                   endif
     C                   endif

     C                   exsr      CvtDates
     C                   exsr      ClearFT

     C                   move      *on           *inlr
     C                   return

      *====================================================================*
      * ClearFT - Subroutine to delete IAFTP records                       *
      *====================================================================*
     C     ClearFT       Begsr

     C   70              except    ##Head
     C   70              except    ##Head2

     C                   read      pft01                                  60
     C                   dow       *in60 = *off

     C                   if        FTFCDT <= ChkDate

     C                   eval      srdti = FTFCDT
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxDate8

     C   70              except    ##FaxDel
     C                   if        *inof = *on
     C   70              except    ##Head
     C   70              except    ##Head2
     C                   eval      *inof = *off
     C                   endif

     C                   Delete    pft01
     C                   EndIf                                                  FTFCDT <= ChkDate2
     C                   Read      pft01                                  60

     C                   EndDo

     C                   Endsr                                                  ClearFT
      *====================================================================*
      * CvtDates - Subroutine to correct the dates in IMDNP                *
      *====================================================================*
     C     CvtDates      begsr

     C   70              except    ##Head
     C   70              except    ##Head1

     C                   read      pdn01                                  60
     C                   dow       *in60 = *off

     C                   eval      xxDate = DNCDAT
     C                   eval      svDate = DNCDAT

     C                   if        xxcc > 12
     C                   eval      xxcc = 0
     C                   eval      xxDate6 = xxDate
     C                   else

     C                   if        xxyy > 31
     C                   eval      xxcc = 0
     C                   eval      xxDate6 = xxDate
     C                   else

     C                   eval      srdti = xxDate
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         xxDate6
     C                   endif
     C                   endif

     C                   eval      srdti = xxDate6
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDate8


     C                   if        *inof = *on
     C   70              except    ##Head
     C   70              except    ##Head1
     C                   eval      *inof = *off
     C                   endif

     C                   if        xxDate8 <= ChkDate
     C                   eval      srdti = xxDate8
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxDate8
     C                   eval      xxcm75 = DNCM75
     C                   Delete    pdn01
     C   70              except    ##Deleted
     C                   else

     C                   if        DNCDAT <> xxDate8
     C                   eval      DNCDAT = xxDate8
     C                   eval      srdti = xxDate8
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxDate8
     C                   eval      xxcm75 = DNCM75
     C                   update    pdn01
     C   70              except    ##Changed
     C                   endif

     C                   endif

     C                   read      pdn01                                  60
     C                   enddo

     C                   endsr

     Oqsysprt   e            ##Head         1 06
     O                       dsdate              15 '  /  /    '
     O                                           61 'IMAGING SYSTEM'
     O                       dstime              95 '  :  :  '
    
     O          e            ##Head         2
     O                       q1pgm               15
     O                                           61 'CLEAN UP FILES'
     O                                           90 'PAGE:'
     O                       page                95
    
     O          e            ##Head1        1
     O                                           10 'File IMDNP'

     O          e            ##Head1        1
     O                                            8 'STATUS'
     O                                           22 'NOTE DATE'
     O                                           33 'NOTE DATA'

     O          e            ##Head2        1
     O                                           10 'File IAFTP'

     O          e            ##Head2        1
     O                                            9 'STATUS'
     O                                           22 'FAX DATE'
     O                                           35 'RECIPIENT'
     O                                           49 'DESCRIPTION'

     O          e            ##Deleted      1
     O                                            9 '*DELETED'
     O                       xxDate8             22 '  /  /    '
     O                       xxCM75              99

     O          e            ##Changed      1
     O                                           11 '*CORRECTED'
     O                       xxDate8             22 '  /  /    '
     O                       xxCM75              99

     O          e            ##FaxDel       1
     O                                            9 '*DELETED'
     O                       xxDate8             22 '  /  /    '
     O                       FTRCID              36
     O                       FTDESC              78
