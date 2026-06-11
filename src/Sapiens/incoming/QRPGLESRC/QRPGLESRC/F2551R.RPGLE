     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
    H**********************************************************************
    H* PROGRAM: F2551R
    H* DESCRIPTION: Work with NY Comorbidity Factors
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED:  6/18/2013  C:274  PROJECT: NY029
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H*  06/18/20 C: 079 Support: 99/PHW89489                              *
    H*                                                                    *
    H**********************************************************************
     H*
     Ff2551f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fftctpny   if   e             disk
     F                                     rename(mor01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fftctl1ny  if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(mor01:dbrcdf)
      /COPY SCOPYWW01D
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'NY Comorbidity Factors'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'FSMESG'
     D acdpgm          c                   'F2551R01'
     D comapi          c                   'FA007R'
     D uspapi          c                   'F0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
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
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     D kkicd9          s                   like(cticd9)
     D kkefdt          s                   like(ctefdt)
     D XXDATE          s                   like(ctefdt)
     D mdyDate         s               d   datfmt(*mdy)
     D isoCharDt       s              8
     D Up              c                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D Lo              c                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D u1pdsc          s                   like(CTDESC)
     D u2pdsc          s                   like(CTDESC)
     D strlen          s              3  0
     D start           s              3  0
     D patlen          s              3  0
     D trans           s              1
     D trim            s              1
     D wild            s              1
     D scnres          s              3  0
     D isoDate         s               d   datfmt(*iso)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@rrn
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkicd9
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C*
     C/COPY SCOPYWW01C
     C*
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
     C                   DO
     C*
     C     *INKJ         IFEQ      *ON
     C                   EXSR      OTHFKS
     C                   LEAVE
     C                   ENDIF
     C*
     C* VERIFY EFFECTIVE DATE IS A VALID CODE
     C*
     C  N90SSEFDT        IFNE      *ZEROS
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   ENDIF                                                   END SRERR
     C                   LEAVE
     C                   ENDIF                                                   END NE BLANK
     C*
     C* VERIFY STATUS
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF                                                   END SSSTAT
     C*
     C                   ENDDO
     C*
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
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C                   eval      kkicd9 = *loval
     C                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              eval      kkicd9 = scpst1
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkicd9 = SCICD9
     C                   eval      kkefdt = scefdt
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   eval      SCICD9 = CTICD9
     C                   eval      SCDESC = CTDESC
     C                   eval      SCEFDT = CTEFDT
     C
     C                   MOVE      CTEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD8
     C
     C                   eval      SCSTAT = CTSTAT
     C                   endsr
    C*================================================================
    C* OPT20 - Subroutine for option 20 = Change status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      @@kc = *inkc
     C                   eval      @@kl = *inkl
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C                   readp     dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
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
     C                   eval      @@rrn = scdbrr
     C                   endsr
     c*================================================================
     c* STRING - Subroutine to search strings
     c*================================================================
     c     string        begsr

     c                   call      'QCLSCAN'
     c                   parm                    u1pdsc
     c                   parm      40            strlen
     c                   parm      1             start
     c                   parm                    u2pdsc
     c                   parm      40            patlen
     c                   parm      '0'           trans
     c                   parm      '1'           trim
     c                   parm      ' '           wild
     c                   parm                    scnres

     c                   if        scnres = 0
     c                   eval      meetss = *off
     c                   endif

     c                   endsr
    c*================================================================
    c* SUBSET - Determine if record should be put in subfile
    c*================================================================
     c     subset        begsr

     c                   if        SSDESC <> *blanks
     c     Lo:Up         xlate(p)  CTDESC        u1pdsc
     c     Lo:Up         xlate(p)  SSDESC        u2pdsc
     c                   exsr      string
     C                   IF        SCNRES = 0
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
     c                   endif

     c                   if        SSEFDT <> *zero
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   IF        XXDATE <> *ZERO
     C                             AND    XXDATE  <> CTEFDT
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
     c                   endif

     C                   IF        SSSTAT <> *BLANK
     C                             AND SSSTAT <> CTSTAT
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF

     c                   endsr
** Column headings
Opt ICD-CM  Description                     Effective date        Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
