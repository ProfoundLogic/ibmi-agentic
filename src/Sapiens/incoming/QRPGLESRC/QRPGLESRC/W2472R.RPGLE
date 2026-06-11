     H****************************************************************
     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
     H****************************************************************
     H* Loss Cost Multiplier for Rate Set Code Maintenance           *
     H*                                                              *
     H*  10/20/04  C:132  PROJECT: WC1136                            *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2472f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdlmp     if   e             disk
     F                                     rename(plm01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdlml     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(plm01:dbrcdf)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Loss Cost Multiplier-Rate Set Code'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W2472R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
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
     D ##psdt          s              7  0
     D XXPSDT          s              8  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@RTCD          s              3
     D ##RTCD          s              3
     D @@JRST          s              2
     D ##JRST          s              2
     D kkco#           s                   like(lmco#)
     D kkrtcd          s                   like(lmrtcd)
     D kkefdt          s                   like(lmefdt)
     D xxefdt          s                   like(lmefdt)
      *
     d datusa          s               d   datfmt(*usa)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@JRST
     C                   parm                    @@RTCD
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    ##JRST
     C                   parm                    ##RTCD
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    kkco#
      *
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkefdt
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
     c                   eval      kkco#  = ##co#
     c                   eval      kkrtcd = *loval
     c                   eval      kkefdt = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     c                   eval      kkco#  = ##co#
     c                   eval      kkrtcd = scrtcd
     c                   eval      kkefdt = ##psdt
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     c                   eval      kkco#  = ##co#
     c                   eval      kkrtcd = scrtcd
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxEFDT
     c                   eval      kkefdt = xxefdt
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    
     c                   eval      scco#  = lmco#
     c   40              eval      scco#  = ##co#
     c                   eval      scrtcd = lmrtcd
     c                   eval      sclcm  = lmlcm
     c                   eval      scstat = lmstat
     C*
     C                   Z-ADD     LMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTmdyy
     C                   MOVEL     SRDTO         SCEFDT
    
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
      *
     c     scsel         caseq     '20'          opt20
     c                   endcs

     C                   endsr

      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         Begsr

     C                   Move      'S'           ##Mode
     C                   Call      ACDPGM        ACDPL                  99

     C                   Endsr

    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   Eval      *INLR = *ON
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   IF        @@CALL = '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C                   ENDIF                                                  SEARCH
     C                   IF        @@CALL = '2'
     C                   MOVE      *ON           *IN30
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       ##RTCD
     C                   EVAL      ##RTCD = @@RTCD
     C                   EVAL      ##JRST = @@JRST
     C                   EVAL      SCRTHD = @@RTCD
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C                   if        *in21 = *on
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
     C                   MOVE(P)   SCRTCD        @@RTCD
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
      *
     C                   IF        LMRTCD <> ##RTCD AND ##RTCD <> *BLANKS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   If        SCPST1  > *blanks
     C                   move      scpst1        XXPSDT
     C                   Z-ADD     XXPSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##PSDT
     C                   If        LMEFDT <> ##PSDT
     C                   MOVE      *OFF          MEETSS
     C                   EndIf
     C                   EndIf
      *
     C                   endsr
    C*================================================================
** Column headings
Opt  Rate set code   Effective date  Loss cost mulitplier    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
