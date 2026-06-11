     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H/TITLE SR42304AI - Additional Entry - Texas Endorsements WC420304A
      *----------------------------------------------------------------- *
      *                                                                   *
      *  Called From: SR42304A                                            *
      *                                                                   *
      *----------------------------------------------------------------- *
      *                                                                   *
      * Created   : 03/26/13                                              *
      * Programmer: C: 287                                                *
      * Project   : WC2194                                                *
      *                                                                   *
      *----------------------------------------------------------------- *
      *Revisions:                                                        *
      *                                                                   *
      * 06/28/19 C:295 Support: 80594 (SUNZ)                              *
      *                To recompile with correct option to open the file  *
      *                SR42304AF correctly.                               *
      *                                                                   *
      *                                                                   *
      *----------------------------------------------------------------- *

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR42304AF cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
      **Additional Information File
     FWXFMP1    uf a e           k disk
      **Group Master File
     FWMFNL     if   e           k disk
      **Employer Master File
     FWMEML     if   e           k disk
      **Employer Detail File
     FWDELL1    if   e           k disk
     F                                     infds(elfdb)
    
      *----------------------------------------------------------------- *
      */COPY 'D' specs
      *----------------------------------------------------------------- *
    
     D/COPY SCOPYPR01D
    
      *----------------------------------------------------------------- *
      *Stand Alone Fields
      *----------------------------------------------------------------- *

     D optnm           c                   const('Additional information for WC+
     D                                     420304')
     D comapi          c                   const('WA001R')

     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3

     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0

     D xxfrm1          s             15
     D xxcall          s                   like(@@call)
     D xxForm          s             10    inz('WC420304A ')
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxfyr           s              3  0
     D xxedn           s             80
     D elfdb           ds
     D elrrn                 397    400b 0
    
     C/COPY SCOPYDTPL
    
      *----------------------------------------------------------------- *
      * PLIST definitions
      *----------------------------------------------------------------- *

      **Entry parameters
     C     *entry        Plist
     C                   Parm                    @@prds
     C                   Parm                    @@call
     C                   Parm                    @@co#
     C                   Parm                    @@fnd
     C                   Parm                    @@emp#
     C                   Parm                    @@div
     C                   Parm                    @@fyr

      **Company file/fund (group) retrieve program parameter list
     C     compl         Plist
     C                   Parm                    ##co#
     C                   Parm                    ##fnd
     C                   Parm                    ##conm
     C                   Parm                    ##fnam

      *-------------------------------------------------------------- *
      * KLIST definitions
      *-------------------------------------------------------------- *

      **Employer Master file
     C     emky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv

      **Additional Information file
     C     fmky1         Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv
     C                   Kfld                    xxfrm1
    
      **Group Master file
     C     fnky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd

      **Employer Detail file
     C     elky          Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxfyr
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv

      *-------------------------------------------------------------- *
      *Main
      *-------------------------------------------------------------- *
    
     C/COPY SCOPYPR01C
    
      *-------------------------------------------------------------- *
      *DSCAPI -Call API for getting descriptions (use 91 on error)
      *-------------------------------------------------------------- *

     C     DSCAPI        Begsr
    
     C                   Call      comapi        compl                  91
    
     C                   Endsr

      *----------------------------------------------------------------- *
      *ERRCK1 -Error checking for srceen 1 (SCR01)
      *----------------------------------------------------------------- *

     C     ERRCK1        Begsr
    
     C                   Exsr      othfks
    
     C                   Endsr

      *----------------------------------------------------------------- *
      *KJDO -Processing for F10
      *----------------------------------------------------------------- *

     C     KJDO          Begsr

     C                   Endsr

      *----------------------------------------------------------------- *
      *OTHFKS -Processing for other functions keys
      *----------------------------------------------------------------- *

     C     OTHFKS        Begsr
    
     C     *inkj         Caseq     *on           kjdo                     90
     C                   Endcs
    
     C                   Endsr

      *----------------------------------------------------------------- *
      *PGMEND -End of program processing
      *----------------------------------------------------------------- *

     C     PGMEND        Begsr

     C                   If        *inkc = *on OR
     C                             *inkl = *on
     C                   Eval      @@call = 'F'
     C                   Endif

     C                   Endsr

      *----------------------------------------------------------------- *
      *PGMSTR -Used for any start of program processing (one-time)
      *----------------------------------------------------------------- *

     C     PGMSTR        Begsr
    
     C                   If        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   Movel(p)  @@prds        ##data
     C                   Else
     C                   Movel(p)  optnm         ##data
     C                   Endif
    
     C                   Call      'SRCENTER'
     C                   Parm                    ##data
     C                   Parm      40            ##leng
    
     C                   If        q1prms >= 2
     C                   Movel     @@call        xxcall
     C                   Else
     C                   Clear                   xxcall
     C                   Endif
    
     C                   Eval      xxfyr = 0
     C                   Move      @@co#         xxco#
     C                   Move      @@fnd         xxfnd
     C                   Move      @@emp#        xxemp#
     C                   Move      @@div         xxdiv
     C                   Move      @@fyr         xxfyr

     C                   Eval      scfnd = xxfnd
     C                   Eval      scfyr = xxfyr
     C                   Eval      xxfrm1 = xxform

     C     fnky          Chain     pfn01                              60
     C                   Eval      scfnnm = fnname

     C     emky          Chain     pem01                              60

     C                   Call      'WA005R'
     C                   Parm                    ememp#
     C                   Parm                    emdiv
     C                   Parm                    emname
     C                   Parm                    xxedn

     C                   Eval      scedn = xxedn

     C                   Endsr

      *----------------------------------------------------------------- *
      *PREDSP -Pre-processing before displaying (every time)
      *----------------------------------------------------------------- *

     C     PREDSP        Begsr

      **Get endorsement additional information
     C     fmky1         Chain     pfm01                              60

     C                   If        *in60 = *off

      **Move the custom fields to the screen
     C                   Eval      scsbwv = f3fl03
     C                   Eval      scopn1 = f3c401
     C                   Eval      scopn2 = f3c402
     C                   Eval      scprem = f3a921
     C                   Eval      scppre = f3a521
     C                   Eval      scapre = f3a923

     C                   Else

     C                   Eval      scsbwv = *blanks
     C                   Eval      scopn1 = *blanks
     C                   Eval      scopn2 = *blanks
     C                   Eval      scprem = *zeros
     C                   Eval      scppre = *zeros
     C                   Eval      scapre = *zeros

     C                   Endif

     C                   Endsr

      *----------------------------------------------------------------- *
      *PRSCTL -Process control after no errors found (Call Submitter)
      *----------------------------------------------------------------- *

     C     PRSCTL        Begsr

     C     fmky1         Chain     pfm01                              60

     C                   If        *in60 = *on
     C                   Clear                   pfm01
     C                   Endif

      **Move the additional information fields to the file
     C                   Eval      f3co# = xxco#
     C                   Eval      f3fnd = xxfnd
     C                   Eval      f3fyr = xxfyr
     C                   Eval      f3emp# = xxemp#
     C                   Eval      f3div = xxdiv
     C                   Eval      f3form = xxform
     C                   Eval      f3fl03 = scsbwv
     C                   Eval      f3c401 = scopn1
     C                   Eval      f3c402 = scopn2
     C                   Eval      f3a921 = scprem
     C                   Eval      f3a521 = scppre
     C                   Eval      f3a923 = scapre

     C   60              Write     pfm01
     C  N60              Update    pfm01

     C                   If        scsbwv = 'S'

     C     elky          Chain     pel01                              60

     C                   Call      'W0815R'
     C                   Parm      *off          qqkc              1
     C                   Parm      *off          qqkl              1
     C                   Parm      ' '           qqcall            1
     C                   Parm      0             qqrrn             9 0
     C                   Parm      elrrn         qqrrn2            9 0
     C                   Parm                    q1pgm
     C                   Endif

     C                   Endsr

