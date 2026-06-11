     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *******************************************************************
      * REVISIONS:
      *
      * 10/26/12 C:136 Support: WA114 80990
      *                Ensure the program returns an error if it finds one.
      *
      * 06/28/11  C:189 Seq#: WA025/76502
      *                      The error indicator was never being
      *                      initialized , resulting all the groups
      *                      coming as invalid
      *
      * 02/15/11 C:061 PROJECT WC1851
      *                Add EXFIRM error checking.
      *
      *******************************************************************
     *
     Fw7944f1   cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxgsp     if   e             disk    rename(pgs01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxgsl     if   e           k disk    rename(pgs01:dbrcdf)
     F                                     infds(dbfds)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(10) ctdata perrcd(1)
     D ofd             s             35    dim(10) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optNm           c                   'Group Setup Checking'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'GRPR02'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   7
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
     D @@Call          s              1
     D @@Type          s             10
     D @@Err           s              1n
     D kkco#           s                   like(GSCO#)
     D kkfnd           s                   like(GSFND)
      *** beg add ***** 10/26/12 *******************************************
     d xxReturnErr     s              1n   inz(*off)
      *** end add ***** 10/26/12 *******************************************
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
     C                   parm                    @@Call
     C                   parm                    @@Type
     C                   parm                    @@Err
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
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key           klist
     C                   kfld                    ##Co#
     C*
     C     key1          klist
     C                   kfld                    ##Co#
     C                   kfld                    kkFnd
     C*
    C*================================================================
    C* Check for errors within the group
    C*================================================================
     C     ChkError      begsr
     C*
     C                   eval      @@Err = *off
     C*
     C     key           setll     dbrcdf
     C                   dou       %eof(wxgsl)
     C     key           reade     dbrcdf
     C                   if        GSIEFL <> 'E'
     C* The following routines will need to turn on @@Err when issues exist
     C     @@Type        caseq     'EXDDRM'      ChkEXDDRM
      ****** beg add ***** 02/15/11 *************************************
     C     @@Type        caseq     'EXFIRM'      ChkEXFIRM
      ****** end add ***** 02/15/11 *************************************
     C                   endcs
     C                   endif
     C                   if        @@Err
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
    C*================================================================
    C* Check for EXDDRM without a value
    C*================================================================
     C     ChkEXDDRM     begsr
     C*
      ***** BEG ADD ******* 06/28/11 **********************************
     C                   eval      @@Err = *off
      ***** END ADD ******* 06/28/11 **********************************
     C                   eval      kkFnd = GSFND
     C     key1          chain     pex00
     C                   if        not %found
     C                              or EXDDRM = *blanks
     C                   eval      @@Err = *on
      *** beg add ***** 10/26/12 **************************************
     C                   eval      xxReturnErr = *on
      *** end add ***** 10/26/12 **************************************
     C                   endif
     C*
     C                   endsr
      ****** beg add ***** 02/15/11 *************************************
     C*
    C*================================================================
    C* Check for EXFIRM without a value
    C*================================================================
     C     ChkEXFIRM     begsr
     C*
     C                   eval      kkFnd = GSFND
     C     key1          chain     pex00
     C                   if        not %found
     C                              or EXFIRM = *blanks
     C                   eval      @@Err = *on
      *** beg add ***** 10/26/12 **************************************
     C                   eval      xxReturnErr = *on
      *** end add ***** 10/26/12 **************************************
     C                   endif
     C*
     C                   endsr
      ****** end add ***** 02/15/11 *************************************

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
     C*
     C                   clear                   kkFnd
     C*
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
     C*
     C                   eval      kkFnd = scFnd
     C*
     C                   endsr
     C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scFnd = GSFND
     C*
     C                   select
     C                   when      @@Type = 'EXDDRM'
     C                   eval      scData = %editc(GSFND:'Z') + '    ' +
      ***** BEG CHG ******* 06/28/11 **********************************
     C*************                  'Default DARTS code not set up for ' +
     C*************                  'deductible invoices.'
     C                               'Default DARTS code not set up in ' +
     C                               'Admin/Darts Control.'
      ***** END CHG ******* 06/28/11 **********************************
      ****** beg add ***** 02/15/11 *************************************
     C                   when      @@Type = 'EXFIRM'
     C                   eval      scData = %editc(GSFND:'Z') + '    ' +
     C                               'Default DARTS code not set up for ' +
     C                               'fee invoices.'
      ****** end add ***** 02/15/11 *************************************
     C                   endsl
     C*
     C                   endsr
     C*
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
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
     C                   call      'SRRMVMSG'                           99
      *** beg add ***** 10/26/12 **************************************
     c                   eval      @@err = xxReturnErr
      *** end add ***** 10/26/12 **************************************
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   call      'SRRMVMSG'                           99
     C*
     C                   exsr      ChkError
     C*
     C                   if        not @@Err
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
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
     C     key           readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
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
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C                   if        GSIEFL = 'E'
     C                   eval      meetSS = *off
     C                   leavesr
     C                   endif
     C*
     C     @@Type        caseq     'EXDDRM'      ChkEXDDRM
     C                   endcs
     C                   if        not @@Err
     C                   eval      meetSS = *off
     C                   leavesr
     C                   endif
     C*
     C                   endsr
** Column headings
Group  Explanation
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
