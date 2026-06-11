     H*      Copyright Progressive Data Solutions, Inc., 2000
    H*
     H*/TITLE W8114R2 -
    H*********************************************************************
    H*  Description: Work With Madison Group Workfile.  Allows selection *
    H*               of multiple groups for download.                    *
    H*                                                                   *
    H*  Called from: W8114C1                                             *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: 05/01/00      C:87      Project: WC964                  *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  xx/xx/xx  C:xx  Project: xxxxx                                                    *
     H*                  Base project is installed as a mod for client.                    *
    H*                                                                                    *
    H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8114f2   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxfnp     if a e             disk
     F                                     rename(pfn01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwxfnl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pfn01:dbrcdf)
    F*
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Madison Download'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W8114R2M'
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
     D @@call          s              1
     D kkco#           s                   like(fnco#)
     D kkfnd           s                   like(fnfnd)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     D                 ds
     D xxdata                  1     70
     D   xxfnd                 1      3  0
     D   xxfnnm                6     60
     D   xxflag               58     65

    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

     C     fnky          klist
     C                   kfld                    kkco#
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr                                                  chkssm
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C                   endsr                                                  errck1
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr                                                  errck2
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr                                                  inzsst
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      *loval        kkfnd
     C*
     C                   endsr                                                  keyinz
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   move      scpst1        kkfnd
     C*
     C                   endsr                                                  keyset
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C*
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C*
     C                   endsr                                                  keysfl
    C*================================================================
    C* KGDO - Process F7=Submit
    C*================================================================
     C     kgdo          begsr
     C*
     C*  Read through file to make sure at least one group is included
     C     fnky          chain     dbrcdf                             71
     C                   dow       *in71=*off
     C                             and fnflag <> 'I'
     C     fnky          reade     dbrcdf                                 71
     C                   enddo
     C*
     C   71              eval      *in90 = *on
     C*
     C*    Go on only if no error
     C                   if        *in90 = *off
     C*
     C                   call      'W8115R1'
     C                   parm                    @@kc              1
     C                   parm                    @@kl              1
     C                   parm                    @@prds           40
     C                   parm                    @@call
     C*
     C                   if        @@kc = *on
     C                   eval      *inkc = *on
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr                                                  kgdo
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C*  Move values into hidden subfile fields
     C                   eval      scfnd = fnfnd
     C*
     C                   eval      xxfnd = fnfnd
     C                   eval      xxfnnm = fnname
     C                   if        fnflag='E'
     C                   eval       xxflag = 'Exclude'
     C                   else
     C                   eval       xxflag = 'Include'
     C                   endif
     C*
     C                   eval      scdata = xxdata
     C*
     C                   endsr                                                  mvdbsc
    C*================================================================
    C* Opt20 - Subroutine to change the status
    C*================================================================
     C     opt20         begsr
     C*
     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                  99
     C*
     C                   endsr                                                  opt20
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkg         caseq     *on           kgdo                       f7=submit
     C                   endcs
     C*
     C                   endsr                                                  othfks
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C                   endcs
     C*
     C                   endsr                                                  othopt
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr                                                  pgmend
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C*
     C                   endsr                                                  pgmstr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
     C*
     C                   if        *in21 = *on
     C     fnky          reade     dbrcdf                                 95
     C                   endif
     C*
     C                   endsr                                                  read
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
     C*
     C                   if        *in21 = *on
     C     fnky          readpe    dbrcdf                                 95
     C                   endif
     C*
     C                   endsr                                                  readp
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
     C*
     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif
     C*
     C                   endsr                                                  setgt
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
     C*
     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif
     C*
     C                   endsr                                                  setll
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr
     C*
     C*
     C                   endsr                                                  sf1upd
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C*
     C*
     C                   endsr                                                  slctrd
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*
     C*
     C                   endsr                                                  subset
** Column headings
Opt Group Description                                         Status
** Description--------------MISN
1011=Include/Exclude        NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Submit                YNN
212F12=Cancel               NNN
217F17=Subset               NNN
3
