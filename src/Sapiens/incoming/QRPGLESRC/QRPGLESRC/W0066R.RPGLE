     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0066R : Group EDI Info Work-with & Maintenance.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: Group EDI Info Work-with & Maintenance.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    None                                                       *
     H*  CALLS TO:                                                    *
     H*    W0066R02                                                   *
     H*                                                               *
     H*  CREATED: 07/02/10  PROGRAMMER: C:257 PROJECT: WC1790         *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 09/07/12 C:189 SUPPORT: 80675 WA099
     H*                UPDATION OPTIONS ARE AVAILABLE IN INQUIRY MODE
     H*                ON OPTION 29.
     H*
     H*  XX/XX/XX C:XXX  Project: XXXXXX
     H*
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     Fw0066f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdf3p     if   e             disk
     F                                     rename(pf301:pfrcdf)
    F* Group EDI information <F3>
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdf3l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pf301:dbrcdf)
    F* CO/FND/ST <F3>
    F*
    FSTSTP     if   e             disk
    F*
     Fwmfnp     if   e             disk
    F*
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
     D qca             s             70    dim(2) ctdata perrcd(1)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Submission Maintenance'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0066R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
     D deletd          c                   '*Deleted'
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
     D ##mode1         s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D @@fnd           s                   like(f3fnd)
     D ##rrn1          s              9
     D str             s              1  0
     D @@call          s              1
     D @@call1         s              1
     D kkco#           s                   like(f3co#)
     D kkfnd           s                   like(f3fnd)
     D kkkst           s                   like(f3kst)
     D length          s             15  5
     D command         s            255    inz(*blanks)
     D @@pgm           s             10
     D flag            s              1    inz('1')
     D flag2           s              1    inz('0')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*---------------------------------------------------------------------
    D* Data structure
    D*---------------------------------------------------------------------
    Ddlt              ds
    Dgrp                      1      3
    Dstate                    9     10
    Dname                    24     60
    Dstatus                  66     66
    D*
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *INKC         ##kc
     C                   parm      *INKL         ##kl
      ***** BEG CHG ******* 09/07/12 **********************************
     C*********          parm                    ##call
     C                   parm                    @@call
      ***** END CHG ******* 09/07/12 **********************************
     C                   parm                    ##mode
     C                   parm                    ##rrn2
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm                    flag2
     C                   parm                    ##rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C* Group search
     C     acdpl2        plist
     C     *INKC         parm                    ##KC
     C     *INKL         parm                    ##KL
     C                   parm                    ##MODE
     C                   parm                    XACO#             3
     C                   parm                    XAFND             3
     C                   parm                    XAFYR             3
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkst
     C*
     C     key2          klist
     C*                  kfld                    kkco#
     C                   kfld                    kkkst
     C*
     C*Key for WMFNL
     C     WMFK          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C
     C
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C
     C                   IF        ssfnd1 <> 0 or sskst1 <> ' '
     C                   move      *on           *IN11
     C                   ENDIF
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
     C     *inkj         ifeq      *on
     C                   select
     C* When cursor is on Group
     C     HFLD          wheneQ    'SSFND1'
     C                   move      ##co#         xaco#
     C                   move      *zero         xafyr
     C                   move      '2'           ##mode
     C                   call      'W0010R'      acdpl2                 99
     C                   move      xafnd         SSFND1
     C                   move      '1'           *in90
     C                   if        xafnd <>  *zero                               Cursor position
     C                   move      '1'           *in88
     C                   endif
     C*
     C*
     C* When cursor is on State
     C     HFLD          wheneQ    'SSKST1'
     C                   move      '1'           *in23
     C                   move      ##co#         xaco#
     C                   move      *zero         xafnd
     C                   move      *zero         xafyr
     C                   move      '2'           ##mode
     C                   call      'SY601R'
     C                   parm      '2'           ##CALL            1
     C                   parm      '0'           ##KC
     C                   parm      '0'           ##Kl
     C                   parm      *zeros        ##RRN
     C*
     C     ##RRN         ifeq      *ZERO
     C                   seton                                        33
     C                   move      '1'           *in90
     C                   else
     C     ##RRN         chain     PST01                              99
     C                   movel     STKEY         SSKST1
     C                   seton                                        33
     C                   move      '1'           *in90
     C                   endif
     C*
     C                   endsl
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   clear                   SsFND1
     C                   move      *zero         SsFND1
     C                   move      *blanks       SsKST1
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
     C                   eval      kkfnd  = *loval
     C                   eval      kkkst  = *loval
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
     C   21              move      scpst1        kkfnd
     C                   if        *in21 = *on
     C                   move      scpstd        kkkst
     C                   endif
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
     C                   eval      kkfnd = scfnd
     C                   movel     sckst         kkkst
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   clear                   scdata
     C*                  movel     F3CO#         SCCO#
     C                   movel     F3NAME        SCNAME
     C                   movel     F3FND         SCFND
     C                   movel     F3KST         SCKST
     C                   movel     F3STAT        SCSTAT
     C
     C                   movel     F3NAME        name
     C                   movel     F3FND         grp
     C                   movel     F3KST         state
     C                   movel     F3STAT        status
     C                   movel     dlt           scdata
     C                   endsr
    C*================================================================
    C* Opt9  - Copy
    C*================================================================
     C     opt9          begsr
     C* create dataarea W0066R1 in qtemp
     C                   if        flag = '1'
     C                   eval      length = 70
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(1)
     C                   parm                    length
     C                   eval      flag = '0'
     C                   endif
     C* change dataarea with RRN value
     C                   eval      ##rrn = scdbrr
     C                   move      ##rrn         ##rrn1
     C                   move      1             str
     C                   dow       str <= 9
     C                   if        %subst(##rrn1:str:1) = '0'
     C                   eval      %subst(##rrn1:str:1) = ' '
     C                   else
     C                   leave
     C                   endif
     C                   eval      str = str + 1
     C                   enddo
     C*
     C                   eval      command =
     C                             'CHGDTAARA DTAARA(QTEMP/W0066R1 *ALL)'
     C                             + ' VALUE('
     C                             + %trim(##rrn1) + ')'
     C*
     C                   call      'QCMDEXC'                              60
     C                   parm                    command
     C                   parm                    length
     C*
     C                   call      'W7944C'                               60
     C*                  parm      *blanks       @@call1
     C                   parm      'W0066R1'     @@pgm
     C*
     C* Delete data area
     C*
     C                   eval      flag = '1'
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(2)
     C                   parm                    length
     C                   endsr                                                  opt9
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
    C* THFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '9'           opt9
     C                   endcs
     C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   call      comapi        compl
     c                   movel     ##co#         kkco#
     C
     C                   if        ##mode  = 'X'                                called from WWGroup
     C                   eval      ##mode1 = ##mode
     C                   move      '1'           xxopfn
     C                   move      'NNN'         xxmis
     C                   move      '09'          xxnmbr
     C                   move      '1'           flag2
     C                   eval      ##rrn  = ##rrn2
     C                   exsr      updofa
     C*
     C     ##rrn2        chain     wmfnp
     C
     C                   endif
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
     C                   eval      scdata = '   *Deleted   '
     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   move      scdbrr        ##rrn
     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C                   SELECT
     C                   WHen      ##mode = 'X'
     C                   movel     FNFND         ssfnd1
     C                   if        ssfnd1 <> f3fnd
     C                   move      *off          meetss
     C                   endif
     C                   WHen      ssfnd1 <> 0  and
     C                             ssfnd1 <> f3fnd
     C                   MOVE      *OFF          MEETSS
     C                   WHen      sskst1 <> ' ' and
     C                             sskst1 <> f3kst
     C                   MOVE      *OFF          MEETSS
     C                   endSL
     C                   endsr
** Column headings
Opt   Group   State           Claim Admin Name                      Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1099=Copy                   YNN
12020=Change Status         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
**  (QCMDEX)
CRTDTAARA DTAARA(QTEMP/W0066R1) TYPE(*DEC) LEN(9 0) VALUE(00)
DLTDTAARA DTAARA(QTEMP/W0066R1)
