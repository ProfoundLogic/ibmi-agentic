     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0069R5 : EDI Claim Control Maintenance
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: EDI Claim Control Maintenance                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/24/10  PROGRAMMER: C:232 PROJECT: 139WNN         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/23/10 C:53  PROJECT: 139WNN/Priss# 70                     *
     H*                 Get the correct status for Automated flag     *
     H*
     H*  07/28/10 C:220 PROJECT: 139WNN/Priss# 41                     *
     H*                 If the status is changed to "I", delete the   *
     H*                 associated WMI1P record
     H*                 - Priss 44: Need to make WMI1L user open.
     H*
     H*  07/20/10 C:220 PROJECT: 139WNN/Priss# 37                     *
     H*                 Newly added records are not showing in the    *
     H*                 work with screen.                             *
     H*
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0069f5   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwmixp     if   e             disk
     F                                     rename(pix01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwmixl     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pix01:dbrcdf)
      *
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
      *
     F*  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *
      ***** beg chg ******* 07/28/10 **********************************
     F***WMI1L     IF   E           K DISK
     FWMI1L     UF A E           K DISK    usropn
      ***** end chg ******* 07/28/10 **********************************
      *
     F*  STATE MASTER
     FSTSTL3    IF   E           K DISK
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D hd1             s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(15) ctdata perrcd(1)
     D ofd             s             35    dim(15) alt(of)

    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Claim Control Maintenance'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0069R50'
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
     D kkco#           s                   like(ixco#)
     D kkjrst          s                   like(ixjrst)
     D kktrid          s                   like(ixtrid)
     D kkedif          s                   like(ixedif)
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
     C                   kfld                    kkjrst
     C                   kfld                    kktrid
     C                   kfld                    kkedif
      *
     C     GTKY          KLIST
     C                   KFLD                    ##co#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
      *
     C     IXKEY1        klist
     C                   kfld                    kkco#
     C                   kfld                    kkjrst
     C                   kfld                    kktrid
     C                   kfld                    kkedif
      *
     C     IXKEY2        klist
     C                   kfld                    ##co#
     C                   kfld                    IXJRST
     C                   kfld                    IXTRID
     C                   kfld                    IXEDIF
      *
     C     SYKY          KLIST
     C                   KFLD                    KKSYK
      *
    C*================================================================
    C*  *LIKE DEFN statements
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTTBID        #1TBID
     C     *LIKE         DEFINE    GTTBID        #2TBID
     C     *LIKE         DEFINE    GTTBID        #3TBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         #1KEY
     C     *LIKE         DEFINE    GTKEY         #2KEY
     C     *LIKE         DEFINE    GTKEY         #3KEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    STKEY         KKKEY
      *
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   IF        SSJRST <> ' ' OR  SSTRID <> ' '
     C                             OR SSEDIF <> ' ' OR SSAUTO <> ' '
     C                             OR SSSTAT <>  ' '
     C                   EVAL      *IN11 = *ON
     C                   ENDIF
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
      *
     C* Validation check.....
     C                   MOVE      '0'           *IN90
      *
     C* Option "6" and "7" can only be taken on a blank (master) transaction set
      *
     C                   IF        *IN90 <> '1'
     C                   IF        (SCTRID <> *BLANKS) AND ( SCSEL = '6' OR
     C                             SCSEL = '7')
     C                   SETON                                        9051
     C                   ENDIF
     C                   ENDIF
      *
      *If a 148 record already exists for this State/Format ID, do
      *not allow the option to be taken.
      *
     C                   IF        *IN90 <> '1'
     C                   IF        SCTRID = *BLANKS AND SCSEL = '6'
      *
     C                   MOVEL     UPCO#         KKCO#
     C                   MOVEL     SCJRST        KKJRST
     C                   MOVEL     '148'         KKTRID
     C                   MOVEL     SCEDIF        KKEDIF
      *
     C     IXKEY1        CHAIN     DBRCDF
     C                   IF        %FOUND(WMIXL)
     C                   SETON                                        9052
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *If a A49 record already exists for this State/Format ID, do
      *not allow the option to be taken.
      *
     C                   IF        *IN90 <> '1'
     C                   IF        SCTRID = *BLANKS and SCSEL = '7'
      *
     C                   MOVEL     UPCO#         KKCO#
     C                   MOVEL     SCJRST        KKJRST
     C                   MOVEL     'A49'         KKTRID
     C                   MOVEL     SCEDIF        KKEDIF
      *
     C     IXKEY1        CHAIN     DBRCDF
     C                   IF        %FOUND(WMIXL)
     C                   SETON                                        9052
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   IF        *IN90 <> '1'
     C                   IF        (SCTRID <> '148' and SCTRID <>'A49')
     C                             and (SCSEL = '8')
     C                   SETON                                        9053
     C                   ENDIF
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
      *
     c                   MOVE      '0'           *IN90
      *
     C                   exsr      OTHFKS
      *
     C*  EDI State must be in state table
      *
     C                   IF        *IN90 <> '1' AND SSJRST <> *BLANKS
     C                   MOVEL     *BLANKS       KKKEY
     C                   MOVEL     SSJRST        KKKEY
      *
     C     STKY          CHAIN     PST01                              60
     C                   IF        *IN60 = '1'
     C                   EVAL      *IN55 = '1'
     C                   EVAL      *IN57 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
      *
     C                   endif
      *
     C*  Validation of Transaction Set .......
      *
     C                   IF        *IN90 <> '1' AND SSTRID <> *BLANKS
      *
     C                   MOVEL     'TSET'        KKTBID
     C                   MOVEL     SSTRID        KKKEY
      *
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   ELSE
     C                   EVAL      *IN58 = '1'
     C                   EVAL      *IN63 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*  The EDI Format ID Must be in the table file if entered.
      *
     C                   IF        *IN90 <> '1' AND SSEDIF <> *BLANKS
     C                   IF        SSEDIF <> *BLANKS
     C                   MOVEL     'EDIF'        KKTBID
     C                   MOVEL     SSEDIF        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   ELSE
     C                   EVAL      *IN59 = '1'
     C                   EVAL      *IN64 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
     c                   ENDIF
      *
     C* Automate submission must be 'Y' or Blanks.
      *
     C                   IF        *IN90 <> '1' AND SSAUTO <> *BLANKS
     C                   IF        SSAUTO <> 'Y'
     C                   SETON                                        616590
     C                   ENDIF
     C                   ENDIF
      *
     C* STATUS must be 'A' or 'I'. . . . .
      *
     C                   IF        *IN90 <> '1' AND SSSTAT <> *BLANKS
     C                   IF        SSSTAT <> 'A' AND  SSSTAT <> 'I'
     C                   SETON                                        626690
     C                   ENDIF
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
      *
     C                   MOVEL     *BLANKS       SSJRST
     C                   MOVEL     *BLANKS       SSTRID
     C                   MOVEL     *BLANKS       SSEDIF
     C                   MOVEL     *BLANKS       SSAUTO
     C                   MOVEL     *BLANKS       SSSTAT
      *
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr
      *
     C                   EVAL      KKCO# = *LOVAL
     C                   EVAL      KKJRST = *LOVAL
     C                   EVAL      KKTRID = *LOVAL
     C                   EVAL      KKEDIF = *LOVAL
      *
     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr
      *
     C   21              EVAL      KKCO# = UPCO#
     C   21              MOVEL(P)  SCPST1        KKJRST
     C   21              MOVEL(P)  SCPST2        KKTRID
     C   21              MOVEL(P)  SCPST3        KKEDIF
      *
     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr
      *
     C   21              EVAL      KKCO# = 777
     C   21              MOVEL(P)  SCJRST        KKJRST
     C   21              MOVEL(P)  SCTRID        KKTRID
     C   21              MOVEL(P)  SCEDIF        KKEDIF
      *
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
      *
     C                   MOVEL     IXJRST        SCJRST
     C                   MOVEL     IXTRID        SCTRID
     C                   MOVEL     IXEDIF        SCEDIF
      *
      * Automated flag will be 'Y' if an Auto Submit record exists for
      * this State/Transaction set/Format ID.
      *
      ***** beg add ******* 07/28/10 **********************************
     C                   open      wmi1l
      ***** end add ******* 07/28/10 **********************************
     C     IXkEY2        CHAIN     PI101
      *** BEG CHG *** 09/23/10 **********************************
     C****************   IF        %FOUND(WMI1L)
     C                   IF        %FOUND(WMI1L) and I1AUTO = 'Y'
      *** END CHG *** 09/23/10 **********************************
     C                   MOVEL     'Y'           SCAUTO
     C                   ELSE
     C                   MOVEL     *BLANKS       SCAUTO
     C                   ENDIF
      *
     C                   MOVEL     IXSTAT        SCSTAT
     C                   IF        WCFL45 = 'Y'
     C                   EVAL      SCDATA = ' ' +SCJRST + '      ' +
     C                             SCTRID + '                ' +
     C                             SCEDIF + '              ' +
     C                             SCAUTO + '         ' +
     C                             SCSTAT
     C                   ELSE
     C                   EVAL      SCDATA = ' ' +SCJRST + '      ' +
     C                             SCTRID + '                ' +
     C                             SCEDIF + '           ' +
     C                             SCSTAT
     C                   ENDIF
      ***** beg add ******* 07/28/10 **********************************
     C                   close     wmi1l
      ***** end add ******* 07/28/10 **********************************
      *
     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
     C                   endsr
    C*================================================================
    C* KJDO   - Processing for F10
    C*================================================================
     C     kjdo          begsr
      *
     C                   MOVE      '0'           *IN55
     C                   MOVE      '0'           *IN58
     C                   MOVE      '0'           *IN59
     C                   MOVE      '0'           *IN61
     C                   MOVE      '0'           *IN62
      *
     C                   IF        HFLD = 'SSJRST'
     C                   CALL      'W2493R'
     C                   PARM                    #1PRDS           40
     C                   PARM      '2'           #1CALL            1
     C                   PARM      'JRST'        #1TBID
     C                   PARM      *BLANK        #1KEY
     C                   PARM      ' '           #1RTCD            1
      *
     C     #1RTCD        IFEQ      '*'
     C                   MOVEL     #1KEY         SSJRST
     C                   MOVE      *ON           *IN58
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   IF        HFLD = 'SSTRID'
     C                   CALL      'W2493R'
     C                   PARM                    #2PRDS           40
     C                   PARM      '2'           #2CALL            1
     C                   PARM      'TSET'        #2TBID
     C                   PARM      *BLANK        #2KEY
     C                   PARM      ' '           #2RTCD            1
      *
     C     #2RTCD        IFEQ      '*'
     C                   MOVEL     #2KEY         SSTRID
     C                   MOVE      *ON           *IN59
     C                   ELSE
     C                   MOVE      *ON           *IN58
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   IF        HFLD = 'SSEDIF'
     C                   CALL      'W2493R'
     C                   PARM                    #3PRDS           40
     C                   PARM      '2'           #3CALL            1
     C                   PARM      'EDIF'        #3TBID
     C                   PARM      *BLANK        #3KEY
     C                   PARM      ' '           #3RTCD            1
      *
     C     #3RTCD        IFEQ      '*'
     C                   MOVEL     #3KEY         SSEDIF
     C                   MOVE      *ON           *IN61
     C                   ELSE
     C                   MOVE      *ON           *IN59
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
      *
     C     SCSEL         CASEQ     '6'           OPT06
     C     SCSEL         CASEQ     '7'           OPT07
     C     SCSEL         CASEQ     '8'           OPT08
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* OPT06  - Create 148 ..........
    C*================================================================
     C     opt06         begsr
      *
     C                   call      'W0069R51'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      'P'           ##MODE
     C                   PARM      SCDBRR        ##RRN
      *
      ***** beg add ******* 07/20/10 **********************************
     C                   eval      autorf  = *on
      ***** end add ******* 07/20/10 **********************************
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* OPT07  - Create A49 ..........
    C*================================================================
     C     opt07         begsr
      *
     C                   CALL      'W0069R52'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      'P'           ##MODE
     C                   PARM      SCDBRR        ##RRN
      *
      ***** beg add ******* 07/20/10 **********************************
     C                   eval      autorf  = *on
      ***** end add ******* 07/20/10 **********************************
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* OPT08  - Auto submission . . .
    C*================================================================
     C     opt08         begsr
      *
     C                   CALL      'W0069R59'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      'C'           ##MODE
     C                   PARM      SCDBRR        ##RRN
     C                   PARM                    SCJRST
     C                   PARM                    SCTRID
     C                   PARM                    SCEDIF
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* OPT20  - Change status . . . .
    C*================================================================
     C     opt20         begsr
      *
      ***** beg add ******* 07/28/10 **********************************
     C                   open      wmi1l
      ***** end add ******* 07/28/10 **********************************
      *
     C                   EVAL       ##MODE = 'S'
     C                   CALL      ACDPGM        ACDPL                    99
      *
      ***** beg add ******* 07/28/10 **********************************
      *
     C     ixkey2        chain     dbrcdf
     C                   if        %found(wmixl) and ixstat = 'I'
      *
     C     ixkey2        chain     pi101
     C                   if        %found(wmi1l)
     C*
     C                   delete    pi101
     C*
     C                   endif
     C                   endif
     C*
     C                   eval      autorf  = *on
     C                   close     wmi1l
      ***** end add ******* 07/28/10 **********************************
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C     Q1USER        CHAIN     PUP01
     C                   IF        %FOUND
     C                   EVAL      KKCO# = UPCO#
     C                   ENDIF
      *
      * Automated field and option "8" will only display if the EDI
      * Auto Submission Control flag = 'Y'
      *
     C                   MOVEL     'WC'          KKSYK             2
     C     SYKY          CHAIN     SWC01
     C                   IF        %FOUND
      *
     C     WCFL45        IFEQ      'Y'
     C                   MOVEA     HD1           HD
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '08'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr
      *
     C                   IF        *IN21 = *ON
     C                   READ      DBRCDF                                 95
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr
      *
     C                   IF        *IN21 = *ON
     C                   READP     DBRCDF                                 95
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr
      *
     C                   IF        *IN21 = *ON
     C     KEY1          SETGT     DBRCDF                             96
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr
      *
     C                   IF        *IN21 = *ON
     C     KEY1          SETLL     DBRCDF                             96
     C                   ENDIF
      *
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
      *
     C                   IF        SSJRST <> ' '  and SSJRST <> IXJRST
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
      *
     C                   IF        SSTRID <> ' '  and SSTRID <> IXTRID
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
      *
     C                   IF        SSEDIF <> ' '  and SSEDIF <> IXEDIF
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
      *
     C                   IF        SSAUTO <> ' '  and SSAUTO <> IXAUTO
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
      *
     C                   IF        SSSTAT <> ' '  and SSSTAT <> IXSTAT
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
** Column headings
Opt   State   Transaction set    Format ID    Sts
** Column headings
Opt   State   Transaction set    Format ID    Automated    Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1066=Create 148             YYY
1077=Create A49             YYY
1088=Auto submission        NNN
12020=Change status         YYY
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
