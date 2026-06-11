     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *========================================================================
     H/TITLE SC960R - Print All Remarks Prompt with date and remark select
      *========================================================================
      * DESCRIPTION:
      *  This program will allow the user to choose a date range, summary
      *  remarks, and/or remark codes to include in the report.
      *
      * CREATED   : 06/03/03
      * PROGRAMMER: C:128
      * PROJECT   : 129WAO
      *========================================================================
      * REVISIONS:
      *
      * 11/16/15  C:295 Support: 33695
      *                 Claims Aggregation - Print of all DARTS and attachments
      *
      * 03/10/09  C:33  Project: DR298
      *                 Add group/employer security.
      *
      * 05/23/08 C:65 Supp#63565
      *               compile over new file format.
      *
      * 04/22/06 C:02 xxx
      *               added input parm value 'D' for edebug to force use of "F"
      *
      * 03/14/05 c:62 Project 129IJS
      *               Change the query selection of remark code and
      *               summary code to an OR
      *
      * ----------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ----------------------------------------------------------------------
      *========================================================================
      * FILE USAGE:
      *
      *  File    Description                    Rcdfmt  Key
      *
      *========================================================================
      * INDICATOR USAGE:
      *  Global Indicators
      *   60 - General chain.
      *   90 - General error.
      *
      *  Error Indicators
      *
      *
      *========================================================================
      * NOTES:
      *
      *
      *========================================================================

      *========================================================================
      * File Definitions
      *========================================================================
      * Must use INFDS(WSDS) on workstation for FKeys to work.
     FSC960F    CF   E             WORKSTN INFDS(WSDS)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwmemp     if   e             disk
     Fwmeml     if   e           k disk    rename(pem01:pem02)
     F                                     infds(dbfds)
     Fwdelp     if   e             disk
     Fwdell     if   e           k disk    rename(pel01:pel02)
     Fwmfnl     if   e           k disk
     F                                     infds(dbfds1)
     Fwmcmp     if   e             disk
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02)
     F                                     infds(dbfds2)
     Fimpsl1    if   e           k disk
     Fstctl1    if   e           k disk
     Fstsml     if   e           k disk
     Fsmupl     if   e           k disk
     Fswaol     uf a e           k disk

      * Program-specific includes
      /COPY SC960RPR

     D***  Begin Add  ***  03/10/09  **************************************
     DGetRestrict      pr             1n
     D inCo#                          3  0 Const
     D inUser                        10    const
     D inAppl                         1    const
     D*
     DRestricted       pr             1n
     D inCo#                          3  0 const
     D inUser                        10    const
     D inAppl                         1    const
     D inGrp                          3  0 const
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     D***  End   Add  ***  03/10/09  **************************************

      *========================================================================
      * Array Definitions
      *========================================================================
      *
     DClm              s              2  0 dim(42)
     D                 ds
     DRmkArr                          5    dim(42)
     Dscrm01                   1      5
     Dscrm02                   6     10
     Dscrm03                  11     15
     Dscrm04                  16     20
     Dscrm05                  21     25
     Dscrm06                  26     30
     Dscrm07                  31     35
     Dscrm08                  36     40
     Dscrm09                  41     45
     Dscrm10                  46     50
     Dscrm11                  51     55
     Dscrm12                  56     60
     Dscrm13                  61     65
     Dscrm14                  66     70
     Dscrm15                  71     75
     Dscrm16                  76     80
     Dscrm17                  81     85
     Dscrm18                  86     90
     Dscrm19                  91     95
     Dscrm20                  96    100
     Dscrm21                 101    105
     Dscrm22                 106    110
     Dscrm23                 111    115
     Dscrm24                 116    120
     Dscrm25                 121    125
     Dscrm26                 126    130
     Dscrm27                 131    135
     Dscrm28                 136    140
     Dscrm29                 141    145
     Dscrm30                 146    150
     Dscrm31                 151    155
     Dscrm32                 156    160
     Dscrm33                 161    165
     Dscrm34                 166    170
     Dscrm35                 171    175
     Dscrm36                 176    180
     Dscrm37                 181    185
     Dscrm38                 186    190
     Dscrm39                 191    195
     Dscrm40                 196    200
     Dscrm41                 201    205
     Dscrm42                 206    210
      *
      *
     D                 ds
     DRprArr                          1    dim(42)
     Dscpr01                   1      1
     Dscpr02                   2      2
     Dscpr03                   3      3
     Dscpr04                   4      4
     Dscpr05                   5      5
     Dscpr06                   6      6
     Dscpr07                   7      7
     Dscpr08                   8      8
     Dscpr09                   9      9
     Dscpr10                  10     10
     Dscpr11                  11     11
     Dscpr12                  12     12
     Dscpr13                  13     13
     Dscpr14                  14     14
     Dscpr15                  15     15
     Dscpr16                  16     16
     Dscpr17                  17     17
     Dscpr18                  18     18
     Dscpr19                  19     19
     Dscpr20                  20     20
     Dscpr21                  21     21
     Dscpr22                  22     22
     Dscpr23                  23     23
     Dscpr24                  24     24
     Dscpr25                  25     25
     Dscpr26                  26     26
     Dscpr27                  27     27
     Dscpr28                  28     28
     Dscpr29                  29     29
     Dscpr30                  30     30
     Dscpr31                  31     31
     Dscpr32                  32     32
     Dscpr33                  33     33
     Dscpr34                  34     34
     Dscpr35                  35     35
     Dscpr36                  36     36
     Dscpr37                  37     37
     Dscpr38                  38     38
     Dscpr39                  39     39
     Dscpr40                  40     40
     Dscpr41                  41     41
     Dscpr42                  42     42
      *
      *
     D                 ds
     DSumArr                          4    dim(14)
     Dscsc01                   1      4
     Dscsc02                   5      8
     Dscsc03                   9     12
     Dscsc04                  13     16
     Dscsc05                  17     20
     Dscsc06                  21     24
     Dscsc07                  25     28
     Dscsc08                  29     32
     Dscsc09                  33     36
     Dscsc10                  37     40
     Dscsc11                  41     44
     Dscsc12                  45     48
     Dscsc13                  49     52
     Dscsc14                  53     56
      *
     D                 ds
     DSprArr                          1    dim(14)
     Dscps01                   1      1
     Dscps02                   2      2
     Dscps03                   3      3
     Dscps04                   4      4
     Dscps05                   5      5
     Dscps06                   6      6
     Dscps07                   7      7
     Dscps08                   8      8
     Dscps09                   9      9
     Dscps10                  10     10
     Dscps11                  11     11
     Dscps12                  12     12
     Dscps13                  13     13
     Dscps14                  14     14
     D                 ds
     DRmkDbA                          5    dim(42)
     Daorc01                   1      5
     Daorc02                   6     10
     Daorc03                  11     15
     Daorc04                  16     20
     Daorc05                  21     25
     Daorc06                  26     30
     Daorc07                  31     35
     Daorc08                  36     40
     Daorc09                  41     45
     Daorc10                  46     50
     Daorc11                  51     55
     Daorc12                  56     60
     Daorc13                  61     65
     Daorc14                  66     70
     Daorc15                  71     75
     Daorc16                  76     80
     Daorc17                  81     85
     Daorc18                  86     90
     Daorc19                  91     95
     Daorc20                  96    100
     Daorc21                 101    105
     Daorc22                 106    110
     Daorc23                 111    115
     Daorc24                 116    120
     Daorc25                 121    125
     Daorc26                 126    130
     Daorc27                 131    135
     Daorc28                 136    140
     Daorc29                 141    145
     Daorc30                 146    150
     Daorc31                 151    155
     Daorc32                 156    160
     Daorc33                 161    165
     Daorc34                 166    170
     Daorc35                 171    175
     Daorc36                 176    180
     Daorc37                 181    185
     Daorc38                 186    190
     Daorc39                 191    195
     Daorc40                 196    200
     Daorc41                 201    205
     Daorc42                 206    210
      *
      *
     D                 ds
     DRprDbA                          1    dim(42)
     Daorp01                   1      1
     Daorp02                   2      2
     Daorp03                   3      3
     Daorp04                   4      4
     Daorp05                   5      5
     Daorp06                   6      6
     Daorp07                   7      7
     Daorp08                   8      8
     Daorp09                   9      9
     Daorp10                  10     10
     Daorp11                  11     11
     Daorp12                  12     12
     Daorp13                  13     13
     Daorp14                  14     14
     Daorp15                  15     15
     Daorp16                  16     16
     Daorp17                  17     17
     Daorp18                  18     18
     Daorp19                  19     19
     Daorp20                  20     20
     Daorp21                  21     21
     Daorp22                  22     22
     Daorp23                  23     23
     Daorp24                  24     24
     Daorp25                  25     25
     Daorp26                  26     26
     Daorp27                  27     27
     Daorp28                  28     28
     Daorp29                  29     29
     Daorp30                  30     30
     Daorp31                  31     31
     Daorp32                  32     32
     Daorp33                  33     33
     Daorp34                  34     34
     Daorp35                  35     35
     Daorp36                  36     36
     Daorp37                  37     37
     Daorp38                  38     38
     Daorp39                  39     39
     Daorp40                  40     40
     Daorp41                  41     41
     Daorp42                  42     42
      *
      *
     D                 ds
     DSumDbA                          4    dim(14)
     Daosc01                   1      4
     Daosc02                   5      8
     Daosc03                   9     12
     Daosc04                  13     16
     Daosc05                  17     20
     Daosc06                  21     24
     Daosc07                  25     28
     Daosc08                  29     32
     Daosc09                  33     36
     Daosc10                  37     40
     Daosc11                  41     44
     Daosc12                  45     48
     Daosc13                  49     52
     Daosc14                  53     56
      *
     D                 ds
     DSprDba                          1    dim(14)
     Daosp01                   1      1
     Daosp02                   2      2
     Daosp03                   3      3
     Daosp04                   4      4
     Daosp05                   5      5
     Daosp06                   6      6
     Daosp07                   7      7
     Daosp08                   8      8
     Daosp09                   9      9
     Daosp10                  10     10
     Daosp11                  11     11
     Daosp12                  12     12
     Daosp13                  13     13
     Daosp14                  14     14

      *========================================================================
      * Variable Definitions
      *========================================================================
     D MAX_SCREEN      C                   CONST(01)
     D COMAPI          C                   Const('WA001R')
     D SUMSEARCH       C                   const('SC585R')
     D RMKSEARCH       C                   const('SC305R')
     D CLMSEARCH       C                   const('W0070R')
     D PIDSEARCH       C                   const('SC970R')
     D SumCodeList     C                   const('CMSMCD *EQ %VALUES("')
     D RemCodeList     C                   const('CMRMCD *EQ %VALUES("')
     D OptionName      C                   const('Complete DARTS File Print')
     D PDSNAME         C                   CONST('StoneRiver, Inc.')

     D cancelled       s              1a
     D callMode        s              1a
     D company         S              3  0
     D pCompany        S              3a
     D year            S              3  0
     D pYear           S              3a
     D case            S              7  0
     D pCase           S              7a
     D coName          S             40a
     D ended           s              1a
     D group           S              3  0
     D pGroup          S              3a
     D pEmployer       s              9a   inz('000000000')
     D groupName       S             40a
     D outq            s             10a
     D quote           s              1a
     D hGroup          S                   like(aofnd)
     D hYear           S                   like(aofyr)
     D hCase           S                   like(aocase)
     D hFdat           S                   like(scfdat)
     D hTdat           S                   like(sctdat)
     D hOtyp           S                   like(aootyp)
     D hOutq           S                   like(aooutq)
     D hPsid           S                   like(aopsid)
     D rrn             s              9  0
     D rrn2            s              9  0
     D Title           S             40a   Inz('Print All Remarks Process')
     D qtype           s              7a   inz('*OUTQ')
     D rtnError        s              1a
     D eDebug          s              1a
     D egroup          s                   like(cmfnd)
     D eyear           s                   like(cmfyr)
     D ecase           s                   like(cmcase)
     D debug           s              1a
     D*** begin add *** 04/22/06 ******************************************
     D eMode           s              1a
     D*** end   add *** 04/22/06 ******************************************
     D call            s              1a
     D sort            s              2a
     D sumkey          s              4a
     D rmkkey          s              5a
     D sumcode         s              4a
     D rmkcode         s              5a
     D rmkdesc         s             40a
     D row             s              5  0
     D col             s              5  0
     D s               s              2  0 Inz(0)
     D r               s              3  0 Inz(0)
     D x               s              2  0
     D xx              s              2  0
     D pfdat           s              7a
     D ptdat           s              7a
     D potyp           s              1a
     D pqry1           s            479a
     D psmcds          s             57a
     D prmcds          s            211a
     D pprnt           s              1a
     D rowPos          s              2  0
     D indNbr          s              2  0
     D clmInd          s              2  0
     D nxtInd          s              2  0
     D remain          s              2  0
     D confirmed       s              1a
     D holdquery       s            355a
     D update          s               n
     D ##data          s             80
     D ##leng          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D***  Begin Add  ***  03/10/09  **************************************
     D restrictW       s              1n
     D usrLvl          s              3  0
     D usrType         s              1
     D usrOutQ         s             10
     D usrXCo#         s              1
     D usrXFnd         s              1
     D usrXEmp#        s              1
     D***  End   Add  ***  03/10/09  **************************************

     D dbfds           ds
     D  dbrrn                397    400b 0

     D dbfds1          ds
     D  dbrrn1               397    400b 0

     D dbfds2          ds
     D  dbrrn2               397    400b 0

      *========================================================================
      * KList Definitions
      *========================================================================

     C     aoky          klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scfyr
     C                   kfld                    sccase

     C     sumky         klist
     C                   kfld                    sumkey

     C     rmkky         klist
     C                   kfld                    rmkkey


     C     cmkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scfyr
     C                   kfld                    sccase

     C     pskey         klist
     C                   kfld                    scpsid

      *========================================================================
      * PList Definitions
      *========================================================================
      * entry parameters
     C     *entry        plist
     C                   parm                    eDebug
     c** add 4/22/06 ************
     C                   parm                    eMode
     c** end add 4/22/06 *******
     C                   parm                    egroup
     C                   parm                    eyear
     C                   parm                    ecase

     C                   if        %PARMS > 0
     C                   eval      debug = eDebug
     C                   else
     C                   clear                   debug
     C                   endif

     c*** add 4/22/06 **********************************
     C                   eval      *in19 = *on
     C                   if        %PARMS > 1 and eMode = 'D'
     C                   eval      *in19 = *off
     C                   endif
     c*** end add 4/22/06 ******************************
     c*** CHG Nxt 4/22/06 ******************************
     C                   if        %PARMS > 2
     C**  end chg 4/22/06 *****************************
     C                   eval      scgrp = egroup
     C                   eval      scfyr = eyear
     C                   eval      sccase = ecase
     C                   seton                                        1632
     C                   callp     GetSwaop(' ')
     C                   else
     C                   clear                   scgrp
     C                   clear                   scfyr
     C                   clear                   sccase
     C                   setoff                                       1632
     C                   endif

      * company/fund api
     C     COMPL         PLIST
     C                   PARM                    company
     C                   PARM                    group
     C                   PARM                    coName
     C                   PARM                    groupName
     C***  Begin Add  ***  03/10/09  **************************************
     C                   parm                    usrLvl
     C                   parm                    usrType
     C                   parm                    usrOutQ
     C                   parm                    usrXCo#
     C                   parm                    usrXFnd
     C                   parm                    usrXEmp#
     C***  End   Add  ***  03/10/09  **************************************
    
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

      * summary code  select
     C     sumpl         plist
     C                   PARM      *blanks       sumcode
     C                   PARM      *blanks       rtnError

      * remark code  select
     C     rmkpl         plist
     C                   PARM      '2'           callMode
     C     *inkc         PARM                    ended
     C     *inkl         PARM                    cancelled
     C                   PARM      *blanks       rmkcode
     C                   PARM      *blanks       rmkdesc
     C                   PARM      *blanks       rtnError


      * Claim    select
     C     clmpl         plist
     C                   PARM      '2'           call
     C     *inkc         PARM                    ended
     C     *inkl         PARM                    cancelled
     C                   PARM      ' '           callMode
     C                   PARM      *zeros        rrn
     C                   PARM      'CA'          sort
     C                   PARM                    rrn2
     C                   PARM                    year

      * Printer id select
     C     pidpl         plist
     C                   PARM      '2'           callMode
     C     *inkc         PARM                    ended
     C     *inkl         PARM                    cancelled
     C                   PARM                    pPsid             2

      *========================================================================
      * Main
      *========================================================================
      * Get company
     C                   exsr      GETCOF

      * Center the program title.
     C                   Eval      SCPRDS = Center(Title)

     C                   CALLP     PgmStart

      ** Main program loop.
     C                   DoU       PgmStatus = EXIT

      ** Process the screens recursively.
     C                   If        confirmed <> 'N'
     C                   Eval      PgmStatus = ProcScreen( 1 : @@Mode )

     C                   If        PgmStatus = CANCEL
     C                             OR PgmStatus = EXIT
     C                   leave
     C                   endif
     C                   endif

     C                   Eval      confirmed = yes
     C                   Eval      PgmStatus = ProcScreen( 2 : @@Mode )

     C                   If        PgmStatus <> EXIT
     C                             AND PgmStatus <> CANCEL

      **   Confirm the screen selections, allow user to cancel
     C                   eval      isDone = AllDone(SCGRP:SCFYR:SCCASE)
     C                   If        isDone = YES
      **     We're done, so process the information.
     C                   Eval      PgmStatus = Finish( @@Mode )
     C                   EndIf

     C                   EndIf

     C                   EndDo

     C                   CALLP     PgmEnd

     C                   Eval      *inlr = *on
     C                   Return

      *========================================================================*
      * Subroutine Definitions                                                 *
      *========================================================================*


      *------------------------------------------------------------------------*
      *  GETAOP - Gets the SWAOP record and loads the screen fields
      *------------------------------------------------------------------------*
     C     getaop        begsr

     C     aoky          chain(n)  pao01                              99
B01  C                   if        *in99 = *off
     C*
     C                   movea     rmkDba        rmkArr
     C                   movea     rprDba        rprArr
     C                   movea     sumDba        sumArr
     C                   movea     sprDba        sprArr
     C*
E01  C                   endif

     C                   endsr

      *------------------------------------------------------------------------*
      *  GETCOF - Gets company and fund names if possible; otherwise
      *           get PDS name for company name
      *------------------------------------------------------------------------*
     C     getcof        begsr

     C                   CALL      ComAPI        compl                  99
B01  C                   if        *in99 = *on
     C                   eval      SCCONM = Center(PDSNAME)
     C                   else
     C                   MoveL     coName        SCCONM
     C                   if        scgrp = *zeros
     C                   Move      group         SCGRP
     C                   endif
E01  C                   endif
     C*
     C***  Begin Add  ***  03/10/09  **************************************
     C                   eval      restrictW = GetRestrict(company
     C                                                    :q1user
     C                                                    :'W'
     C                                                    )
     C*
     C                   select
     C                   when      usrXFnd = 'Y' and not restrictW
     C                   eval      *in40 = *on
     C                   other
     C                   eval      *in40 = *off
     C                   endsl
     C***  End   Add  ***  03/10/09  **************************************

     C                   endsr

      *========================================================================*
      * Procedure Definitions                                                  *
      *========================================================================*



      *------------------------------------------------------------------------*
      * CheckFkys - Checks for the press of a function key and branches program
      *------------------------------------------------------------------------*
     P CheckFkys       B

      ** Procedure Interface
     D CheckFkys       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   Select

     C                   When      Fkey = F3
     C                   Return    EXIT

     C                   When      Fkey = F12
     C                   Return    CANCEL

     C                   Other
     C                   Return    OtherFkys( Screen# : Mode : Fkey )

     C                   EndSL

     P CheckFkys       E


      *------------------------------------------------------------------------*
      * DspScreen - Explicitly execute format for each screen.
      *------------------------------------------------------------------------*
     P DspScreen       B

      ** Procedure Interface
     D DspScreen       PI
     D  Screen#                       2  0 Const

     C                   Select

     C                   When      Screen# = 1
     C                   EXFMT     SCR01
     C                   movea     '00000'       *in(31)

     C                   When      Screen# = 2
     C                   EXFMT     SCR02

     C                   EndSL

     P DspScreen       E



      *------------------------------------------------------------------------*
      *  rrorChk- This is the general error check function.
      *------------------------------------------------------------------------*
     P ErrorChk        B

      ** Procedure Interface
     D ErrorChk        PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   Select

     C                   When      Screen# = 1
     C                   Return    ErrorChk1(Screen#:Mode)

     C                   When      Screen# = 2
     C                   Return    ErrorChk2(Screen#:Mode)

     C                   EndSL

     P ErrorChk        E



      *------------------------------------------------------------------------*
      * ErrorChk1 - Check screen 1 for errors.
      *------------------------------------------------------------------------*
     P ErrorChk1       B

      ** Procedure Interface
     D ErrorChk1       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C     cmkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp
     C                   kfld                    scfyr
     C                   kfld                    sccase

     C     fnkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp

     C                   eval      *in90 = *off
     C                   eval      pfdat = '0000000'
     C                   eval      ptdat = '0000000'

      * year and case must be valid
     C     cmkey         chain     pcm02                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  75
     C                   endif
     C*
     C***  Begin Add  ***  03/10/09  **************************************
      * user must be authorized to case
     C  n90              if        Restricted(company
     C                                       :q1user
     C                                       :'W'
     C                                       :CMFND
     C                                       :CMEMP#
     C                                       :CMDIV
     C                                       )
     C                   eval      *in90 = *on
     C                   eval      *in81 = *on
     C                   endif
     C***  End   Add  ***  03/10/09  **************************************

      * Dates must be valid
     C     sctdat        ifgt      *zero

     C                   Eval      dateOut = CvtDate(scfdat:BLANK:DT_CYMD)
     C     dateOut       ifeq      *blanks
     C                   move      *on           *in90
     C                   move      *on           *in72
     C                   else
     C                   z-add     scfdat        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scfdat
     C*
     C                   MoveL     dateOut       pfdat
     C                   endif

     C                   Eval      dateOut = CvtDate(sctdat:BLANK:DT_CYMD)
     C     dateOut       ifeq      *blanks
     C                   move      *on           *in90
     C                   move      *on           *in72
     C                   else
     C                   z-add     sctdat        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctdat
     C*
     C                   MoveL     dateOut       ptdat
     C                   endif
      * from date must be <= todate.

     C     *in90         ifeq      *off
     C     pfdat         ifgt      ptdat
     C                   move      *on           *in90
     C                   move      *on           *in76
     C                   endif
     C                   endif

     C                   endif
     C
     C*** add 4/22/06 *****************************************
     C                   if         *in19
     C*** end add 4/22/06 ************************************

      * queue must not be blank and must be valid
     C  n90              if        scoutq = *blanks
     C                   seton                                        90  73
     C                   else
     C                   callp     CheckOutq(scoutq:qtype:rtnError)
     C                   if        rtnError = *on
     C                   seton                                        90  74
     C                   endif
     C                   endif

      * Output type must be 'P', 'F' or 'B'
     C                   Select
     C                   when      scotyp = 'P'
     C                   when      scotyp = 'F'
     C                   when      scotyp = 'B'
     C                   other
     C                   seton                                        90  71
     c                   EndSL

      * Print Server ID must be valid if entered
     C     scpsid        ifne      *blank
     C     pskey         chain     pps01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90  79
     C                   endif
     C                   endif
     C*** add 4/22/06 *********************************************
     c                   endif
     c*** end add 4/22/06 ****************************************

      * Update screen values if claim changes

     C  n90              if        hGroup <> scgrp or hYear <> scfyr or
     C                             hCase <> sccase
     C                   callp     getSwaop('E')
     C                   endif

     C  n90              return    0
     C   90              return    ERROR

     P ErrorChk1       E

      *------------------------------------------------------------------------*
      * ErrorChk2 - Check screen 2 for errors.
      *------------------------------------------------------------------------*
     P ErrorChk2       B

      ** Procedure Interface
     D ErrorChk2       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

      * Validate remark code and print flag
     C                   do        42            x

      * Determine row position (1 thru 14) and error indicator

     C     x             div       3             rowPos
     C                   mvr                     remain

     C     remain        ifne      *zero
     C                   eval      rowPos = rowPos + 1
     C                   endif

     C                   eval      indNbr = rowPos + 20

     C     RmkArr(x)     ifne      *blank
     C     RmkArr(x)     chain     pct01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90
     C                   move      *on           *in(indNbr)
     C                   move      clm(x)        clmInd
     C                   move      *on           *in(clmInd)
     C                   endif
     C                   endif

     C     RprArr(x)     ifne      'N'
     C     RprArr(x)     andne     'Y'
     C                   move      *on           *in90
     C                   move      *on           *in(indNbr)
     C                   move      clm(x)        clmInd
     C                   eval      nxtInd = clmInd + 1
     C                   move      *on           *in(nxtInd)
     C                   endif

     C                   enddo


      * Validate summary code and print flag
     C                   do        14            x

      * Determine error indicator

     C                   eval      indNbr = x + 20


     C     SumArr(x)     ifne      *blank
     C     SumArr(x)     chain     psm01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        90
     C                   move      *on           *in(indNbr)
     C                   move      *on           *in41
     C                   endif
     C                   endif

     C     SprArr(x)     ifne      'N'
     C     SprArr(x)     andne     'Y'
     C                   move      *on           *in90
     C                   move      *on           *in(indNbr)
     C                   move      *on           *in42
     C                   endif

     C                   enddo

     C  n90              return    0
     C   90              return    ERROR

     P ErrorChk2       E


     C***  Begin Add  ***  03/10/09  **************************************
      *------------------------------------------------------------------------*
      * Determine if restrictions exist
      *------------------------------------------------------------------------*
     PGetRestrict      b
     DGetRestrict      pi             1n
     D inCo#                          3  0 Const
     D inUser                        10    const
     D inAppl                         1    const

     D ##Co#           s              3  0
     D ##User          s             10
     D ##Appl          s              1
     D ##RtnCd         s              1n

     C                   call      'SA008R'
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd

     C                   return    ##RtnCd

     PGetRestrict      e
     C***  End   Add  ***  03/10/09  **************************************


      *------------------------------------------------------------------------*
      *  GETSWAOP - Gets the SWAOP record and loads the screen fields
      *------------------------------------------------------------------------*
     P GetSwaop        B

      ** Procedure Interface
     D GetSwaop        PI
     D  ermode                        1    Const

     C*
     C                   eval      hGroup = scgrp
     C                   eval      hYear = scfyr
     C                   eval      hCase = sccase
     C                   eval      hFdat = scfdat
     C                   eval      hTdat = sctdat
     C                   eval      hOtyp = scotyp
     C                   eval      hOutq = scoutq
     C                   eval      hPsid = scpsid
     C*
     C     aoky          chain(n)  pao01                              99
B01  C                   if        *in99 = *off
     C*
     C***** Beg Chg ****** 11/16/15 ********************************************
     C*****              if        aofdat = *zeros
     C*****              eval      scfdat = *zeros
     C*****              else
     C*****              Eval      dateOut = CvtDate(aofdat:DT_CYMD:DT_MDYY)
     C*****              MoveL     dateOut       scfdat
     C*****              endif
     C*****
     C*****              if        aotdat = *zeros
     C*****              eval      sctdat = *zeros
     C*****              else
     C*****              Eval      dateOut = CvtDate(aotdat:DT_CYMD:DT_MDYY)
     C*****              MoveL     dateOut       sctdat
     C*****              endif
     C***** End Chg ****** 11/16/15 ********************************************
     C*
     C                   eval      scotyp = aootyp
     C                   eval      scoutq = aooutq
     C                   eval      scpsid = aopsid
     C*
     C                   movea     rmkDba        rmkArr
     C                   movea     rprDba        rprArr
     C                   movea     sumDba        sumArr
     C                   movea     sprDba        sprArr
     C*
     C                   else
     C                   clear                   rmkArr
     C                   movea     *all'N'       rprArr
     C                   clear                   sumArr
     C                   movea     *all'N'       sprArr
E01  C                   endif
     C*
     C                   if        ermode = 'E'
     C                   if        hFdat <> scfdat or hTdat <> sctdat or
     C                             hOtyp <> scotyp or hOutQ <> scoutq or
     C                             hPsid <> scpsid
     C                   seton                                        90
     C                   endif
     C                   endif

     P GetSwaop        E


     C***  Begin Add  ***  03/10/09  **************************************
      *------------------------------------------------------------------------*
      * Determine if user is authorized to record
      *------------------------------------------------------------------------*
     PRestricted       b
     DRestricted       pi             1n
     D inCo#                          3  0 const
     D inUser                        10    const
     D inAppl                         1    const
     D inGrp                          3  0 const
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)

     D ##Co#           s              3  0
     D ##User          s             10
     D ##Appl          s              1
     D ##RtnCd         s              1n
     D ##Auth          s              1
     D ##Grp           s              3  0
     D ##Emp#          s              9  0

     C     authGrp       plist
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd
     C                   parm                    ##Auth
     C                   parm      inGrp         ##Grp

     C     authEmp#      plist
     C                   parm      inCo#         ##Co#
     C                   parm      inUser        ##User
     C                   parm      inAppl        ##Appl
     C                   parm                    ##RtnCd
     C                   parm                    ##Auth
     C                   parm      inGrp         ##Grp
     C                   parm      inEmp#        ##Emp#

     C                   eval      ##RtnCd = *off
     C                   select
     C                   when      usrXFnd = 'N' and
     C                             (%parms < 5 or inEmp# = 0)
     C                   eval      ##RtnCd = *off
     C                   when      usrXEmp# = 'N' and
     C                             (%parms >= 5 and inEmp# <> 0)
     C                   eval      ##RtnCd = *off
     C                   when      usrXFnd = 'Y' and not restrictW
     C                   if        inGrp <> group
     C                   eval      ##RtnCd = *on
     C                   endif
     C                   when      %parms >= 5 and inEmp# <> *zero and
     C                             usrXEmp# = 'Y' and not restrictW
     C                   select
     C                   when      UPFL5 = 'Y' and
     C                             (inEmp# <> UPEMP# or inDiv <> UPDIV)
     C                   eval      ##RtnCd = *on
     C                   when      inEmp# <> UPEMP#
     C                   eval      ##RtnCd = *on
     C                   endsl
     C                   when      %parms >= 5 and inEmp# <> *zero
     C                   call      'SA008R'      authEmp#
     C                   other
     C                   call      'SA008R'      authGrp
     C                   endsl

     C                   return    ##RtnCd

     PRestricted       e
     C***  End   Add  ***  03/10/09  **************************************


      *------------------------------------------------------------------------*
      *  WRUSWAOP - Writes/Updates the SWAOP record
      *------------------------------------------------------------------------*
     P WruSwaop        B

      ** Procedure Interface
     D WruSwaop        PI

     C                   eval      update = *off
     C     aoky          chain     pao01                              99
     C                   if        *in99 = *off
     C                   eval      update = *on
     C                   endif
     C*
     C                   callp     getDate
     C*
     C                   if        scfdat = *zeros
     C                   eval      aofdat = *zeros
     C                   else
     C                   Eval      dateOut = CvtDate(scfdat:BLANK:DT_CYMD)
     C                   MoveL     dateOut       aofdat
     C                   endif
     C*
     C                   if        sctdat = *zeros
     C                   eval      aotdat = *zeros
     C                   else
     C                   Eval      dateOut = CvtDate(sctdat:BLANK:DT_CYMD)
     C                   MoveL     dateOut       aotdat
     C                   endif
     C*
     C                   eval      aootyp = scotyp
     C                   eval      aooutq = scoutq
     C                   eval      aopsid = scpsid
     C*
     C                   movea     rmkArr        rmkDba
     C                   movea     rprArr        rprDba
     C                   movea     sumArr        sumDba
     C                   movea     sprArr        sprDba
     C*
B01  C                   if        update
     C*
     C                   eval      aouser = q1user
     C                   eval      aopgm = q1pgm
     C                   eval      aotime = svtime
     C                   eval      aodt = svdate
     C*
     C                   update    pao01
     C                   else
     C*
     C                   eval      aoco# = company
     C                   eval      aofnd = scgrp
     C                   eval      aofyr = scfyr
     C                   eval      aocase = sccase
     C*
     C                   eval      aouser = q1user
     C                   eval      aopgm = q1pgm
     C                   eval      aoiusr = q1user
     C                   eval      aoipgm = q1pgm
     C                   eval      aotime = svtime
     C                   eval      aoitim = svtime
     C                   eval      aodt = svdate
     C                   eval      aoidt = svdate
     C*
     C                   write     pao01
E01  C                   endif

     P WruSwaop        E

      *------------------------------------------------------------------------*
      * Finish - The data's been entered/viewed, so do final processing.
      *------------------------------------------------------------------------*
     P Finish          B

      ** Procedure Interface
     D Finish          PI             2  0
     D  Mode                          1a   Const

     C                   move      scgrp         pGroup
     C                   moveL     scoutq        outq
     C                   move      company       pCompany
     C                   move      scfyr         pyear
     c                   move      sccase        pcase
     C                   move      scpsid        psid              2
     C                   move      scotyp        potyp

     C* Building query statements
     C* Query 1

     C                   clear                   pqry1
     C     1             do        14            xx
     C                   if        sumarr(xx) <> *blanks and
     C                             sprarr(xx) = 'Y'
     C                   if        pqry1 = *blanks
     C     sumCodeList   cat(p)    sumarr(xx):0  pqry1
     C                   cat       '"':0         pqry1
     C                   else
     C                   cat       ' "':0        pqry1
     C                   cat       sumarr(xx):0  pqry1
     C                   cat       '"':0         pqry1
     C                   endif
     C                   endif
     C                   enddo

     C                   if        pqry1 <> *blanks
     C                   cat       ')':0         pqry1
     C                   endif

     C                   clear                   holdquery
     C     1             do        42            xx
     C                   if        rmkarr(xx) <> *blanks and
     C                             rprarr(xx) = 'Y'
     C                   if        holdquery = *blanks
     C     remCodeList   cat(p)    rmkarr(xx):0  holdquery
     C                   cat       '"':0         holdquery
     C                   else
     C                   cat       ' "':0        holdquery
     C                   cat       rmkarr(xx):0  holdquery
     C                   cat       '"':0         holdquery
     C                   endif
     C                   endif
     C                   enddo

     C                   if        holdquery <> *blanks
     C                   cat       ')':0         holdquery
     C                   endif

     C                   if        pqry1 = *blanks
     C                   if        holdquery <> *blanks
     C                   movel(p)  holdquery     pqry1
     C                   endif
     C                   else
     C                   if        holdquery <> *blanks
     C*** begin chg *** 03/14/05 ******************************************
     C**********         cat       '*AND':1      pqry1
     C                   cat       '*OR':1       pqry1
     C*** end   chg *** 03/14/05 ******************************************
     C                   cat       holdquery:1   pqry1
     C                   endif
     C                   endif

     C* Assign print flag value

     C                   eval      pprnt = 'N'

     C                   if        pqry1 = *blanks
     C                   eval      xx=1
     C     'Y'           lookup    sprarr(xx)                             99
     C                   if        *in99 = *on
     C                   eval      pprnt = 'Y'
     C                   else
     C                   eval      xx=1
     C     'Y'           lookup    rprarr(xx)                             99
     C                   if        *in99 = *on
     C                   eval      pprnt = 'Y'
     C                   endif
     C                   endif
     C                   else
     C     1             do        14            xx
     C                   if        sprarr(xx) = 'Y' and sumarr(xx) <> *blanks
     C                   eval      pprnt = 'Y'
     C                   leave
     C                   endif
     C                   enddo

     C                   if        pprnt = 'N'
     C     1             do        42            xx
     C                   if        rprarr(xx) = 'Y' and rmkarr(xx) <> *blanks
     C                   eval      pprnt = 'Y'
     C                   leave
     C                   endif
     C                   enddo
     C                   endif

     C                   endif

     C                   move      '*'           pqry1
     C                   clear                   psmcds
     C                   movea     sumarr        psmcds
     C                   move      '*'           psmcds
     C                   clear                   prmcds
     C                   movea     rmkarr        prmcds
     C                   move      '*'           prmcds

     C*** begin add *** 04/22/06 ******************************************
     C                   if        not *in19
     C                   eval      potyp = 'F'
     C                   eval      outq = 'QPRINT'
     C                   eval      psid = '  '
     C                   endif
     C*** end   add *** 04/22/06 ******************************************
     C                   callp     WruSwaop
     C                   CALLP     submitter(pCompany:pGroup:pEmployer:
     C                             pYear:pCase:outq:debug:psid:pfdat:
     C                             ptdat:potyp:pqry1:psmcds:prmcds:pprnt)
     C                   Return    EXIT

     P Finish          E



      *------------------------------------------------------------------------*
      * GetDate - Gets the current date and time.  Used for maint fields.
      *------------------------------------------------------------------------*
     P GetDate         B

      ** Procedure Interface
     D GetDate         PI

     C                   time                    dsDateTime
     C                   Eval      svTime = dsTime
     C                   Eval      dateOut = CvtDate(dsDate:BLANK:DT_CYMD)
     C                   MoveL     dateOut       svDate

     C                   Return

     P GetDate         E



      *------------------------------------------------------------------------*
      * OtherFkys - Handles processing of Function keys other than F3=Exit and
      *             F12=Cancel.  The function key pressed will be passed in,
      *             along with the screen# and mode.
      *------------------------------------------------------------------------*
     P OtherFkys       B

      ** Procedure Interface
     D OtherFkys       PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const
     D  KeyPressed                    1a   Const

     C                   Select

     C                   When      Fkey = F10
     C                   Return    F10pressed(Screen#)

     C                   Other
     C                   Return    0

     C                   EndSL

     P OtherFkys       E


      *------------------------------------------------------------------------*
      * F10pressed - Calls the search programs based on the field that had
      *              focus when F10 was pressed.
      *------------------------------------------------------------------------*
     P F10pressed      B

      ** Procedure Interface
     D F10pressed      PI             2  0
     D  Screen#                       2  0 Const

     C     fnkey         klist
     C                   kfld                    company
     C                   kfld                    scgrp

     C                   if        screen# = 1

     C                   Select

     C                   When       (scfld = 'SCCASE' or scfld = 'SCFYR' or
     C                              scfld = 'SCGRP') and *in16 = *off

     C     fnkey         chain     pfn01                              60
     C                   if        *in60 = *off
     C                   eval      rrn2 = dbrrn1
     C                   else
     C                   eval      rrn2 = *zeros
     C                   endif

     C                   eval      year = scfyr

     C                   if        *in90 <> *on
     C                   call      CLMSEARCH     clmpl
     C                   if        *inkc = *on
     C                   return    ERROR
     C                   endif
     C                   if        *inkl = *on  or rrn = 0
     C                   seton                                        31
     C                   return    ERROR
     C                   endif
     C                   if        rrn <> *zeros
     C     rrn           chain     pcm01                              60
     C                   if        *in60 = *off
     C                   eval      scgrp = cmfnd
     C                   eval      scfyr  = cmfyr
     C                   eval      sccase = cmcase
     C                   callp     GetSwaop(' ')
     C                   seton                                        32
     C                   endif
     C                   endif
     C                   endif
     C                   eval      *in90 = *off
     C                   Return    ERROR

     C*** chg 4/22/06 ** when      scfld = 'SCPSID'
     C                   when      scfld = 'SCPSID' and *in19

     C                   call      PIDSEARCH     pidpl
     C                   if        *inkc = *on
     C                   return    ERROR
     C                   endif
     C                   if        *inkl = *on or pPsid = *blanks
     C                   seton                                        35
     C                   return    ERROR
     C                   endif
     C                   if        pPsid <> *blanks
     C                   move      pPsid         scpsid
     C                   endif
     C                   Return    ERROR
     C                   Other
     C                   Return    ERROR

     C                   EndSL

     C                   else

     C     RowCol        Div       256           row
     C                   mvr                     col

     C                   if        row > 7 and row < 22
     C                   Select
     C                   When      (col > 4 and col < 18)

     C                   call      SUMSEARCH     sumpl
     C                   if        rtnError = 'A'
     C                   return    ERROR
     C                   endif
     C                   if        rtnError = 'B'
     C                   seton                                        81
     C                   return    ERROR
     C                   endif
     C                   if        sumcode  <> *blanks
     C                   eval      s = 1
     C     sumcode       lookup    SumArr(s)                              99
     C                   if        *in99 = *off
     C                   eval      s = 1
     C     *blanks       lookup    SumArr(s)                              99
     C                   eval      SumArr(s) = sumcode
     C                   endif
     C                   endif
     C                   eval      *in90 = *off
     C                   Return    ERROR

     C                   When      (col > 20 and col < 75)

     C                   call      RMKSEARCH     rmkpl
     C                   if        *inkc = *on
     C                   return    ERROR
     C                   endif
     C                   if        *inkl = *on
     C                   seton                                        82
     C                   return    ERROR
     C                   endif
     C                   if        rmkcode  <> *blanks
     C                   eval      r = 1
     C     rmkcode       lookup    RmkArr(r)                              99
     C                   if        *in99 = *off
     C                   eval      r = 1
     C     *blanks       lookup    RmkArr(r)                              99
     C                   eval      RmkArr(r) = rmkcode
     C                   endif
     C                   endif
     C                   eval      *in90 = *off
     C                   Return    ERROR
     C                   Other
     C                   Return    ERROR

     C                   EndSL

     C                   else
     C                   Return    ERROR
     C                   Endif

     C                   EndIf

     P F10pressed      E

      *------------------------------------------------------------------------*
      * PgmEnd - Any processing at the very end of the program.
      *------------------------------------------------------------------------*
     P PgmEnd          B

      ** Procedure Interface
     D PgmEnd          PI


     P PgmEnd          E



      *------------------------------------------------------------------------*
      * PgmStart - Processing before entering the screen display cycles.
      *------------------------------------------------------------------------*
     P PgmStart        B

      ** Procedure Interface
     D PgmStart        PI


     C                   call      'SRCENTER'
     C                   parm      OptionName    ##data
     C                   parm      40            ##leng
     C                   movel(p)  ##data        scprds

     C                   eval      @@mode='P'
     C                   movea     *all'N'       SprArr
     C                   movea     *all'N'       RprArr
     C                   move      'P'           scotyp

     C     q1user        chain     pup01                              60
     C                   eval      scoutq = upoutq
     C                   eval      scpsid = 'S2'

      *** Load array with indicator numbers for validation matrix

     C                   z-add     1             x                 2 0
     C     X             dougt     42
      *
     C                   do        3             y                 1 0
      *
     C                   select
     C                   when      y = 1
     C                   move      43            Clm(x)
     C                   add       1             x
     C                   when      y = 2
     C                   move      45            Clm(x)
     C                   add       1             x
     C                   when      y = 3
     C                   move      47            Clm(x)
     C                   add       1             x
     C                   endsl
      *
     C                   enddo
      *
     C                   enddo

      * Check to see if there is any values to preload

     C                   callp     GetSwaop(' ')
     C*** begin add *** 04/22/06 ******************************************
     C                   seton                                        19
     C                   if        q1prms > 1 and eMode = 'D'
     C                   setoff                                       19
     C                   endif
     C*** end   add *** 04/22/06 ******************************************

     P PgmStart        E



      *------------------------------------------------------------------------*
      * ProcScreen - This handles all the processing of any number of screens
      *              through recursion.
      *------------------------------------------------------------------------*
     P ProcScreen      B

      ** Procedure Interface
     D ProcScreen      PI             2  0
     D  Screen#                       2  0 Const
     D  Mode                          1a   Const

     C                   DoU       PgmStatus <> ERROR

      ** Display the screens.
     C                   CALLP     DspScreen(Screen#)
     C                   movea     '0000000000'  *in(21)
     C                   movea     '0000'        *in(31)
     C                   movea     '00000000'    *in(41)
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)

      ** Check for the press of function keys.
     C                   Eval      PgmStatus = CheckFkys( Screen# : Mode )

     C                   If        PgmStatus <> EXIT
     C                             AND PgmStatus <> CANCEL
     C                             AND PgmStatus <> ERROR
     C                   Eval      PgmStatus = ErrorChk( Screen# : Mode )

     C                   EndIf

     C                   EndDo

     C                   Return    PgmStatus

     P ProcScreen      E



      *------------------------------------------------------------------------*
      * AllDone - This handles the processing of the screen that allows the
      *           user to confirm/cancel their choices.
      *------------------------------------------------------------------------*
     P AllDone         B

      ** Procedure Interface
     D AllDone         PI             1a
     D  Grp                           3  0 Const
     D  Yr                            3  0 Const
     D  Case                          7  0 Const

     D  ScreenStatus   S              2  0

     C                   eval       SXGRP1 = Grp
     C                   eval       SXFYR = Yr
     C                   eval       SXCASE = Case

     C                   DoU       ScreenStatus <> ERROR
     C                   eval      ScreenStatus = 0

     C                   If        Yr <> 0
     C                   seton                                            73
     C                   setoff                                       7172
     C                   else
     C                   seton                                        71
     C                   setoff                                         7273
     C                   EndIf

     C                   EXFMT     SCCONF

     C                   If        Fkey = F3 or Fkey = F10
     C                   eval      ScreenStatus = ERROR
     C                   EndIf

     C                   EndDo

     C                   If        Fkey = F12
     C                   eval      pgmStatus = CANCEL
     C                   eval      confirmed = no
     C                   Return    NO
     C                   else
     C                   eval      confirmed = yes
     C                   Return    YES
     C                   EndIf

     C                   Return    NO

     P AllDone         E

      * utility procedure definitions
      /COPY SC960RPI
