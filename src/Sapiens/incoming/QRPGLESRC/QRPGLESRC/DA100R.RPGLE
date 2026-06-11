     *========================================================================
      * (c) 2013 StoneRiver, Inc. All rights reserved.
      * StoneRiver is a trademark of StoneRiver, Inc.
     *========================================================================
     *
     * DPIWRTRMK - API's to write DARTS remarks
     *
     * Created   : 04/19/00
     * Programmer: C:113
     * Project   : DARTS
     *
     *========================================================================
     * REVISIONS:
      *
      *   07/02/19 C:62  Project: 152WAB
      *                  Use Prefix Folder.
      *
      *   08/30/13 C:33  Project: SY195
      *                  Use correct DCID for UNCN path.
      *
      *   05/23/12 C:062 Project: wc2115
      *                  - Fix %Parm conditions in WriteRmk Routine.
      *
      *   03/16/12 C:282 Project: 133WML
      *                  - Adding new procedure to write dart remark.
      *
      *   03/13/12 C:282 Project: 114WEG
      *                  - Adding new procedure ResSplDDoc to resolve spool
      *                    into second attached DARTS doc
      *
      *   10/17/11 C:279 Project: SY195
      *                  - Modify program to pass/return UNC path
      *                    field and IFS flag to called/calling
      *                    programs.
     *
     * 01/18/2011 C:062 PROJECT: 71656
      *                  Use TIME instead of *DATE.
     *
     * 06/14/2004 C:062 PROJECT: 129WCS
      *                  Add process to allow both a data entry pgm
      *                  and reference an image
     *
     * 07/12/2000 C:113 PROJECT: DR113
     *            Added Image parms to WriteRmk API.
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================
     H nomain

     *========================================================================
     * File Definitions
     *========================================================================
     Fsdcmli    if   e           k disk    rename(pcm01:pcm0i)
     F*** BEG   ADD *** 03/13/12 **************************************
     F*****sdcmp     o  a e             disk
     Fsdcmp     uf a e             disk
     F*** END   ADD *** 03/13/12 **************************************
     Fsmarl1    if   e           k disk    rename(par01:par0i)
     Fsmarp     o  a e             disk
     Fsmupl     if   e           k disk
     Fstctl1    if   e           k disk
     Fsdaip     o  a e             disk
    

     d*** BEG   ADD *** 03/13/12 **************************************
     d psds          esds                  extname(szq1p)
     d*** END   ADD *** 03/13/12 **************************************
     d suffix          s              3
     d prefixFldr      s             80
     *========================================================================
     * Prototypes
     *========================================================================
      /copy DA100RINC
      /copy DPRDATE

     d GetCltPrefix    pr                  extPgm('SA030R')
     d suffix                         3
     d prefixFldr                    80

     Ipcm0i
     I              cmkey                       c2key
     I              cmseq#                      c2seq#

     Ipar0i
     I              arasq#                      a2asq#

     *------------------------------------------------------------------------
     * WriteRmk - This API writes a DARTS remark.  It performs no error
     *            checking.
     *------------------------------------------------------------------------
     P WriteRmk        B                   export

      *entry
     *** Procedure Interface ***
     D WriteRmk        PI             1a
     D  eKey                         40a   const                                Key
     D  eSequence                     5a                                        Sequence
     D  eComment                    256a   const                                Remark
     D  eRmkDate                      8a   const                                Remark date
     D  eRmkCode                      5a   const                                Remark code
     D  eProgram                     10a   const                                Maint program
     D  eUser                        10a   const                                Maint user
     D  ePname                       10a   const                                Key program
     D  eDiaryFlag                    1a   const                                Diary flag
     D  eDEflag                       1a   const                                Data entry flag
     D  ePurgeFlg                     1a   const                                Purge flag
     D  eSecByLvl                     1a   const                                Secure by lvl flag
     D  eSecLevel                     3a   const options(*nopass)               Security level
     D  eDocID                       12a   const options(*nopass)               Document ID
     D  eFolder                      80a   const options(*nopass)               Document folder
     D  eComm2                       19a   const options(*nopass)               Remark (end)
     D  eImgFlag                      1a   const options(*nopass)               Image app prod
     D  eImgDocId                    12a   const options(*nopass)               Img Document ID

     *** Locals ***
     D dateIn          s              8  0
     D rmkCode         s                   like(cmrmcd)
     D sequence        s                   like(cmseq#)
     D xDate           s                   like(cmdt)
     ** Date & Time structures
     D                 ds
     D dsdttm                        14  0
     D  xTime                         6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D  pKey           s                   like(cmkey)                          Key
     D  pSequence      s                   like(cmseq#)                         Sequence
     D  pAllowCF       s                   like(aivcfn)                         Remark
     D  pDataEntryP    s                   like(aidpgm)                         Remark date
     D  pDataEntryK    s                   like(aidkey)                         Remark code
     D  pProgram       s                   like(cmpgm)                          Maint program
     D  pUser          s                   like(cmuser)                         Maint program
     D  xxdepg         s                   like(ctdepg)                         Maint program
     D ##ar            s              1
     D ##arls          s              1
     D ##autb          s              1
     D ##ccod          s              1
     D ##clt#          s              3
     D ##crac          s              1
     D ##crvw          s              1
     D ##curs          s              1
     D ##dcf           s              1
     D ##deif          s              1
     D ##edi           s              1
     D ##freq          s              1
     D ##fsch          s              1
     D ##kcif          s              1
     D ##mcif          s              1
     D ##mcrv          s              1
     D ##med           s              1
     D ##mgri          s              1
     D ##mss           s              1
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##os            s              1
     D ##parn          s              1
     D ##pcib          s              1
     D ##preo          s              1
     D ##prom          s              1
     D ##prvb          s              1
     D ##rhab          s              1
     D ##sufx          s              3
     D ##ucpf          s              1
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     d root            s             10
     d env             s              1

    
     * Remark Type Table file
     C     ctkey         klist
     C                   kfld                    rmkCode
    
     * DARTS Remark File
     C     c2ky          klist
     C                   kfld                    eKey
     C                   kfld                    sequence
    
      * Get Root
     c                   call      'RTVDOCROOT'
     c                   parm                    root
      * Find IFS
     c                   call      'RTVDOCENV'
     c                   parm                    env
     c
     C                   call      'SA001R'
01   C                   parm      *blanks       ##wcif
02   C                   parm      *blanks       ##mcif
03   C                   parm      *blanks       ##wcin
04   C                   parm      *blanks       ##ar
05   C                   parm      *blanks       ##ntwk
06   C                   parm      *blanks       ##ur
07   C                   parm      *blanks       ##med
08   C                   parm      *blanks       ##edi
09   C                   parm      *blanks       ##wcdk
10   C                   parm      *blanks       ##rhab
11   C                   parm      *blanks       ##mss
12   C                   parm      *blanks       ##fsch
13   C                   parm      *blanks       ##prvb
14   C                   parm      *blanks       ##npwc
15   C                   parm      *blanks       ##os
16   C                   parm      *blanks       ##arls
17   C                   parm      *blanks       ##ccod
18   C                   parm      *blanks       ##crvw
19   C                   parm      *blanks       ##prom
20   C                   parm      *blanks       ##mcrv
21   C                   parm      *blanks       ##freq
22   C                   parm      *blanks       ##crac
23   C                   parm      *blanks       ##preo
24   C                   parm      *blanks       ##parn
25   C                   parm      *blanks       ##mgri
26   C                   parm      *blanks       ##kcif
27   C                   parm      *blanks       ##curs
28   C                   parm      *blanks       ##pcib
29   C                   parm      *blanks       ##clt#
30   C                   parm      *blanks       ##ucpf
31   C                   parm      *blanks       ##dcf
32   C                   parm      *blanks       ##autb
33   C                   parm      *blanks       ##deif
    
     *** Procedure Definition ***
     C                   clear                   pcm01
    
     * Get today's date and time
     C                   time                    dsdttm
     C                   eval      xDate = Cvt2cymd(dsdate)

     * Get remark code detail
     C                   movel(p)  eRmkCode      rmkCode                        Remark code
     C     ctkey         chain     pct01                              60
     C                   if        ##deif = 'Y'
     C                   if        not *in60 and ctdepg <> *blanks
     C                   eval      cmfl01 = 'Y'
     C                   eval      xxdepg = ctdepg
     C                   else
     C                   eval      cmfl01 = ' '
     C                   eval      xxdepg = ' '
     C                   endif
     C                   else
     C                   eval      cmfl01 = ' '
     C                   eval      xxdepg = ' '
     C                   endif

     ** Load record fields **
     C                   movel     eKey          cmkey                          Key
    
     * If the comment is blank, use the remark type description
     C                   if        eComment = *blank
     C                   movel(p)  ctdesc        cmcomm                         Remark
     C                   else
     C                   movel(p)  eComment      cmcomm
     C                   endif
    
     * Remark date
     C                   move(p)   eRmkDate      dateIn
     C                   eval      cmcdat = Cvt2cymd(dateIn)

     * Remark code
     C                   movel(p)  eRmkCode      cmrmcd

     * Transaction date & time
     C                   eval      cmtrdt = xDate
     C                   eval      cmtrtm = xTime
    
     * DARTS keyed program
     C                   movel     ePname        cmpnam

     * Status
     C                   movel(p)  'A'           cmstat

     * Diary flag
     C                   movel(p)  eDiaryFlag    cmdifl
    
     * Summary code
     C                   movel(p)  ctsmcd        cmsmcd
    
     * Data Entry flag
     C                   if        ##deif <> 'Y'
     C                   movel(p)  eDEflag       cmfl01
     C                   endif

     * Purge flag
     C                   movel(p)  ePurgeFlg     cmfl02

     * Diary category
     C                   movel(p)  ctfl02        cmfl03
    
     * Load maintenance fields
     C                   movel(p)  eUser         cmiusr
     C                   movel(p)  eProgram      cmipgm
     C                   eval      cmidt = xDate
    
     * Secure by level?
     C                   if        eSecByLvl <> 'Y'
     C                   move      'N'           cmsbyl
    
     C                   else
     C                   move      eSecByLvl     cmsbyl
     C                   if        %parms > 12
     C                   movel(p)  eRmkCode      rmkCode
     C     ctkey         chain     pct01                              60
     C                   if        NOT *in60
    
     C                   if        ctdslv = *zero
     C     eUser         chain     pup01                              60
     C                   if        NOT *in60
     C                   move      uplvl         cmsecl
     C                   endif
     C                   else
     C                   move      ctdslv        cmsecl
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    
     * Document
     C                   if        %parms > 13
     C                   eval      cmdcid = eDocID
     C                   eval      cmfold = eFolder
     C***  Begin Chg  ***  08/30/13  *****************************************
     c**********         if        cmfold <> ' ' or cmdcid <> ' '
     c**********         eval      cmuncn = '/' + %trim(root) + '/' +
     c**********                            %trim(cmfold) + '/' +
     c**********                            %trim(cmdcid)
     c**********         eval      cmifsp = env
     c**********         else
     c**********         eval      cmuncn = ' '
     c**********         eval      cmifsp = ' '
     c**********         endif
      /Free
       Clear CMuncN;
       Clear CMifsP;
       GetCltPrefix(suffix:prefixFldr);
       If prefixFldr = ' ';
          Select;
          When %parms >= 18 and (eFolder <> ' ' and eImgDocId <> ' ');
             CMuncN = '/' + %trim(root) + '/' + %trim(eFolder) + '/'
              + %trim(eImgDocId);
          When (eFolder <> ' ' and eDocId <> ' ');
             CMuncN = '/' + %trim(root) + '/' + %trim(eFolder) + '/'
              + %trim(eDocId);
          Other;
          Endsl;
       Else;
          Select;
          When %parms >= 18 and (eFolder <> ' ' and eImgDocId <> ' ');
             CMuncN = '/' + %trim(root) + '/' + %trim(prefixFldr) + '/' +
              %trim(eFolder) + '/' + %trim(eImgDocId);
          When (eFolder <> ' ' and eDocId <> ' ');
             CMuncN = '/' + %trim(root) + '/' + %trim(prefixFldr) + '/' +
              %trim(eFolder) + '/' + %trim(eDocId);
          Other;
          Endsl;
       EndIf;
       If CMuncN <> ' ';
          CMifsP = env;
       Endif;
      /End-free
     C***  End   Chg  ***  08/30/13  *****************************************
     C                   endif

     * Comment (end)
     C*** Begin Chg *** 05/23/12 *******************************************
     C**********         if        %parms >= 15
     C                   if        %parms > 15
     C*** End   Chg *** 05/23/12 *******************************************
     C                   move      eComm2        cmcomm
     C                   endif

     C*** Begin Chg *** 05/23/12 *******************************************
     C**********         if        %parms >= 16
     C                   if        %parms > 16
     C*** End   Chg *** 05/23/12 *******************************************
     * Image fields
     C                   move      eImgFlag      cmimgf
     C                   move      eImgDocID     cmidoc
     C                   endif

     C                   eval      cmuser = cmiusr
     C                   eval      cmpgm = cmipgm
     C                   eval      cmtime = cmtrtm
     C                   eval      cmdt = cmtrdt

     * Get last sequence# used
     C                   eval      sequence = *hival
     C     c2ky          setgt     pcm0i
     C     eKey          readpe    pcm0i                                  60
     C                   if        NOT *in60
     C                   eval      cmseq# = c2seq# + 1
     C                   else
     C                   eval      cmseq# = 1
     C                   endif
    
     * Write record
     C                   write     pcm01
     C                   move      cmseq#        eSequence
     C                   feod      sdcmp
     C*
     C                   if        ##deif = 'Y'
     C                   eval      pKey = cmkey
     C                   eval      pSequence = cmseq#
     C                   eval      pAllowCF = *blanks
     C                   eval      pDataEntryP = xxdepg

     C                   if        pDataEntryP <> *blanks
     C                   eval      pDataEntryK = cmkey
     C                   else
     C                   clear                   pDataEntryK
     C                   endif

     C                   eval      pProgram = cmpgm
     C                   eval      pUser = cmuser
     C*
     C                   callp     writeAddInfo(pKey:pSequence:pAllowCF:
     C                             pDataEntryP:pDataEntryK:pProgram:
     C                             pUser)
     C                   endif

     C                   return    '1'

     P WriteRmk        E
     *------------------------------------------------------------------------
     * WriteAddInfo - This API writes the DARTS additional info record.
     *                This does no error checking.
     *------------------------------------------------------------------------
     P WriteAddInfo    B

     *** Procedure Interface ***
     D WriteAddInfo    PI
     D  eKey                         40a                                        Key
     D  eSequence                     5p 0                                      Sequence
     D  eAllowCF                      1a                                        Remark
     D  eDataEntryP                  10a                                        Remark date
     D  eDataEntryK                  40a                                        Remark code
     D  eProgram                     10a                                        Maint program
     D  eUser                        10a                                        Maint user
     *** Locals ***
     D dateIn          s              8  0
     D xDate           s                   like(aidt)
     ** Date & Time structures
     D                 ds
     D dsdttm                        14  0
     D  xTime                         6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     * Get today's date and time
     C                   time                    dsdttm
     C                   eval      xDate = Cvt2cymd(dsdate)

     C                   clear                   pai01

     C                   eval      aikey = eKey
     C                   move      eSequence     aiseq#
     C                   eval      aivcfn = eAllowCF
     C                   eval      aidpgm = eDataEntryP
     C                   eval      aidkey = eDataEntryK

     C                   eval      aiuser = eUser
     C                   eval      aipgm  = eProgram
     C                   eval      aidt  = xDate
     C                   eval      aitime = xTime
     C                   eval      aiiusr = eUser
     C                   eval      aiipgm = eProgram
     C                   eval      aiidt = xDate
     C                   eval      aiitim = xTime

     C                   write     pai01

     P WriteAddInfo    E

     *------------------------------------------------------------------------
     * WriteAddl - This API writes a DARTS additional remark.  It performs
     *             no error checking.
     *------------------------------------------------------------------------
     P WriteAddl       B                   export

     D WriteAddl       PI             1a
     D  eUpdateFlg                    1a
     D  eKey                         40a
     D  eSequence                     5a
     D  eAddlSeq#                     5a
     D  eComment                     75a
     D  eRmkCode                      5a
     D  eProgram                     10a
     D  eUser                        10a
     D  ePname                       10a
     D  eSecByLvl                     1a
     D  eSecLevel                     3a


     *** Locals ***
     D dateIn          s              8  0
     D rmkCode         s                   like(arrmcd)
     D sequence        s                   like(arseq#)
     D addlSeq#        s                   like(arasq#)
     D xDate           s                   like(ardt)
     ** Date & Time structures
     D                 ds
     D dsdttm                        14  0
     D  xTime                         6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     C     a2ky          klist
     C                   kfld                    eKey
     C                   kfld                    sequence
     C                   kfld                    addlSeq#

     C     a2ky2         klist
     C                   kfld                    eKey
     C                   kfld                    sequence

     * Remark Type Table file
     C     ctkey         klist
     C                   kfld                    rmkCode

    
     *** Procedure Definition ***
     * Open the needed files
     C                   open      smarl1
     C                   open      smarp
     C                   open      smupl
     C                   open      stctl1

     C                   clear                   par01
    
     * Get today's date
     C                   time                    dsdttm
     C                   eval      xDate = Cvt2cymd(dsdate)
    
     * Load record fields
     C                   movel     eKey          arkey
     C                   move      eSequence     arseq#
     C                   movel(p)  eComment      arcm75
    
     C                   move      arasq#        addlSeq#
     C                   clear                   arcsid
     C                   eval      arcdat = xDate
     C                   clear                   arcode
     C                   movel     ePname        arpnam
     C                   eval      artrdt = xDate
     C                   eval      artrtm = xTime
     C                   movel(p)  'A'           arstat
     C                   movel(p)  eRmkCode      arrmcd
    
     * Secure by level?
     C                   if        eSecByLvl <> 'Y'
     C                   if        eSecLevel = *zero
     C                   move      'N'           arsbyl
     C                   clear                   arsecl
     C                   endif
    
     C                   else
     C                   move      'Y'           arsbyl
     C                   if        eSecLevel = *zero
     C                   movel(p)  eRmkCode      rmkCode
     C     ctkey         chain     pct01                              60
     C                   if        NOT *in60
    
     C                   if        ctdslv = *zero
     C     eUser         chain     pup01                              60
     C                   if        NOT *in60
     C                   move      uplvl         arsecl
     C                   endif
     C                   else
     C                   move      ctdslv        arsecl
     C                   endif
     C                   endif
     C                   endif
     C                   endif

     * Load maintenance fields
     C                   if        eUpdateFlg = 'U'
     C                   movel(p)  eUser         aruser
     C                   movel(p)  eProgram      arpgm
     C                   eval      artime = xTime
     C                   eval      ardt = xDate
     C                   else
     C                   movel(p)  eUser         ariusr
     C                   movel(p)  eProgram      arpgm
     C                   eval      aridt = xDate
     C                   endif

     * Get last sequence# used
    
     C                   move      eSequence     sequence
     C                   eval      addlSeq# = *hival
     C     a2ky          setgt     par0i
     C     a2ky2         readpe    par0i                                  60
     C                   if        *in60 = *off
     C                   eval      arasq# = a2asq# + 1
     C                   else
     C                   eval      arasq# = 1
     C                   endif
    
     * Write or update record
     C                   write     par01
     C                   move      arasq#        eAddlSeq#
     C                   feod      smarp
    
     * Close all files
     C                   close     smarl1
     C                   close     smarp
     C                   close     smupl
     C                   close     stctl1

     C                   return    '1'

     P WriteAddl       E
      *** BEG ADD *** 03/13/12 *******************************************
     *------------------------------------------------------------------------
     * ResSplDDoc - This API will resolve spool file as second attached DARTS doc
     *------------------------------------------------------------------------
     P ResSplDDoc      B                   export

     *** Procedure Interface ***
     d ResSplDDoc      PI
     d inSplf                        10
     d inQue                         10
     d inRrn                          9  0

     *** Locals ***
     d strSplf         s             10
     d strFold         s             80
     d strDcid         s             12
     d strPre          s              2
     d strRtcd         s              1
     d strSub#         s              5
     d strPflr         s             80
     d strOutq         s             10
     d strComm         s             75
     d strPnam         s             10
     d strSecl         s              3
     d strUser         s             10
     d strPgm          s             10
     d strRmcd         s              5
     d strSeq#         s              5
     d strAsq#         s              5
     d strDocid        s             40
     d strFlr          s             60
     d strTyp          s              1
     d strDsc          s             40
     d strSysn         s             20
     d strRtncd        s              1
     d strEdsc         s             80
     d strApth         s             80
     d strRpyn         s              1
      *---------------------------------------------------------------------
      * Data Structures
      *---------------------------------------------------------------------
     d intKey          DS            40
     d  intCo#                 1      3
     d  intFnd                 4      6
      *
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d GetSnxtDc       pr                  extPgm('SNXTDCID')
     d strFold                       80
     d strDcid                       12
     d strPre                         2
     d intCo#                         3
     d intFnd                         3
     d strRtcd                        1

     d GetSreSolv      pr                  extPgm('SRESOLVE')
     d strSplf                       10
     d strFold                       80
     d strDcid                       12
     d strOutq                       10

     d GetAddDart      pr                  extPgm('DA701R')
     d intKey                        40
     d strSeq#                        5
     d strComm                       75
     d strPnam                       10
     d strSecl                        3
     d strUser                       10
     d strPgm                        10
     d strRmcd                        5
     d strAsq#                        5

     d GetAdAtDoc      pr                  extPgm('ISEWMD')
     d intKey                        40
     d strSeq#                        5
     d strDocid                      40
     d strFlr                        60
     d strTyp                         1
     d strDsc                        40
     d strRmcd                        5
     d strSysn                       20
     d strPgm                        10
     d strUser                       10
     d strRtncd                       1
     d strEdsc                       80
     d strApth                       80
     d strRpyn                        1

     *** Procedure Definition ***
      /free

        strSplf = inSplf;
        strOutq = inQue;

       // Chain SDCMP with relative record no. to retrieve the latest record written
       // for audit request.

        Chain inRrn pcm01;

        If %Found(sdcmp);

           intKey  = CMkey;
           strComm = CMcomm;
           strSeq# = %editc(CMseq#:'X');
           strRmcd = CMrmcd;

        Endif;

        Unlock(e) sdcmp;

       // Get new document name.

        strFold = *blanks;
        strDcid = *blanks;
        strPre  = 'AU';
        strRtcd = 'P';

        GetSnxtDc(strFold:strDcid:strPre:intCo#:intFnd:strRtcd);

       // Call program to Resolve Spool File to document.

        GetSreSolv(strSplf:strFold:strDcid:strOutq);

       // Call program to write additional darts.

        strPnam = 'W0070R';
        strSecl = *zeros;
        strUser = q1user;
        strPgm  = q1pgm;
        strAsq# = *blanks;

        GetAddDart(intKey:strSeq#:strComm:strPnam:strSecl:
                    strUser:strPgm:strRmcd:strAsq#);

       // Call program to additional attach darts document.

        strDocid = strDcid;
        strFlr   = strFold;
        strTyp   = 'O';
        strDsc   = 'Additional attach DARTS document';
        strSysn  = 'AS400   ';
        strPgm   = q1pgm;
        strUser  = q1user;
        strRtncd = *blanks;
        strEdsc  = *blanks;
        strApth  = *blanks;
        strRpyn  = *blanks;

        GetAdAtDoc(intKey:strSeq#:strDocid:strFlr:strTyp:
                    strDsc:strRmcd:strSysn:strPgm:strUser:
                    strRtncd:strEdsc:strApth:strRpyn);

      /end-free

     P ResSplDDoc      E
      *** END ADD *** 03/13/12 *******************************************
      *** BEG ADD *** 03/16/12 *******************************************
     *------------------------------------------------------------------------
     * WriteDarts - This procedure is used to write darts remarks.
     *------------------------------------------------------------------------
     P WriteDarts      B                   EXPORT

     *** Procedure Interface ***
     d WriteDarts      PI
     d inKey                         40                                         01
     d inComm                       256                                         02
     d inCdat                         8                                         03
     d inDcid                        12                                         04
     d inPnam                        10                                         05
     d inSecl                         3                                         06
     d inUser                        10                                         07
     d inPgm                         10                                         08
     d inFold                        80                                         09
     d inDifl                         1                                         10
     d inIpgm                        10                                         11
     d inFl01                         1                                         12
     d inFl02                         1                                         13
     d inRmcd                         5                                         14
     d inSeq                          5                                         15
     d inSbyl                         1    options(*nopass)                     16
     d inCom2                        19    options(*nopass)                     17
     d inApid                         2    options(*nopass)                     18
     d inIdoc                        12    options(*nopass)                     19
     d inIque                        10    options(*nopass)                     20
     d inImgf                         1    options(*nopass)                     21
     d inIpth                        10    options(*nopass)                     22
     d inPidx                         5    options(*nopass)                     23
     d inQpty                         1    options(*nopass)                     24
     d inDctp                         5    options(*nopass)                     25
     d inCsid                         2    options(*nopass)                     26
     d inCode                         2    options(*nopass)                     27
     d inTrdt                         7    options(*nopass)                     28
     d inTrtm                         6    options(*nopass)                     29
     d inStat                         1    options(*nopass)                     30
     d inIusr                        10    options(*nopass)                     31
     d inDcdt                         7    options(*nopass)                     32
     d inFl03                         1    options(*nopass)                     33
     d inFl04                         1    options(*nopass)                     34
     d inFl05                         1    options(*nopass)                     35
     d inIdt                          7    options(*nopass)                     36
     d inVcfn                         1    options(*nopass)                     37
     d inDpgm                        10    options(*nopass)                     38
     d inDkey                        40    options(*nopass)                     39
     d inPath                        80    options(*nopass)                     40
     d inRpyn                         1    options(*nopass)                     41
      *
      * Local Variable
     d strKey#         s             40                                         01
     d strComm#        s            256                                         02
     d strCdat#        s              8                                         03
     d strDcid#        s             12                                         04
     d strPnam#        s             10                                         05
     d strSecl#        s              3                                         06
     d strUser#        s             10                                         07
     d strPgm#         s             10                                         08
     d strFold#        s             80                                         09
     d strDifl#        s              1                                         10
     d strIpgm#        s             10                                         11
     d strFl01#        s              1                                         12
     d strFl02#        s              1                                         13
     d strRmc#         s              5                                         14
     d strSeq#         s              5                                         15
     d strSbyl#        s              1                                         16
     d strCom2#        s             19                                         17
     d strApid#        s              2                                         18
     d strIdoc#        s             12                                         19
     d strIque#        s             10                                         20
     d strImgf#        s              1                                         21
     d strIpth#        s             10                                         22
     d strPidx#        s              5                                         23
     d strQpty#        s              1                                         24
     d strDctp#        s              5                                         25
     d strCsid#        s              2                                         26
     d strCode#        s              2                                         27
     d strTrdt#        s              7                                         28
     d strTrtm#        s              6                                         29
     d strStat#        s              1                                         30
     d strIusr#        s             10                                         31
     d strDcdt#        s              7                                         32
     d strFl03#        s              1                                         33
     d strFl04#        s              1                                         34
     d strFl05#        s              1                                         35
     d strIdt#         s              7                                         36
     d strVcfn#        s              1                                         37
     d strDpgm#        s             10                                         38
     d strDkey#        s             40                                         39
     d strPath#        s             80                                         40
     d strRpyn#        s              1                                         41
      *
     d WrtDart         pr                  extPgm('DA700R')
     d strKey#                       40                                         01
     d strComm#                     256                                         02
     d strCdat#                       8                                         03
     d strDcid#                      12                                         04
     d strPnam#                      10                                         05
     d strSecl#                       3                                         06
     d strUser#                      10                                         07
     d strPgm#                       10                                         08
     d strFold#                      80                                         09
     d strDifl#                       1                                         10
     d strIpgm#                      10                                         11
     d strFl01#                       1                                         12
     d strFl02#                       1                                         13
     d strRmc#                        5                                         14
     d strSeq#                        5                                         15
     d strSbyl#                       1    options(*nopass)                     16
     d strCom2#                      19    options(*nopass)                     17
     d strApid#                       2    options(*nopass)                     18
     d strIdoc#                      12    options(*nopass)                     19
     d strIque#                      10    options(*nopass)                     20
     d strImgf#                       1    options(*nopass)                     21
     d strIpth#                      10    options(*nopass)                     22
     d strPidx#                       5    options(*nopass)                     23
     d strQpty#                       1    options(*nopass)                     24
     d strDctp#                       5    options(*nopass)                     25
     d strCsid#                       2    options(*nopass)                     26
     d strCode#                       2    options(*nopass)                     27
     d strTrdt#                       7    options(*nopass)                     28
     d strTrtm#                       6    options(*nopass)                     29
     d strStat#                       1    options(*nopass)                     30
     d strIusr#                      10    options(*nopass)                     31
     d strDcdt#                       7    options(*nopass)                     32
     d strFl03#                       1    options(*nopass)                     33
     d strFl04#                       1    options(*nopass)                     34
     d strFl05#                       1    options(*nopass)                     35
     d strIdt#                        7    options(*nopass)                     36
     d strVcfn#                       1    options(*nopass)                     37
     d strDpgm#                      10    options(*nopass)                     38
     d strDkey#                      40    options(*nopass)                     39
     d strPath#                      80    options(*nopass)                     40
     d strRpyn#                       1    options(*nopass)                     41
      *
      /free
       strKey#   = inKey;                                                    // 01
       strComm#  = inComm;                                                   // 02
       strCdat#  = inCdat;                                                   // 03
       strDcid#  = inDcid;                                                   // 04
       strPnam#  = inPnam;                                                   // 05
       strSecl#  = inSecl;                                                   // 06
       strUser#  = inUser;                                                   // 07
       strPgm#   = inPgm;                                                    // 08
       strFold#  = inFold;                                                   // 09
       strDifl#  = inDifl;                                                   // 10
       strIpgm#  = inIpgm;                                                   // 11
       strFl01#  = inFl01;                                                   // 12
       strFl02#  = inFl02;                                                   // 13
       strRmc#   = inRmcd;                                                   // 14
       strSeq#   = inSeq;                                                    // 15

       If %parms > 15;
        strSbyl#  = inSbyl;                                                  // 16
       Endif;

       If %parms > 16;
        strCom2#  = inCom2;                                                  // 17
       Endif;

       If %parms > 17;
        strApid#  = inApid;                                                  // 18
       Endif;

       If %parms > 18;
        strIdoc#  = inIdoc;                                                  // 19
       Endif;

       If %parms > 19;
        strIque#  = inIque;                                                  // 20
       Endif;

       If %parms > 20;
        strImgf#  = inImgf;                                                  // 21
       Endif;

       If %parms > 21;
        strIpth#  = inIpth;                                                  // 22
       Endif;

       If %parms > 22;
        strPidx#  = inPidx;                                                  // 23
       Endif;

       If %parms > 23;
        strQpty#  = inQpty;                                                  // 24
       Endif;

       If %parms > 24;
        strDctp#  = inDctp;                                                  // 25
       Endif;

       If %parms > 25;
        strCsid#  = inCsid;                                                  //  26
       Endif;

       If %parms > 26;
        strCode#  = inCode;                                                  //  27
       Endif;

       If %parms > 27;
        strTrdt#  = inTrdt;                                                  //  28
       Endif;

       If %parms > 28;
        strTrtm#  = inTrtm;                                                  //  29
       Endif;

       If %parms > 29;
        strStat#  = inStat;                                                  //  30
       Endif;

       If %parms > 30;
        strIusr#  = inIusr;                                                  //  31
       Endif;

       If %parms > 31;
        strDcdt#  = inDcdt;                                                  //  32
       Endif;

       If %parms > 32;
        strFl03#  = inFl03;                                                  //  33
       Endif;

       If %parms > 33;
        strFl04#  = inFl04;                                                  //  34
       Endif;

       If %parms > 34;
        strFl05#  = inFl05;                                                  //  35
       Endif;

       If %parms > 35;
        strIdt#   = inIdt;                                                   //  36
       Endif;

       If %parms > 36;
        strVcfn#  = inVcfn;                                                  //  37
       Endif;

       If %parms > 37;
        strDpgm#  = inDpgm;                                                  //  38
       Endif;

       If %parms > 38;
        strDkey#  = inDkey;                                                  //   39
       Endif;

       If %parms > 39;
        strPath#  = inPath;                                                  //   40
       Endif;

       If %parms > 40;
        strRpyn#  = inRpyn;                                                  //   41
       Endif;


       WrtDart(strKey#:strComm#:strCdat#:strDcid#:strPnam#:
                strSecl#:strUser#:strPgm#:strFold#:strDifl#:
                strIpgm#:strFl01#:strFl02#:strRmc#:strSeq#:
                strSbyl#:strCom2#:strApid#:strIdoc#:strIque#:
                strImgf#:strIpth#:strPidx#:strQpty#:strDctp#:
                strCsid#:strCode#:strTrdt#:strTrtm#:strStat#:
                strIusr#:strDcdt#:strFl03#:strFl04#:strFl05#:
                strIdt#:strVcfn#:strDpgm#:strDkey#:strPath#:
                strRpyn#);

      /end-free

     P WriteDarts      E
      *** END ADD *** 03/13/12 *******************************************
