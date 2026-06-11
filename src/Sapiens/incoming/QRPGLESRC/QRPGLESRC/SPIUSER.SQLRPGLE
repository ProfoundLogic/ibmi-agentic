     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     *===============================================================================
     H nomain
     *===============================================================================
     *
     * Created   : 06/06/20
     * Programmer: C:33
     * Project   : SY420
     *
     *===============================================================================
      * Revisions:                                                                                 *
      *
      * 10/28/20 C: 079 Project: WC2671
      *                 Add prior selection procedure                                              *
     *===============================================================================
       //-------------------------------------------------------------------
       // File Definitions
       //-------------------------------------------------------------------
       //*** Beg Add *** 10/28/20 ******************************************
     fsdpsl1    uf a e           k disk
     fsmupl     if   e           k disk
       //*** End Add *** 10/28/20 ******************************************

      /copy QSYSINC/QRPGLESRC,QUSEC

     d/copy sPrUser
     d/copy sPrString
       //*** Beg Add *** 10/28/20 ******************************************
     d/copy sPrDate
       //*** End Add *** 10/28/20 ******************************************

     daryGroupSupp     s             10    dim(15) inz
     dstrHomeDir       s           1000    varying inz
     dstrPath          s           1000    varying inz

     dpsds           esds                  extname(SZQ1P)

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Get client ID
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d   outObjName                  10
     d   outObjType                   7
     d   outObjErr                    1

     ddsUSRI0300       ds         32767    qualified inz
     d #BytesRtnd                    10i 0
     d #BytesAvail                   10i 0
     d user                          10
     d signOnDtTime                  13
     d reserved1                      1
     d #invalidSignOn                10i 0
     d status                        10
     d pwdChgDt                       8
     d noPwd                          1
     d reserved2                      1
     d pwdExpInterval                10i 0
     d pwdExpDt                       8
     d #pwdDaysExp                   10i 0
     d pwdExpired                     1
     d userClass                     10
     d specialAuth                   15
     d groupProfile                  10
     d owner                         10
     d groupAuth                     10
     d assistanceLvl                 10
     d currentLib                    10
     d initMenu                      10
     d initMenuLib                   10
     d initPgm                       10
     d initPgmLib                    10
     d limitCapabilities...
     d                               10
     d description                   50
     d dspSignOnInfo                 10
     d limitSessions                 10
     d keyBuffering                  10
     d reserved3                      2
     d maxStorage                    10i 0
     d storageUsed                   10i 0
     d highSchdPriority...
     d                                1
     d jobD                          10
     d jobDLib                       10
     d accountCode                   15
     d msgQ                          10
     d msgQLib                       10
     d msgQMethod                    10
     d reserved4                      2
     d msgQSev                       10i 0
     d outQ                          10
     d outQLib                       10
     d printDevice                   10
     d specialEnv                    10
     d attnKeyPgm                    10
     d attnKeyPgmLib                 10
     d language                      10
     d countryRegion                 10
     d charCodeSet                   10i 0
     d options                       36
     d sortSeqTable                  10
     d sortSeqTableLib...
     d                               10
     d objectAuditing                10
     d actionAuditLvl                64
     d groupAuthType                 10
     d groupSuppOffset...
     d                               10i 0
     d #groupSupp                    10i 0
     d userId                        10i 0
     d groupId                       10i 0
     d homeDirOffset                 10i 0
     d homeDirLen                    10i 0
     d jobAttributes                 16
     d pathOffset                    10i 0
     d pathLen                       10i 0
     d groupMbr                       1
     d digitalCert                    1
     d charIdControl                 10
     d aspOffset                     10i 0
     d #asp                          10i 0
     d #aspReturned                  10i 0
     d aspLen                        10i 0
     d localPwdMgmt                   1
     d blockPwdChg                   10
     d entilementReq                  1
     d expInterval                   10i 0
     d expDt                          8
     d expAction                     10

     D QSYRUSRI        pr                  extpgm('QSYRUSRI')
     D   RcvVar                      53
     D   RcvVarLen                    9b 0 const
     D   Format                       8    const
     D   UserPrf                     10    const
     D   Error                             likeds(QUSEC)

      *=====================================================================
      * Always leave this as first procedure
      *=====================================================================
     pAAANothing       b                   export
     dAAANothing       pi
      /Free
       Exec SQL set option commit = *NONE;
      /End-free
     pAAANothing       e

      *=====================================================================
      * Retrieve the information from the system API
      *=====================================================================
     pGetUserInfo      b
     dGetUserInfo      pi
     d inUserProfile                 10    const

     di1               s             10i 0
      /Free
       Select;
       When inUserProfile = dsUSRI0300.user;
          Return;
       When inUserProfile = '*CURRENT' and dsUSRI0300.user = q1User;
          Return;
       Endsl;
       Reset dsUSRI0300;
       Reset aryGroupSupp;
       Reset strHomeDir;
       Reset strPath;
       QUSBPRV = 0; // Bytes provided in QUSEC error code parameter
       QSYRUSRI(dsUSRI0300:%size(dsUSRI0300):'USRI0300':inUserProfile:QUSEC);
       For i1 = 1 to dsUSRI0300.#groupSupp;
          aryGroupSupp(i1) =
           %subst(dsUSRI0300:dsUSRI0300.groupSuppOffset+1+(10*(i1-1)):10);
       Endfor;
       strHomeDir =
        %subst(dsUSRI0300:dsUSRI0300.homeDirOffset:dsUSRI0300.homeDirLen);
       strPath =
        %subst(dsUSRI0300:dsUSRI0300.pathOffset:dsUSRI0300.pathLen);
      /End-free
     pGetUserInfo      e

      *=====================================================================
      * Retrieve the profiles associated with the user
      *=====================================================================
     pGetAssociatedProfiles...
     p                 b                   export
     dGetAssociatedProfiles...
     d                 pi            10    dim(20)
     d inUserProfile                 10    const options(*nopass)

     di1               s             10i 0
     daryAssociatedProfiles...
     d                 s             10    dim(20) inz
     dstrUserProfile   s             10    inz('*CURRENT')
      /Free
       If %parms >= 1 and inUserProfile <> *blanks;
          strUserProfile = inUserProfile;
       Endif;
       GetUserInfo(strUserProfile);
       aryAssociatedProfiles(1) = dsUSRI0300.user;
       aryAssociatedProfiles(2) = dsUSRI0300.groupProfile;
       For i1 = 1 to dsUSRI0300.#groupSupp;
          aryAssociatedProfiles(i1+2) = aryGroupSupp(i1);
       Endfor;
       Return aryAssociatedProfiles;
      /End-free
     pGetAssociatedProfiles...
     p                 e
      //*** Beg Add *** 10/28/20 *******************************************
      *=====================================================================
      * Save or Retrieve the users prior selection in a screen file
      *=====================================================================
     pPriorSelection   b                   export
     dPriorSelection   pi             1n
     d inCompany                      3s 0 const
     d inUser                        10a   const
     d inScreenFile                  10a   const
     d inMode                         1a   const
     d data                        6500a   varying

       Select;

       // Get mode will Retrive existing record if it exist and return data
       When InMode = 'G';
          Chain(N)(inCompany:inUser:inScreenFile) pps01;
          If %Found(SDPSL1);
             data = PSdata;
             Return *on;
          Else;
             data = *blanks;
             Return *off;
          Endif;

       // Save mode will update existing record or write new record with screen
       When InMode = 'S';
          Chain (inCompany:inUser:inScreenFile) pps01;
          If not %Found(SDPSL1);
             PSco#  = inCompany;
             PSusr  = inUser;
             PSfile = inScreenFile;
             PSdata = data;
             PSiUsr = Q1user;
             PSiPgm = Q1pgm;
             PSiDt  = Dateto7(SystemDate());
             PSiTim = Timeto6(SystemTime());
             PSUser = Q1user;
             PSPgm  = Q1pgm;
             PSDt   = Dateto7(SystemDate());
             PStime = Timeto6(SystemTime());
             Write pps01;
          Else;
             PSdata = data;
             PSUser = Q1user;
             PSPgm  = Q1pgm;
             PSDt   = Dateto7(SystemDate());
             PStime = Timeto6(SystemTime());
             Update pps01;
          Endif;
          Return *on;
       Endsl;

     pPriorSelection   e
      //*** End Add *** 10/28/20 *******************************************
