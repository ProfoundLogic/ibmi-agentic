     H*****************************************************************
     H* Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
    H*
    H/TITLE F6211R - EDI FORMAT/TRAMSMISSION PROCESS
    H*               BASE (CELERITY VERSION)
    H*
    H*****************************************************************
    H*                                                               *
    H*  CREATED: 06/03/99                                            *
    H*           C:56  PROJECT: MC152  BASED ON W8074R               *
    H*                                                               *
    H*****************************************************************
    H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/06/17 C:079 Project: 25MUL                                 *
     H*                Do not lookup lic# in database                 *
     H*                                                               *
     H* 06/28/17 C:309 Project: 25MUL                                 *
     H*                Clear the NPI# after each EOB                  *
     H*                                                               *
     H* 06/23/17 C:180 Support 47657                                  *
     H*                Round units                                    *
     H*                                                               *
     H* 11/16/16 C:309 Project: 25MUL                                 *
     H*                Populate fields in the type 60 Common Service  *
     H*                Record based on TASB's Provider Information    *
     H*                File using NPI#                                *
     H*                                                               *
     H* 11/10/15 C:309 Support: 38266                                 *
     H*                837 EDI FEF1P record                           *
     H*                                                               *
     H* 10/21/15 C:079 Support: MC025 37996                           *
     H*                Remap HCFA and UB ICD-10 codes                 *
     H*                                                               *
     H* 06/11/15 C:079 Support: MC099 35316                           *
     H*                Update ICD-10 Indicator                        *
     H*                                                               *
     H* 04/16/15       Project: MC395                                 *
     H*          C:212 ICD10 TX 837 EDI Changes Hospital              *
     H*          C:079 ICD10 TX 837 EDI Changes physicianc and dental *
     H*                                                               *
     H* 09/10/14 C:296 SUPPORT: 86536                                 *
     H*                Remove patch which moved *blank in patient acc.*
     H*                number. User now wants patient acc. no. in     *
     H*                file TFEIP.                                    *
     H*                                                               *
     H* 09/01/14 C:296 SUPPORT: 86539                                 *
     H*                Populate discharge date correctly.             *
     H*                                                               *
     H* 08/06/13 C:062 SUPPORT: MC025 82993                           *
     H*                Populate Bill Id all bill types                *
     H*                                                               *
     H* 04/03/13 C:277 SUPPORT: 81748                                 *
     H*                Comment code xx9999999 and Correction for TX   *
     H*                Bill                                           *
     H*                                                               *
     H* 02/20/12 C:251 Support: 78735                                 *
     H*                Move -ve COPAID,LGPAID in correct format     e *
     H*                                                               *
     H* 09/08/11 C:273 Support: 77697                                 *
     H*                removing the condition the causes PSUBAS to be *
     H*                set with blanks                                *
     H*                                                               *
     H* 08/11/11 C:079 Project: TX075                                 *
     H*                837 EDI Changes for 9/1/2011                   *
     H*                                                               *
     H* 08/10/11 C:062 Support: 77625                                 *
     H*                Copy the mods.                                 *
     H*                                                               *
     H* 12/16/10 C:257 Support: 74444                                 *
     H*                Added processing for new field PAGR.           *
     H*                                                               *
     H* 05/18/09 C:198 Support: 67689                                 *
     H*                Referring provider NPI# processing             *
     H*                                                               *
     H* 02/25/09 C:079 SUPPORT : 67178                                *
     H*                Clear state license number for Pharmacy        *
     H*                                                               *
     H* 02/25/09 C:062 SUPPORT : 67178                                *
     H*                Change the load of rx npi number.              *
     H*                                                               *
     H* 02/17/09 C:227 SUPPORT : 67020                                *
     H*                837 Errors - Possible base upgrade problem     *
     H*                                                               *
     H* 02/12/09 C:079 Project: 67178                                 *
     H*                Load state license numbers from provider bills *
     H*                files                                          *
     H*                                                               *
     H* 01/29/09 C:189 SUPPORT : 66146                                *
     H*                Write 01 in bictcd for TX Hospital bills       *
     H*                                                               *
     H* 12/13/08 C:079 Project: TX048                                 *
     H*                Load state license numbers from extract files  *
     H*                                                               *
     H* 10/30/08 C:198 Project: TX048                                 *
     H*                Rendering Provider Lic# Processing             *
     H*                                                               *
     H* 07/03/08 C:198 Project: TX046                                 *
     H*                Add NPI#  and DRG# Processing                  *
     H*                                                               *
     H* 06/25/08 C:62 Project: 54048                                  *
     H*               Don't write 05 and 06 records if descriptions   *
     H*               blank                                           *
     H*                                                               *
     H* 06/16/08 C:62 Project: 53842                                  *
     H*               Correct modifier population                     *
     H*               Correct Dental License Number for TX            *
     H*                                                               *
     H* 05/28/08 C:62 Project: TX045                                  *
     H*               DEFAULT RENDERING STATE FOR TX AS TX
     H*                                                               *
     H* 08/14/07 C:180 SUPPORT: 58626
     H*                Fix problem of 05 & 06 records written without
     H*                zip code, name or any bill info
     H*                                                               *
     H*  04/10/07 C:188 CA002: 837 CA Medical EDI                     *
     H*                 Additional logic to support writing of DME    *
    H*                                                               *
    H*  02/23/07  C:079 SUPPORT: 56053                               *
    H*                  1. FOR HISTORICAL WE ARE SENDING 10 9'S IF   *
    H*                     THEY BILL DME ON A PHARMACY BILL          *
    H*                  2. IF NUMBER OF DAYS SUPPLY = 0 THEN SEND 7  *
    H*                     AS A DEFAULT                              *
    H*                                                               *
    H*  01/25/07  C:079 SUPPORT: 56053                               *
    H*                  1. FOR HISTORICAL WE ARE ONLY GOING TO SUBMIT*
    H*                     TWO EOB CODES                             *
    H*                  2. IF PLACE OF SERVICE IS NOT VALID THEN     *
    H*                  3. IF PLACE OF SERVICE BILL IS NOT VALID THEN*
    H*                     DEFAULT TO 11.                            *
     H*                                                               *
     H*  01/19/07 C:188 CA002: 837 CA Medical EDI                     *
     H*                 Implement CA 837 Medical EDI Requirements     *
    H*                                                               *
    H*  12/04/06  C:079 SUPPORT: 56053                               *
    H*                  1. USE BQ FOR PRINC PROC CODE QUAL DEFAULT   *
    H*                  2. IF THE ADJUSTMENT IS NEGATIVE SEND THE    *
    H*                     POSITIVE VALUE                            *
    H*                  3. ONLY SEND BILLING TYPE CODE FOR OUTPATIENT*
    H*                     BILLS 13x, 83x, 71x-79x                   *
    H*                                                               *
    H*  11/16/06  C:079 SUPPORT: 54910                               *
    H*                  BLANK OUT ROOMM AND ROOMS REV CODES          *
    H*                                                               *
    H*  09/12/06  C:079 PROJECT: 25MNZ                               *
    H*                  ADD RENDERING PROV. FOR PHARMACY             *
    H*                                                               *
    H*  07/18/06  C:079 PROJECT: 135MHP                              *
    H*                  1. ADD LOGIC TO READ IN SERVICE DATE ORDER   *
    H*                     FOR SUBMITTING HISTORICAL                 *
    H*                  2. IF NON PHARMACY BILL AND POS = 99 DEFAULT *
    H*                     TO 11                                     *
    H*                  3. OTHER DEFAULTS FOR HISTORICAL             *
    H*                                                               *
    H*  07/08/06  C:079 PROJECT: 25MNZ                               *
    H*                  ADD LOGIC FOR FACITLITY PROVIDER             *
    H*                  DONT LOAD PATIENT ACCOUNT NUMBER             *
    H*                                                               *
    H*  05/13/06  C:079 PROJECT: 25MNZ                               *
    H*                  ADD LOGIC FOR LATEST FILE SUBMISSION ISSUES  *
    H*                                                               *
    H*  11/13/05  C:079 PROJECT: 25MNZ                               *
    H*                  ADD LOGIC FOR TEXAS 837 CONTINUE             *
    H*                                                               *
    H*  02/18/05  C:079 PROJECT: 25MNZ                               *
    H*                  ADD LOGIC FOR TEXAS 837                      *
    H*                                                               *
    H*  11/06/01  C:053 SUPPORT MC048 # 29527                        *
    H*                  POPULATE RECORD COUNT (MF837T)               *
    H*                                                               *
    H* 07/05/01 C:53  Project: #28080                                *
    H*                Loaded blanks to discharge hour if there is    *
    H*                no discharge date.                             *
    H*                                                               *
    H* 01/29/01 C:15  Project: KY010                                 *
    H*                Chgd code to allow amount fields to contain    *
    H*                zeros.  (see 8/07/00 chg)                      *
    H*                                                               *
    H* 01/24/01 C:15  Project: KY010                                 *
    H*               -Corrected the 'Common Service' record layout.  *
    H*               -Load the SS# into patient acct# for pharmacy   *
    H*                bills.                                         *
    H*                                                               *
    H* 12/14/00 C:15  Project: KY010                                 *
    H*                Per KY, the revenue code should not be loaded  *
    H*                into the procedure code field if the procedure *
    H*                code is blank.                                 *
    H*                                                               *
    H* 08/07/00 C:15  Project: KY010                                 *
    H*               -Added code to blank fill data elements which   *
    H*                are loaded with zeros.                         *
    H*               -Per State of KY, send blanks in '# of units'   *
    H*                field.                                         *
    H*                                                               *
    H* 05/24/00 C:15  Project: KY010                                 *
    H*                Per Celerity, do not load the principal diag   *
    H*                code for HCFA bills.  Load the diag code into  *
    H*                the other diag code fields.                    *
    H*****************************************************************
    H*                                                               *
    H* INDICATOR USAGE                                               *
    H*                                                               *
    H* *IN10 = *ON DENOTES A PHYSICIAN BILL                          *
    H* *IN11 = *ON DENOTES A HOSPITAL BILL                           *
    H* *IN12 = *ON DENOTES A PHARMACY BILL                           *
    H* *IN13 = *ON DENOTES A DENTAL BILL                             *
    H*****************************************************************
    F* FORMAT MANAGEMENT FILE
     Ffemfl     uf   e           k disk
    F*
    F* LOG RECORD FILE
     Ffelgl3    uf   e           k disk
    F*
     F*** BEG ADD *** 07/18/06 *********************************************
     Ffelgl5    uf   e           k disk    rename(plg01:plg02)
     F*** END ADD *** 07/18/06 *********************************************
    F*
    F* EDI CONTROL FILE
     Ffeexl     if   e           k disk
    F*
    F* 837 FLAT FILE
     Ffef1p     o    e           k disk
    F*
    F* EOMB FILE
     Ffaeolg    if   e           k disk
    F*
    F* PHYSICIAN HEADER EXTRACT FILE
     Ffeehl     if   e           k disk
    F*
    F* HOSPITAL HEADER EXTRACT FILE
     Ffeeul     if   e           k disk
    F*
    F* PHARMACY HEADER EXTRACT FILE
     Ffeepl     if   e           k disk
    F*
    F* DENTAL HEADER EXTRACT FILE
     Ffeedl     if   e           k disk
    F*
    F* PHYSICAIN DETAIL EXTRACT FILE
     Ffedhl     if   e           k disk
    F*
    F* HOSPITAL DETIAL EXTRACT FILE
     Ffedul     if   e           k disk
    F*
    F* PHARMACY DETAIL EXTRACT FILE
     Ffedpl     if   e           k disk
    F*
    F* DENTAL DETAIL EXTRACT FILE
     Ffeddl     if   e           k disk
    F*
     Fftgil     if   e           k disk
     Fftgtl     if   e           k disk
    F*
     F*** BEG ADD *** 01/25/07 *********************************************
     Ffaeul     if   e           k disk    rename(peu01:peu02)
     F                                     prefix(E1:2)
     Ffddpl     if   e           k disk    rename(pdp01:pdp02)
     F                                     prefix(d1:2)
     F*** BEG ADD *** 01/25/07 *********************************************
     F*** BEG ADD *** 04/10/07 *********************************************
     Ffaepl     if   e           k disk    rename(pep01:pep02)
     F                                     prefix(E2:2)
     F*** BEG ADD *** 04/10/07 *********************************************
     F*** BEGIN ADD *** 06/16/08 *******************************************
     Ffddul     if   e           k disk    rename(pdu01:pdu02)
     F                                     prefix(d2:2)
     F*** END   ADD *** 06/16/08 *******************************************
     F*** BEG ADD *** 10/30/08 *******************************************
     Ffaehl     if   e           k disk    rename(peh01:peh02)
     F                                     prefix(e2:2)
     F*** END ADD *** 10/30/08 *******************************************
     F*** BEG ADD *** 02/12/09 *******************************************
     Ffaedl     if   e           k disk    rename(ped01:ped02)
     F                                     prefix(e3:2)
     F*** END ADD *** 02/12/09 *******************************************
     F*** BEG ADD *** 12/16/10 *******************************************
     Fnmnwl     if   e           k disk
     F*** END ADD *** 12/16/10 *******************************************

     F*** Beg Add *** 11/16/16 **********************************************
     * NPI Master database file for business address
     Ftrpnpimst if   e           k disk    usropn
     F*** End Add *** 11/16/16 **********************************************

     D*** Beg Add *** 02/20/12 **********************************************
     D xxpaid          s                   like(dhpaid)
     D xxpaid1         s                   like(dhpaid)
     D xxpchr          s             11
     D xxpos           s              3  0
     D*** End Add *** 02/20/12 **********************************************
    D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D                 ds
     D  gc                     1     20
     D                                     dim(10)
     D  lgbag1                 1      2
     D  lgbag2                 3      4
     D  lgbag3                 5      6
     D  lgbag4                 7      8
     D  lgbag5                 9     10
     D  lgbag6                11     12
     D  lgbag7                13     14
     D  lgbag8                15     16
     D  lgbag9                17     18
     D  lgbag0                19     20
     D                 ds
     D  rc                     1     30
     D                                     dim(10)
     D  lgbar1                 1      3
     D  lgbar2                 4      6
     D  lgbar3                 7      9
     D  lgbar4                10     12
     D  lgbar5                13     15
     D  lgbar6                16     18
     D  lgbar7                19     21
     D  lgbar8                22     24
     D  lgbar9                25     27
     D  lgbar0                28     30
     D                 ds
     D  dip                    1      4
     D                                     dim(4)
     D  xxdip1                 1      1
     D  xxdip2                 2      2
     D  xxdip3                 3      3
     D  xxdip4                 4      4
    D*
     D bil837        e ds                  extname(febip)
     D clm837        e ds                  extname(feclp)
     D phy837        e ds                  extname(fephp)
    D**********LIN837    E DSFEDLP
    D*
    D*-----------------------------------
    D* DATA STRUCTURES FOR 837 FLATFILE *
    D*-----------------------------------
    D*
    D*   HEADER
     D                 ds
     D header                       107
     D  hequal                        3    overlay(header)
     D  hesid                        15    overlay(header:4)
     D  hespcd                        9    overlay(header:20)
     D  herid                        15    overlay(header:29)
     D  herpcd                        9    overlay(header:45)
     D  hetsdt                        8    overlay(header:54)
     D  hetstm                        6    overlay(header:62)
     D  heotdt                        8    overlay(header:68)
     D  heottm                        6    overlay(header:76)
     D  hetpin                        1    overlay(header:82)
     D  hetset                        3    overlay(header:83)
     D  herls#                        2    overlay(header:86)
     D  heacde                        2    overlay(header:88)
     D  herepp                       16    overlay(header:90)
     D  hestyp                        2    overlay(header:106)
    D*   INSURER
     D                 ds
     D insur                         55
     D  inqual                        2    overlay(insur)
     D  inname                       35    overlay(insur:3)
     D  inpcod                        9    overlay(insur:38)
     D  infein                        9    overlay(insur:47)
    D*   CLIAM ADMINISTRATOR
     D                 ds
     D admin                         55
     D  adqual                        2    overlay(admin)
     D  adname                       35    overlay(admin:3)
     D  adpcod                        9    overlay(admin:38)
     D  adfein                        9    overlay(admin:47)
    D*   MANAGED CARE ORGANIZATION
     D                 ds
     D mco                          179
     D  mcqual                        2    overlay(mco)
     D  mcname                       35    overlay(mco:3)
     D  mcpadr                       40    overlay(mco:38)
     D  mcsadr                       40    overlay(mco:78)
     D  mccity                       30    overlay(mco:118)
     D  mcstat                        2    overlay(mco:148)
     D  mcpcod                        9    overlay(mco:150)
     D  mcctry                        3    overlay(mco:159)
     D  mcfein                        9    overlay(mco:162)
     D  mcanum                        9    overlay(mco:171)
    D*   EMPLOYER
     D                 ds
     D emplyr                       216
     D  emqual                        2    overlay(emplyr)
     D  emname                       35    overlay(emplyr:3)
     D  empadr                       40    overlay(emplyr:38)
     D  emsadr                       40    overlay(emplyr:78)
     D  emcity                       30    overlay(emplyr:118)
     D  emstat                        2    overlay(emplyr:148)
     D  empcod                        9    overlay(emplyr:150)
     D  emfein                        9    overlay(emplyr:159)
     D  emphn#                       15    overlay(emplyr:168)
     D  emsfin                        1    overlay(emplyr:183)
    D*** BEG ADD *** 02/18/05 ****************************************
     D  empol#                       30    overlay(emplyr:184)
     D  emcnty                        3    overlay(emplyr:214)
    D*** END ADD *** 02/18/05 ****************************************
    D*   PROVIDER
     D                 ds
     D prov                          62
     D  prqual                        2    overlay(prov)
     D  prfein                       10    overlay(prov:3)
     D  prname                       30    overlay(prov:13)
     D  prbwci                       20    overlay(prov:43)
    D*   HOSPITAL
     D                 ds
     D hosp                          16
     D  hoqual                        2    overlay(hosp)
     D  hoatyp                        1    overlay(hosp:3)
     D  hoasrc                        1    overlay(hosp:4)
     D  hopsts                        2    overlay(hosp:5)
     D  hodayc                        5    overlay(hosp:7)
     D  hodayn                        5    overlay(hosp:12)
    D*   HEALTH INFORMATION
     D                 ds
     D hinfo                         69
     D  hiqual                        2    overlay(hinfo)
     D  hicodq                        3    overlay(hinfo:3)
     D  hivocd                       16    overlay(hinfo:6)
     D  hidtfq                        3    overlay(hinfo:22)
     D  hidtpd                       16    overlay(hinfo:25)
     D  himona                       14    overlay(hinfo:41)
     D  hiqnty                       15    overlay(hinfo:55)
    D*   CLAIM LEVEL EOB TYPE/BILL ADJ REASON CODES
     D                 ds
     D tyeob                         70
     D  tyqual                        2    overlay(tyeob)
     D  tyajgc                        2    overlay(tyeob:3)
     D  tyajrc                        4    overlay(tyeob:5)
     D  tyaja1                       11    overlay(tyeob:9)
     D  tyaju1                        7    overlay(tyeob:20)
     D  tyajr2                        4    overlay(tyeob:27)
     D  tyaja2                       11    overlay(tyeob:31)
     D  tyaju2                        7    overlay(tyeob:42)
     D  tyajr3                        4    overlay(tyeob:49)
     D  tyaja3                       11    overlay(tyeob:53)
     D  tyaju3                        7    overlay(tyeob:64)
    D*   SERVICE NUMBER
     D                 ds
     D serv#                          8
     D  sequal                        2    overlay(serv#)
     D  selin#                        6    overlay(serv#:3)
    D*   PROFESSIONAL SERVICE
     D                 ds
     D pserv                        202
     D  psqual                        2    overlay(pserv)
     D  pssvq1                        2    overlay(pserv:3)
     D  psprbc                        6    overlay(pserv:5)
     D  pssvq2                        2    overlay(pserv:11)
     D  psproc                        6    overlay(pserv:13)
     D  pssmd1                        2    overlay(pserv:19)
     D  pssmd2                        2    overlay(pserv:21)
     D  pssmd3                        2    overlay(pserv:23)
     D  pssmd4                        2    overlay(pserv:25)
     D  pspdsc                       40    overlay(pserv:27)
     D  psdip1                        1    overlay(pserv:67)
     D  psdip2                        1    overlay(pserv:68)
     D  psdip3                        1    overlay(pserv:69)
     D  psdip4                        1    overlay(pserv:70)
     D  pspdia                       80    overlay(pserv:71)
     D  pschrg                       14    overlay(pserv:151)
     D  psubas                        2    overlay(pserv:165)
     D  psuniq                       15    overlay(pserv:167)
     D  pspslc                        2    overlay(pserv:182)
     D  psfacc                        2    overlay(pserv:184)
     D  psstyp                        2    overlay(pserv:186)
     D  psmprc                        1    overlay(pserv:188)
     D  psncam                       14    overlay(pserv:189)
    D*   INSTITUTIONAL SERVICE
     D                 ds
     D iserv                        201
     D  isqual                        2    overlay(iserv)
     D  issvq1                        2    overlay(iserv:3)
     D  isprbc                        6    overlay(iserv:5)
     D  issvq2                        2    overlay(iserv:11)
     D  isproc                        6    overlay(iserv:13)
     D  issmd1                        2    overlay(iserv:19)
     D  issmd2                        2    overlay(iserv:21)
     D  issmd3                        2    overlay(iserv:23)
     D  issnd4                        2    overlay(iserv:25)
     D  ispdsc                       40    overlay(iserv:27)
     D  ispdia                       80    overlay(iserv:67)
     D  ischrg                       14    overlay(iserv:147)
     D  isubas                        2    overlay(iserv:161)
     D  isuniq                       15    overlay(iserv:163)
     D  isdrat                       10    overlay(iserv:178)
     D  isncam                       14    overlay(iserv:188)
    D*   PHARMACY
     D                 ds
     D rxinfo                       207
     D  rxqual                        2    overlay(rxinfo)
     D  rxprs#                       30    overlay(rxinfo:3)
     D  rxsvq1                        2    overlay(rxinfo:33)
     D  rxprbc                       11    overlay(rxinfo:35)
     D  rxsvq2                        2    overlay(rxinfo:46)
     D  rxproc                       11    overlay(rxinfo:48)
     D  rxsmd1                        2    overlay(rxinfo:59)
     D  rxsmd2                        2    overlay(rxinfo:61)
     D  rxsmd3                        2    overlay(rxinfo:63)
     D  rxsnd4                        2    overlay(rxinfo:65)
     D  rxdesc                       80    overlay(rxinfo:67)
     D  rx#rfl                       10    overlay(rxinfo:147)
     D  rxgenr                        1    overlay(rxinfo:157)
     D  rxdisw                        1    overlay(rxinfo:158)
     D  rx#dsu                       10    overlay(rxinfo:159)
     D  rxpldt                        8    overlay(rxinfo:169)
     D  rxchrg                       14    overlay(rxinfo:177)
    D*** BEG ADD *** 02/18/05 ****************************************
     D  rxqtyd                        4    overlay(rxinfo:191)
     D  rxdspf                       11    overlay(rxinfo:195)
     D  rxbcdt                        2    overlay(rxinfo:206)
    D*** END ADD *** 02/18/05 ****************************************
    D*   DENTAL
     D                 ds
     D dental                        74
     D  dequal                        2    overlay(dental)
     D  desvq1                        2    overlay(dental:3)
     D  deprbc                        6    overlay(dental:5)
     D  desvq2                        2    overlay(dental:11)
     D  deproc                        6    overlay(dental:13)
     D  dedesc                       40    overlay(dental:19)
     D  depslc                        2    overlay(dental:59)
     D  dechrg                       14    overlay(dental:61)
    D*   DME
     D                 ds
     D dme                          183
     D  dmqual                        2    overlay(dme)
     D  dmsvq1                        2    overlay(dme:3)
     D  dmprbc                        6    overlay(dme:5)
     D  dmsvq2                        2    overlay(dme:11)
     D  dmproc                        6    overlay(dme:13)
     D  dmdesc                       40    overlay(dme:19)
    D*** BEG ADD *** 02/18/05 ****************************************
     D  dmchrr                       11    overlay(dme:59)
     D  dmchrp                       11    overlay(dme:70)
     D  dmbfcd                        1    overlay(dme:81)
    D*** END ADD *** 02/18/05 ****************************************
    D*** BEG ADD *** 04/10/07 ****************************************
     D  dmfil1                       77    overlay(dme:82)
     D  dmubas                        2    overlay(dme:159)
     D  dmuqty                       15    overlay(dme:161)
     D  dmfil2                        8    overlay(dme:176)
    D*** END ADD *** 04/10/07 ****************************************
    D*   COMMON SERVICE
     D                 ds
     D comser                       713
     D  coqual                        2    overlay(comser)
     D  codrty                        3    overlay(comser:3)
     D  cobded                       16    overlay(comser:6)
     D  copaid                       14    overlay(comser:22)
     D  cotusp                       15    overlay(comser:36)
     D  coppay                       14    overlay(comser:51)
     D  cooexp                       15    overlay(comser:65)
     D  coeobt                       80    overlay(comser:80)
     D  cosagc                        2    overlay(comser:160)
     D  coeob1                        4    overlay(comser:162)
     D  coeob2                        4    overlay(comser:166)
     D  coeob3                        4    overlay(comser:170)
     D  coeob4                        4    overlay(comser:174)
     D  coeob5                        4    overlay(comser:178)
     D  coeob6                        4    overlay(comser:182)
     D  coeob7                        4    overlay(comser:186)
     D  coeob8                        4    overlay(comser:190)
     D  coeob9                        4    overlay(comser:194)
     D  coeo10                        4    overlay(comser:198)
    D*** BEGIN DLT *** 01/24/01 ***************************************
    D************************************* 202 206 CORVBC
    D************************************* 207 209 CORVPC
    D*** END DLT ***** 01/24/01 ***************************************
    D*** BEGIN ADD *** 01/24/01 ***************************************
     D  corvbc                        4    overlay(comser:202)
     D  corvpc                        4    overlay(comser:206)
    D*** END ADD ***** 01/24/01 ***************************************
    D*** BEG ADD *** 02/18/05 ****************************************
     D  coralc                       30    overlay(comser:210)
     D  corcty                       30    overlay(comser:240)
     D  corcnt                        3    overlay(comser:270)
     D  corfei                        9    overlay(comser:273)
     D  corfnm                       15    overlay(comser:282)
     D  corlnm                        4    overlay(comser:297)
     D  corgnm                       40    overlay(comser:301)
     D  cormed                       30    overlay(comser:341)
     D  cormid                       15    overlay(comser:371)
     D  cornat                       30    overlay(comser:386)
     D  corpos                        9    overlay(comser:416)
     D  corad1                       40    overlay(comser:425)
     D  corspc                       10    overlay(comser:465)
     D  corad2                       40    overlay(comser:475)
     D  corspl                       30    overlay(comser:515)
     D  corst                         2    overlay(comser:545)
     D  corstl                       30    overlay(comser:547)
     D  copos                         2    overlay(comser:577)
     D  cofill                        3    overlay(comser:579)
     D  comod1                        2    overlay(comser:582)
     D  comod2                        2    overlay(comser:584)
     D  comod3                        2    overlay(comser:586)
     D  comod4                        2    overlay(comser:588)
     D  cosadj                       11    overlay(comser:590)
     D  cosunt                        7    overlay(comser:601)
     D  cotaut                       30    overlay(comser:608)
     D  coctty                        2    overlay(comser:638)
     D  copagr                        2    overlay(comser:640)
     D  cosad2                       11    overlay(comser:642)
     D  cosun2                        7    overlay(comser:653)
     D  cosad3                       11    overlay(comser:660)
     D  cosun3                        7    overlay(comser:671)
     D  cosad4                       11    overlay(comser:678)
     D  cosun4                        7    overlay(comser:689)
     D  cosad5                       11    overlay(comser:696)
     D  cosun5                        7    overlay(comser:707)
    D*** END ADD *** 02/18/05 ****************************************
    D*   TRAILER
     D                 ds
     D trail                         12
     D  trqual                        3    overlay(trail)
     D  trdcnt                        9    overlay(trail:4)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##ar            s              1
     D ##edi           s              1
     D ##fsch          s              1
     D ##mcif          s              1
     D ##med           s              1
     D ##mss           s              1
     D ##npwc          s              1
     D ##ntwk          s              1
     D ##prvb          s              1
     D ##rhab          s              1
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D hlcase          s                   like(lgcase)
     D hlco#           s                   like(lgco#)
     D hldiv           s                   like(lgdiv)
     D hlemp#          s                   like(lgemp#)
     D hlfein          s                   like(lgfein)
     D hlfnd           s                   like(lgfnd)
     D hlfyr           s                   like(lgfyr)
     D hlmod           s                   like(lgmod)
     D hlsub#          s                   like(lgsub#)
     D kkcase          s                   like(lgcase)
     D kkco#           s                   like(lgco#)
     D kkeob#          s                   like(lgeob#)
     D kkfnd           s                   like(lgfnd)
     D kkfyr           s                   like(lgfyr)
     D kkjst           s                   like(lgjst)
     D kkkey           s                   like(gtkey)
     D kklseq          s                   like(lglseq)
     D kkotdt          s                   like(lgotdt)
     D kkottm          s                   like(lgottm)
     D kkrcn           s                   like(lgrcn)
     D kksent          s                   like(lgotdt)
     D kksub#          s                   like(lgsub#)
     D kktbid          s                   like(gttbid)
     D kktset          s              3
     D qqco#           s              3
     D qqjst           s              2
     D qqotdt          s              8
     D qqottm          s              6
     D qqtset          s              3
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0
     D xxdate          s              8  0
     D xxdat8          s              8  0
     D xxdip           s              4
     D xxphn#          s             10
     D xxrcnt          s              9  0
     D xxsadj          s                   like(dhchrg)
     D xxtim4          s              4  0
     D xxtrfg          s                   like(*inlr)
     D xx12a           s             12
     D xx8a            s              8
     D*** BEG ADD *** 02/18/05 *********************************************
     D xxunts          s              5  0
     D*** END ADD *** 02/18/05 *********************************************
     D*** BEG ADD *** 07/03/08 *********************************************
     D xxdrg           s              5
     D*** END ADD *** 07/03/08 *********************************************
     D*** Beg Chg *** 09/01/14 **********************************************
     D xxdchr          s              4A
     D*** End Chg *** 09/01/14 **********************************************

     D*** Beg Add *** 11/16/16 **********************************************
     D strNPIR         s             10  0
     D*** End Add *** 11/16/16 **********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*   INITIALIZATION SUBROUTINE
    C*
     C                   exsr      srinit
    C*
    C*  VERIFY EDI CONTROL FLAG IS 'Y'
    C*
     C                   call      'SA001R'
     C                   parm                    ##wcif
     C                   parm                    ##mcif
     C                   parm                    ##wcin
     C                   parm                    ##ar
     C                   parm                    ##ntwk
     C                   parm                    ##ur
     C                   parm                    ##med
     C                   parm                    ##edi
     C                   parm                    ##wcdk
     C                   parm                    ##rhab
     C                   parm                    ##mss
     C                   parm                    ##fsch
     C                   parm                    ##prvb
     C                   parm                    ##npwc
    C*
     C                   if        ##edi <> 'Y'
     C                   move      *on           *inlr
     C                   return
     C                   endif
    C*
    C*  RETRIEVE THE EDI CONTROL RECORD
    C*
     C                   clear                   kktset
     C     exky          chain     pex01                              60
     C                   if        *in60 = *on
     C*** Beg Add *** 11/16/16 ****************************
     C                   exsr      srEnd
     C*** End Add *** 11/16/16 ****************************
     C                   goto      term
     C                   endif
    C*
    C*  RETRIEVE THE TRANSMISSION MANAGEMENT RECORD.
    C*
     C                   move      '837'         kktset
     C     mfky          chain     pmf01                              60
     C                   if        *in60 = *on
     C*** Beg Add *** 11/16/16 ****************************
     C                   exsr      srEnd
     C*** End Add *** 11/16/16 ****************************
     C                   goto      term
     C                   endif
    C*
    C* IF THIS IS AN ORIGINAL (MFSTAT = A), THEN CLEAR KKOTDT AND
    C* KKOTTM TO GET NEW LOG RECORDS
    C*
     C                   if        mfstat = 'A'
     C                   eval      kkotdt = 0
     C                   eval      kkottm = 0
     C                   endif
    C*
    C*  PROCESS ALL 837 LOG RECORDS
    C*
     C*** BEG CHG *** 07/18/06 *********************************************
     C**** lgky          chain     plg01                              60
     C  N71lgky          chain     plg01                              60
     C   71lgky          chain     plg02                              60
     C*** END CHG *** 07/18/06 *********************************************
     C                   dow       *in60 = *off
     C                   if        lgstat <> 'H'
    C*
    C*  MAINLINE OF PROGRAM - LOOP THRU EXTRACT MANAGEMENT RECORDS
    C*  1) IF THIS IS A RE-TRANSMISSION, PROCESS ALL EXTRACTED RECORDS
    C*      EQUAL TO THE ORIGINAL TRANSMISSION DATE.
    C*  2) IF THIS IS AN ORIGINAL TRANSMISSION, PROCESS ALL EXTRACTED
    C*      RECORDS WHICH HAVEN'T ALREADY BEEN FORMATED AND THE EXTRACT
    C*      DATE IS LESS THAN OR EQUAL TO THE TRANSMISSION DATE.
    C*
     C                   if        mfstat = 'R'
     C                             or mfstat = 'A'
     C                             and lgsts = 'EX'
    C*
    C*  FIRST TIME THROUGH, WRITE THE TRANSMISSION HEADER.
    C*
     C                   if        xxtrfg = *off
     C                   exsr      srxhdr
     C                   move      *on           xxtrfg
     C                   endif
    C*
    C* WRITE NEW INSURER, CLAIM ADMINISTRATOR AND MCO ORGANIZATION
    C* RECORD WHEN THE COMPANY NUMBER CHANGES
    C*
      ******* BEGIN ADD *******  08/14/07 **********************************
     C                   IF        LGINZP = *ZEROS AND LGINNM = *BLANKS
     C                   ELSE
      ******* END ADD *********  08/14/07 **********************************
     C                   exsr      srxins
      ******* BEGIN ADD *******  08/14/07 **********************************
     C                   ENDIF
      ******* END ADD *********  08/14/07 **********************************
      *
      ******* BEGIN ADD *******  08/14/07 **********************************
     C                   IF        LGCAZP = *ZEROS AND LGCANM = *BLANKS
     C                   ELSE
      ******* END ADD *********  08/14/07 **********************************
     C                   exsr      srxcla
      ******* BEGIN ADD *******  08/14/07 **********************************
     C                   ENDIF
      ******* END ADD *********  08/14/07 **********************************
      *
     C                   if        lgmcnm <> *blanks
     C                   exsr      srxmco
     C                   endif
     C                   eval      hlco# = lgco#
    C*
    C*  PROCESS ALL 837 DETAIL ASSOCIATED WITH EACH EXTRACT (837)
    C*
    C* DETERMINE TYPE OF BILL
    C* ACCESS HEADER EXTRACT RECORD BASED ON TYPE OF BILL
    C* SET INDICATORS TO IDENTIFY TYPE OF BILL FOR LATER PROCESSING
    C*
    C*
     C                   eval      kkco# = lgco#
     C                   eval      kksub# = lgsub#
     C                   eval      kkfnd = lgfnd
     C                   eval      kkfyr = lgfyr
     C                   eval      kkcase = lgcase
     C                   eval      kkeob# = lgeob#
     C                   eval      kkrcn = lgrcn
     C                   eval      kklseq = lglseq
     C     eoky          chain     peo01                              99
     C                   if        *in99 = *off
     C                   eval      *in10 = *off
     C                   eval      *in11 = *off
     C                   eval      *in12 = *off
     C                   eval      *in13 = *off
     C                   select
     C                   when      eobtyp = 'N'
     C     ehky          chain     peh01                              98
     C                   eval      *in10 = *on
     C                   when      eobtyp = 'H'
     C     euky          chain     peu01                              98
     C                   eval      *in11 = *on
     C                   when      eobtyp = 'X'
     C     epky          chain     pep01                              98
     C                   eval      *in12 = *on
     C                   when      eobtyp = 'D'
     C     edky          chain     ped01                              98
     C                   eval      *in13 = *on
     C                   endsl
    C*
     C                   if        *in98 = *off
    C*
    C*** EXECUTE NECESSARY SUBROUTINES
    C*
     C                   exsr      srxemp
     C                   if        lgjst = 'OH'
     C                   exsr      srxprv
     C                   endif
     C                   exsr      srxclm
     C                   exsr      srxbil
    C*
    C* WRITE HOSPTIAL RECORD IF HOSPITAL BILL (*IN11 = *ON)
    C*
     C                   if        *in11 = *on
     C                   exsr      srxhos
     C                   endif
    C*
    C* WRITE PHYSICIAN BILL LEVEL RECORD FOR ALL BILLS
    C* REPEAT FOR DIFFERENT PROVIDER TYPES
    C*
     C*** BEG ADD *** 02/18/05 *********************************************
     C                   clear                   f1data
     C                   clear                   phy837
    C*** BEG ADD *** 05/13/06 ****************************************
     C                   movel     lggtki        phgati
    C*** END ADD *** 05/13/06 ****************************************
    C*
    C* Billing provider
    C*
     C                   move      '42'          phqual
     C                   move      '85'          phrole
     C                   move      '1'           phenty
     C*** END ADD *** 02/18/05 *********************************************
     C                   exsr      srxphy
     C*** BEG ADD *** 02/18/05 *********************************************
    C*
    C* Referring provider
    C*
     C                   if        lgjst = 'TX' and *in11 = *on
    C*
     C                   move      '42'          phqual
     C                   move      'DN'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
     C*** END ADD *** 02/18/05 *********************************************
     C*** BEG ADD *** 11/13/05 *********************************************
    C*
     C                   if        lgjst = 'TX' and *in12 = *on
    C*
     C                   move      '42'          phqual
     C                   move      'DN'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
    C*
    C* Facility
    C*
     C                   if        lgjst = 'TX' and *in10 = *on
    C*
     C                   move      '42'          phqual
     C                   move      '61'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
     C*
     C*** END ADD *** 02/18/05 *********************************************
     C*
     C*** BEG ADD *** 01/19/07 *********************************************
    C*
     C                   if        lgjst = 'CA' and *in11 = *on
    C*
     C                   move      '42'          phqual
     C                   move      '61'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
     C*** END ADD *** 01/19/07 *********************************************
     C*
     C*** BEG ADD *** 05/13/06 *********************************************
    C*
    C* Dental - Rendering
    C*
     C                   if        lgjst = 'TX' and *in13 = *on
    C*
     C                   move      '42'          phqual
     C                   move      '82'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
    C*
     C*** BEG ADD *** 01/19/07 *********************************************
    C* CA - Rendering
    C*
     C                   if        lgjst = 'CA'
    C*
    C                   move      '42'          phqual
    C                   move      '82'          phrole
    C                   move      '1'           phenty
    C                   exsr      srxphy
    C*
    C                   endif
     c*** END ADD *** 01/19/07 *********************************************
     C*** END ADD *** 05/13/06 *********************************************
     C*** BEG ADD *** 09/12/06 *********************************************
    C*
    C* Pharmacy - Rendering
    C*
     C                   if        lgjst = 'TX' and *in12 = *on
    C*
     C                   move      '42'          phqual
     C                   move      '82'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
     C*** END ADD *** 09/12/06 *********************************************
     C*** BEG ADD *** 12/13/08 *********************************************
    C*
    C* Physician - Rendering
    C*
    C*
     C                   if        lgjst = 'TX' and *in10 = *on
    C*
     C                   move      '42'          phqual
     C                   move      '82'          phrole
     C                   move      '1'           phenty
     C                   exsr      srxphy
    C*
     C                   endif
    C*
     C*** END ADD *** 12/13/08 *********************************************
    C*
     C                   if        lgjst = 'OH'
     C                   exsr      srxhlt
     C                   endif
    C*
     C                   eval      x = 1
     C*** BEG CHG *** 09/12/06 *********************************************
     C*****************  dow       rc(x) <> *blanks
     C*****************            and x <= 10
     C                   dow       x <= 10
     C                   if        rc(x) <> *blanks
     C*** END CHG *** 09/12/06 *********************************************
     C                   exsr      srxtye
     C*** BEG ADD *** 09/12/06 *********************************************
     C                   endif
     C*** END ADD *** 09/12/06 *********************************************
     C                   eval      x = x + 1
     C                   enddo
    C*
    C* CYCLE THROUGH LINE ITEMS FOR BILL
    C*
     C                   select
    C*
     C                   when      *in10 = *on
     C     dhky          chain     pdh01                              60
     C                   dow       *in60 = *off
      /Free
       //*** Begin Add *** 06/23/17 *******************************************
       If dhunts > *zero and dhunts < 1;
          dhunts = 1;
       Endif;
       //*** End   Add *** 06/23/17 *******************************************
      /End-Free
     C                   exsr      srxsv#
     C                   exsr      srxpsv
     C                   exsr      srxcom
     C     dhky          reade     pdh01                                  60
     C                   enddo
    C*
     C                   when      *in11 = *on
     C     duky          chain     pdu01                              60
     C                   dow       *in60 = *off
      /Free
       //*** Begin Add *** 06/23/17 *******************************************
       If duunts > *zero and duunts < 1;
          duunts = 1;
       Endif;
       //*** End   Add *** 06/23/17 *******************************************
      /End-Free
     C                   exsr      srxsv#
     C                   exsr      srxist
     C                   exsr      srxcom
     C     duky          reade     pdu01                                  60
     C                   enddo
    C*
     C                   when      *in12 = *on
     C     dpky          chain     pdp01                              60
     C                   dow       *in60 = *off
       //*** Begin Add *** 06/23/17 *******************************************
      /Free
       If dpunts > *zero and dpunts < 1;
          dpunts = 1;
       Endif;
      /End-Free
       //*** End   Add *** 06/23/17 *******************************************
     C                   exsr      srxsv#
     C*** BEG ADD *** 02/23/07 *********************************************
     C                   if        dprxdy = 0
     C                   eval      dprxdy = 7
     C                   endif
     C*** END ADD *** 02/23/07 *********************************************
     C                   exsr      srxrx
     C*
     C*** BEG ADD *** 04/10/07 *********************************************
     C     euk1          chain     pep02                              99
     C                   if        *in99 = *off and
     C                             e2btcd = 'DM' and
     C                             eoast <> 'TX'
     C                   exsr      srxdme
     C                   endif
     C*** BEG ADD *** 04/10/07 *********************************************
     C*
     C                   exsr      srxcom
     C     dpky          reade     pdp01                                  60
     C                   enddo
    C*
     C                   when      *in13 = *on
     C     ddky          chain     pdd01                              60
     C                   dow       *in60 = *off
      /Free
       //*** Begin Add *** 06/23/17 *******************************************
       If ddunts > *zero and ddunts < 1;
          ddunts = 1;
       Endif;
       //*** End   Add *** 06/23/17 *******************************************
      /End-Free
     C                   exsr      srxsv#
     C                   exsr      srxden
     C                   exsr      srxcom
     C     ddky          reade     pdd01                                  60
     C                   enddo
    C*
     C                   endsl
    C*
     C                   eval      xxrcnt = xxrcnt + 1
    C*
     C                   if        mfstat <> 'R'
     C                   move      mfotdt        lgotdt
     C                   move      mfottm        lgottm
     C                   move      'SN'          lgsts
     C*** BEG CHG *** 07/18/06 *********************************************
     C****************** update    plg01
     C  N71              update    plg01
     C   71              update    plg02
     C*** END CHG *** 07/18/06 *********************************************
     C                   endif
     C                   endif
    C*
     C                   endif
    C*
    C*  PROCESS ONLY THE NUMBER SPECIFIED IN EDI CONTROL
    C*
     C                   if        xxrcnt >= exmaxt
     C                             and exmaxt > 0
     C                   leave
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
     C*** BEG CHG *** 07/18/06 *********************************************
     C**** lgky          reade     plg01                                  60
     C  N71lgky          reade     plg01                                  60
     C   71lgky          reade     plg02                                  60
     C*** END CHG *** 07/18/06 *********************************************
     C                   enddo
    C*
     C                   if        xxtrfg = *on
     C                   exsr      srxtrl
     C                   endif
    C*
     C                   move      'P'           mfstat
    C*** BEG ADD *** 11/06/01 ****************************
     C                   eval      mf837t = xxrcnt
    C*** END ADD *** 11/06/01 ****************************
     C                   update    pmf01
    C*
     C     term          tag
     C                   move      *on           *inlr
    C*
    C*----------------------------------------------------------------
    C* SRINT - DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
    C*----------------------------------------------------------------
     C     srinit        begsr
    C*
    C*  WMUPL - USER PARAMETER FILE
    C*
     C     upky          klist
     C                   kfld                    q1user
    C*
    C*  FMLGL  - EDI LOG FILE
    C*
     C     lgky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkjst
     C                   kfld                    kkotdt
     C                   kfld                    kkottm
    C*
    C*  FEEXL  - EDI CONTROL FILE
    C*
     C     exky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkjst
     C                   kfld                    kktset
    C*
    C*  FEMFL - SYSTEM MANAGEMENT RECORDS
    C*
     C     mfky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkjst
     C                   kfld                    kktset
     C                   kfld                    kkotdt
     C                   kfld                    kkottm
    C*
    C*  FEAOL - KEY LIST FOR EOMB FILE
    C*
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C*  FEEHL - KEY LIST FOR PHYSICIAN EXTRACT HEADER FILE
    C*
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEEUL - KEY LIST FOR HOSPITAL EXTRACT HEADER FILE
    C*
     C     euky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
     C*** BEG ADD *** 01/25/07 *********************************************
    C*
     C     euk1          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C*** END ADD *** 01/25/07 *********************************************
    C*
    C*  FEEPL - KEY LIST FER PHARMACY EXTRACT HEADER FILE
    C*
     C     epky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEEDL - KEY LIST FOR DENTAL EXTRACT HEADER FILE
    C*
     C     edky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEDHL - KEY LIST FOR PHYSICIAN EXTRACT DETAIL FILE
    C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEDUL - KEY LIST FOR HOSPITAL EXTRACT DETAIL FILE
    C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEDPL - KEY LIST FER PHARMACY EXTRACT DETAIL FILE
    C*
     C     dpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
    C*  FEDDL - KEY LIST FOR DENTAL EXTRACT DETAIL FILE
    C*
     C     ddky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    kklseq
    C*
     C*** BEG ADD *** 02/13/07 *********************************************
    C*
    C*  FDDPL - EOMB Pharmacy Detail Transaction File
    C*
     C     ddk1          klist
     C                   kfld                    dpco#
     C                   kfld                    dpsub#
     C                   kfld                    dpfnd
     C                   kfld                    dpfyr
     C                   kfld                    dpcase
     C                   kfld                    dpeob#
     C                   kfld                    dpdseq
     C*** BEGIN ADD *** 06/16/08 *******************************************
    C*
    C*  FDDUL - EOMB Hospital Detail Transaction File
    C*
     C     duky2         klist
     C                   kfld                    duco#
     C                   kfld                    dusub#
     C                   kfld                    dufnd
     C                   kfld                    dufyr
     C                   kfld                    ducase
     C                   kfld                    dueob#
     C                   kfld                    dudseq
     C*** END   ADD *** 06/16/08 *******************************************
    C*
     C*** BEG ADD *** 02/13/07 *********************************************
    C*
    C*  FTGIL - Key list for the System Table ID file
    C*
     C     giky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
    C*
    C*  FTGTL - Key list for the System Table file
    C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*** BEG ADD *** 10/30/08 *********************************************
    C*  FAEHL - Key list
    C*
     C     ehky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*** END ADD *** 10/30/08 *********************************************
     C*** BEG ADD *** 02/12/09 *********************************************
    C*  FAEDL - Key list
    C*
     C     edky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*** END ADD *** 02/12/09 *********************************************
     C*** BEG ADD *** 12/16/10 *********************************************
    C*  NMNWL - Key list
    C*
     C     NWKEY         klist
     C                   kfld                    kkco#
     C                   kfld                    eontwk
     C*** END ADD *** 12/16/10 *********************************************
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
    C*  PROGRAM VARIABLE DEFINITIONS
    C*
    C*** BEG ADD *** 02/18/05 ****************************************
    C*** END ADD *** 02/18/05 ****************************************
    C*
     C                   clear                   xxdate
     C                   clear                   xxdat8
     C                   clear                   xxtim4
     C                   move      *off          xxtrfg
     C                   clear                   xxrcnt
     C                   clear                   x
     C                   clear                   xxdip
    C*
    C*   ENTRY PARAMETERS (ALL ALPHANUMERIC)
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqjst
     C                   parm                    qqtset
     C                   parm                    qqotdt
     C                   parm                    qqottm
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C                   move      qqco#         kkco#
     C                   move      qqjst         kkjst
     C                   move      qqotdt        xxdate
     C                   move      qqottm        kkottm
     C                   move      qqtset        kktset
     C                   eval      srdti = xxdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkotdt
    C*
     C                   time                    dsdttm
    C*
    C* CLEAR HOLD FIELDS
    C*
     C                   clear                   hlco#
     C                   clear                   hlsub#
     C                   clear                   hlfnd
     C                   clear                   hlemp#
     C                   clear                   hldiv
     C                   clear                   hlfein
     C                   clear                   hlmod
     C                   clear                   hlfyr
     C                   clear                   hlcase
     C*
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   call      'RTVCLTID'
     C                   parm      *blanks       ##CLT#            3
     C
     C                   if        ##clt# = '135'
     C                   seton                                        71
     C                   else
     C                   setoff                                       71
     C                   endif
     C*** END ADD *** 07/18/06 *********************************************
     C*** Beg Add *** 11/16/16 *********************************************
      /Free
          If ##clt# = '025';
             If Not %Open(trpnpimst);
                Open trpnpimst;
             EndIf;
          EndIf;
      /End-Free
     C*** End Add *** 11/16/16 *********************************************
    C*
     C     eninit        endsr
    C*----------------------------------------------------------------
    C*  SRXHDR - HEADER RECORD
    C*----------------------------------------------------------------
    C*
     C     srxhdr        begsr
    C*
     C                   clear                   header
     C                   clear                   f1data
    C*
     C                   move      'HD1'         hequal
     C                   movel     exsein        hesid
     C                   move      exszp         hespcd
     C                   movel     exrein        herid
     C                   move      exrzp         herpcd
    C************         MOVE MFLTDT    HETSDT
     C                   eval      srdti = mfltdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         hetsdt
     C                   move      mflttm        hetstm
    C************         MOVE LGOTDT    HEOTDT
     C                   eval      srdti = lgotdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         heotdt
     C                   move      lgottm        heottm
     C                   if        mftstf = 'Y'
     C                   move      'T'           hetpin
     C                   else
     C                   move      'P'           hetpin
     C                   endif
     C                   move      '837'         hetset
     C                   movel     '1'           herls#
     C                   move      lgjst         heacde
    C************         MOVELMFRFDT    HEREPP
     C                   eval      srdti = mfrfdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         herepp
    C************         MOVE MFRTDT    HEREPP
     C                   eval      srdti = mfrtdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdat8
     C                   move      xxdat8        herepp
     C                   if        lgjst = 'OH'
     C                   move      '00'          hestyp
     C                   endif
    C*
     C                   movel     header        f1data
     C                   write     pf101
    C*
     C     enxhdr        endsr
    C*----------------------------------------------------------------
    C*  SRXINS - INSURER RECORD
    C*----------------------------------------------------------------
     C     srxins        begsr
     C                   clear                   insur
     C                   clear                   f1data
    C*
     C                   move      '05'          inqual
     C                   movel     lginnm        inname
     C                   move      lginzp        inpcod
     C                   movel     lginfe        infein
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lginzp = *zeros
     C                   clear                   inpcod
     C                   endif
     C                   if        lginfe = *zeros
     C                   clear                   infein
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     insur         f1data
    C*** BEGIN ADD *** 06/25/08 **************************************
     C                   if        inname<>' '
    C*** END   ADD *** 06/25/08 **************************************
     C                   write     pf101
    C*** BEGIN ADD *** 06/25/08 **************************************
     c                   endif
    C*** END   ADD *** 06/25/08 **************************************
     C     enxins        endsr
    C*----------------------------------------------------------------
    C*  SRXCLA - CLAIM ADMINISTRTOR RECORD
    C*----------------------------------------------------------------
     C     srxcla        begsr
     C                   clear                   admin
     C                   clear                   f1data
    C*
     C                   movel     '06'          adqual
     C                   movel     lgcanm        adname
     C                   movel     lgcazp        adpcod
     C                   movel     lgcafe        adfein
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgcazp = *zeros
     C                   clear                   adpcod
     C                   endif
     C                   if        lgcafe = *zeros
     C                   clear                   adfein
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     admin         f1data
    C*** BEGIN ADD *** 06/25/08 **************************************
     C                   if        adname<>' '
    C*** END   ADD *** 06/25/08 **************************************
     C                   write     pf101
    C*** BEGIN ADD *** 06/25/08 **************************************
     C                   endif
    C*** END   ADD *** 06/25/08 **************************************
     C     enxcla        endsr
    C*----------------------------------------------------------------
    C*  SRXMCO - MANAGED CARE ORGANIZATION RECORD
    C*----------------------------------------------------------------
     C     srxmco        begsr
     C                   clear                   mco
     C                   clear                   f1data
    C*
     C                   move      '07'          mcqual
     C                   movel     lgmcnm        mcname
     C                   movel     lgmca1        mcpadr
     C                   movel     lgmca2        mcsadr
     C                   movel     lgmcct        mccity
     C                   movel     lgmcst        mcstat
     C                   movel     lgmczp        mcpcod
     C                   movel     lgmccr        mcctry
     C                   movel     lgmcfe        mcfein
     C                   movel     lgmcid        mcanum
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgmczp = *zeros
     C                   clear                   mcpcod
     C                   endif
     C                   if        lgmcfe = *zeros
     C                   clear                   mcfein
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     mco           f1data
     C                   write     pf101
     C     enxmco        endsr
    C*----------------------------------------------------------------
    C*  SRXEMP - EMPLOYER RECORD
    C*----------------------------------------------------------------
     C     srxemp        begsr
     C                   clear                   emplyr
     C                   clear                   f1data
    C*
     C                   move      '08'          emqual
     C                   movel     lgemnm        emname
     C                   movel     lgema1        empadr
     C                   movel     lgema2        emsadr
     C                   movel     lgemct        emcity
     C                   movel     lgemst        emstat
     C                   movel     lgemzp        empcod
     C                   movel     lgemfe        emfein
     C                   movel(p)  lgemar        xxphn#
     C                   move      lgemph        xxphn#
     C                   movel     xxphn#        emphn#
     C                   clear                   emsfin
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgemzp = *zeros
     C                   clear                   empcod
     C                   endif
     C                   if        lgemfe = *zeros
     C                   clear                   emfein
     C                   endif
     C                   if        lgemph = *zeros
     C                   clear                   emphn#
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN ADD *** 02/18/05 **************************************
     C                   movel(p)  lgerct        emcnty
    C*** END   ADD *** 02/18/05 **************************************
    C*
     C                   movel     emplyr        f1data
     C                   write     pf101
     C     enxemp        endsr
    C*----------------------------------------------------------------
    C*  SRXPRV - PROVIDER RECORD
    C*----------------------------------------------------------------
     C     srxprv        begsr
    C*
     C                   clear                   prov
     C                   clear                   f1data
    C*
     C                   move      '10'          prqual
     C                   movel     lgbpfe        prfein
     C                   movel     lgbpln        prname
     C                   movel     lgptp#        prbwci
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgbpfe = *zeros
     C                   clear                   prfein
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     prov          f1data
     C                   write     pf101
    C*
     C     enxprv        endsr
    C*----------------------------------------------------------------
    C*  SRXCLM - CLAIM RECORD
    C*----------------------------------------------------------------
     C     srxclm        begsr
    C*
     C                   clear                   clm837
     C                   clear                   f1data
    C*
     C                   move      '30'          clqual
     C                   movel     lgss#         clessn
     C                   movel     lgtype        cletyp
     C                   movel     lgclln        clelsn
     C                   movel     lgclfn        clefsn
     C                   movel     lgclmi        clemid
     C                   movel     lgclsx        clnsfx
     C                   movel     lgcla1        clpadr
     C                   movel     lgcla2        clsadr
     C                   movel     lgclct        clcity
     C                   movel     lgclst        clstat
     C                   movel     lgclzp        clpcod
     C                   movel     lgclvs        clvisa
     C                   movel     lgclgc        clgrcd
    C*************        MOVELLGDTOB    CLEDOB
     C                   eval      srdti = lgdtob
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cledob
     C                   movel     lgsex         clgend
     C                   movel     lgmsts        clmsts
    C*************        MOVELLGACDT    CLDTOI
     C                   eval      srdti = lgacdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cldtoi
     C                   movel     lgactm        xxtim4
     C                   move      xxtim4        cldtoi
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        xxtim4 > 2400 or xxtim4 < 0001
     C                   move      '0800'        cldtoi
     C                   endif
     C*** END ADD *** 07/18/06 *********************************************
     C                   movel     lgclty        clctyp
     C                   movel     lgaclt        clacty
     C                   movel     lgscl#        clagc#
     C                   movel     lgcac#        clclm#
     C                   if        eobtyp = 'H'
     C                   movel     eumrc#        clmrc#
     C                   endif
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgclzp = *zeros
     C                   clear                   clpcod
     C                   endif
     C                   if        lgdtob = *zeros
     C                   clear                   cledob
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN ADD *** 02/18/05 **************************************
     C                   movel(p)  lgecty        clemct
    C*** END   ADD *** 02/18/05 **************************************
    C*
     C                   movel     clm837        f1data
     C                   write     pf101
    C*
    C*
     C     enxclm        endsr
    C*----------------------------------------------------------------
    C*  SRXBIL - BILL RECORD
    C*----------------------------------------------------------------
     C     srxbil        begsr
    C*
     C                   clear                   f1data
     C                   clear                   bil837
    C*
    C*           GENERAL EOMB INFORMATION
    C*
     C                   move      '40'          biqual
     C                   if        *in10 = *on
     C                   movel     ehacct        bipact
     C                   endif
     C                   if        *in11 = *on
     C                   movel     euacct        bipact
     C                   endif
     C                   if        *in12 = *on
     C                   movel     epacct        bipact
     C                   endif
     C                   if        *in13 = *on
     C                   movel     edacct        bipact
     C                   endif
     C***** Beg Del ***** 09/10/14 ************************************
    C*** BEGIN ADD *** 01/24/01 **************************************
     C*****              if        bipact = *blanks
     C*****              movel     lgss#         bipact
     C*****              endif
    C*** BEG ADD *** 07/08/06 ****************************************
     C*****              movel     *blanks       bipact
    C*** END ADD *** 07/08/06 ****************************************
    C*** END ADD ***** 01/24/01 **************************************
     C***** End Del ***** 09/10/14 ************************************
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   biinap
     C                   move      *zeros        biinap
    C*** END CHG ***** 05/13/06 **************************************
     C                   move      lg1pd         biinap
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   bichrg
     C                   move      *zeros        bichrg
    C*** END CHG ***** 05/13/06 **************************************
     C                   move      lgchrg        bichrg
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   bipaid
     C                   move      *zeros        bipaid
    C*** END CHG ***** 05/13/06 **************************************
     C*** Beg Add *** 02/20/12 **********************************************
     C                   eval      xxpaid = lgpaid
     C                   exsr      srfmtLg
     C*** End Add *** 02/20/12 **********************************************
     C*** Beg Del *** 02/20/12 **********************************************
     C*****              move      lgpaid        bipaid
     C*** End Del *** 02/20/12 **********************************************
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   bitncc
     C                   move      *zeros        bitncc
    C*** END CHG ***** 05/13/06 **************************************
     C                   move      lgnocv        bitncc
     C*
     C                   if        lgrcn = *zeros
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        lgcsdt= *zeros
     C                   eval      lgcsdt = lgcrdt
     C                   endif
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   eval      srdti = lgcsdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bibidt
     C                   eval      srdti = lgcrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bircvd
     C*
     C                   else
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        lgrsdt= *zeros
     C                   eval      lgrsdt = lgrrdt
     C                   endif
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   eval      srdti = lgrsdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bibidt
     C                   eval      srdti = lgrrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bircvd
     C*
     C                   endif
     C*
     C                   eval      srdti = lgapdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bidpap
     C                   eval      srdti = lgpddt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bipddt
     C                   movel     lgbltc        bibtyp
     C                   movel     lgmtc         bisbrc
     C                   if        *in11 = *on
     C                   movel     eublty        bifacc
     C                   endif
     C                   movel     lghscd        bihosc
     C                   movel     lgblfc        bifmtc
     C                   movel     lgfreq        bifreq
     C                   movel     lgbene        bibenf
     C                   movel     lgohpi        bipmri
     C                   movel     lgpaut        bipaut
    C*
    C*** BEGIN ADD *** 08/07/00 **************************************
    C*** BEGIN DLT *** 01/29/01 **************************************
    C********** LG1PD     IFEQ *ZEROS
    C**********           MOVE *BLANKS   BIINAP
    C**********           END
    C********** LGCHRG    IFEQ *ZEROS
    C**********           MOVE *BLANKS   BICHRG
    C**********           END
    C********** LGPAID    IFEQ *ZEROS
    C**********           MOVE *BLANKS   BIPAID
    C**********           END
    C********** LGNOCV    IFEQ *ZEROS
    C**********           MOVE *BLANKS   BITNCC
    C**********           END
    C*** END DLT ***** 01/29/01 **************************************
    C*
     C                   if        lgrcn = *zeros
    C*
     C                   if        lgcsdt = *zeros
     C                   clear                   bibidt
     C                   endif
     C                   if        lgcrdt = *zeros
     C                   clear                   bircvd
     C                   endif
    C*
     C                   else
    C*
     C                   if        lgrsdt = *zeros
     C                   clear                   bibidt
     C                   endif
     C                   if        lgrrdt = *zeros
     C                   clear                   bircvd
     C                   endif
    C*
     C                   endif
    C*
     C                   if        lgapdt = *zeros
     C                   clear                   bidpap
     C                   endif
     C                   if        lgpddt = *zeros
     C                   clear                   bipddt
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
    C***11                MOVELEUADDT    BIADDT
    C***11                MOVE EUADHR    BIADDT
    C***11                MOVELEUDCDT    BIDSDT
    C***11                MOVE EUDCHR    BIDSDT
     C                   if        *in11 = *on
    C*
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        euaddt <> *zeros
    C*** END ADD ***** 08/07/00 **************************************
     C                   eval      srdti = euaddt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         biaddt
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   endif
     C                   if        euadhr <> *zeros
    C*** END ADD ***** 08/07/00 **************************************
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   movel     euadhr        xxadhr            4
     C                   move      '01'          xxadhr
     C                   move      xxadhr        biaddt
     C*** END ADD *** 07/18/06 *********************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        euaddt = *zeros
     C                             and euadhr = *zeros
     C                   clear                   biaddt
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        bifacc = '11' or bifacc = '12' or
     C                             bifacc = '13' or bifacc = '17'
     C                   if        euadhr = *zeros
     C                   move      '0101'        biaddt
     C                   endif
     C                   endif
     C*** END ADD *** 07/18/06 *********************************************
    C*
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        eudcdt <> *zeros
    C*** END ADD ***** 08/07/00 **************************************
     C                   eval      srdti = eudcdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bidsdt
    C*** BEGIN ADD *** 08/07/00 **************************************
    C*** BEG DEL *** 07/05/01 ****************************************
    C**********           END
    C*** END DEL *** 07/05/01 ****************************************
     C                   if        eudchr <> *zeros
    C*** END ADD ***** 08/07/00 **************************************
     C*** Beg Chg ***** 09/01/14 **************************************
     C*****              move      eudchr        bidsdt
     C                   movel     EUdChr        xxdChr
     C                   move      '01'          xxdChr
     C                   move      xxdChr        bidsdt
     C*** End Chg ***** 09/01/14 **************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   endif
    C*** BEG ADD *** 07/05/01 ****************************************
     C                   endif
    C*** END ADD *** 07/05/01 ****************************************
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        eudcdt = *zeros
    C*** BEG DEL *** 07/05/01 ****************************************
    C********** EUDCHR    ANDEQ*ZEROS
    C*** END DEL *** 07/05/01 ****************************************
     C                   clear                   bidsdt
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   endif
    C*
     C                   movel     lgdtrt        birtyp
    C**********           MOVELLGFRDT    BISDRG
    C**********           MOVE LGTODT    BISDRG
     C                   eval      srdti = lgfrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bisdrg
     C                   eval      srdti = lgtodt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel(p)  srdto         xx8a
     C                   cat       xx8a:0        bisdrg
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgfrdt = *zeros
     C                             or lgtodt = *zeros
     C                   clear                   bisdrg
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel(p)  lgeob#        xx12a
     C                   move      lgrcn         xx12a
     C                   movel     xx12a         biuid#
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   bibild
     C                   move      *zeros        bibild
    C*** END CHG ***** 05/13/06 **************************************
     C                   move      lgchrg        bibild
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgchrg = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   BIBILD
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C                   if        *in11 = *on
     C                   movel     eucoc1        biccd1
     C                   movel     eucoc2        biccd2
     C                   movel     eucoc3        biccd3
     C                   movel     eucoc4        biccd4
     C                   movel     eucoc5        biccd5
     C                   if        eupcm = '5'
     C                   move      'BP'          bipqal
     C                   endif
     C                   if        eupcm = '9'
    C*** BEG CHG *** 04/16/15 ****************************************
B01  C                   if        Euicdind <> 'Y'
     C                   move      'BR'          bipqal
     C                   endif
     C*****              move      'BR'          bipqal
    C*** END CHG *** 04/16/15 ****************************************
     C                   endif
    C*** BEG CHG ***** 01/19/07 **************************************
     C*******            if        qqjst = 'TX'
     C                   if        qqjst = 'TX' or qqjst = 'CA'
    C*** END CHG ***** 01/19/07 **************************************
     C                   if        eupcm = '5'
     C                   move      'BO'          bipqal
     C                   endif
     C                   if        eupcm = '9'
    C*** BEG CHG *** 04/16/15 ****************************************
     C                   if        Euicdind <> 'Y'
     C                   move      'BQ'          bipqal
     C                   endif
     C*****              move      'BQ'          bipqal
    C*** END CHG *** 04/16/15 ****************************************
     C                   endif
     C                   endif
    C*** END ADD ***** 02/18/05 **************************************
     C                   movel     euprc1        bipcpt
    C***********          MOVELEUPRD1    BIPPDT
     C                   eval      srdti = euprd1
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bippdt
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        bipcpt = *blanks
     C                   movel     '99.99'       bipcpt
     C*** BEG ADD *** 12/04/06 *********************************************
     C                   move      'BQ'          bipqal
     C*** END ADD *** 12/04/06 *********************************************
     C                   endif
     C*** END ADD *** 07/18/06 *********************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        euprd1 = *zeros
     C                   clear                   bippdt
     C*** BEG ADD *** 02/18/05 *********************************************
     C                   if        qqjst = 'TX' and bipcpt <> *blanks
     C                   eval      srdti = eofrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         bippdt
     C                   endif
     C*** END ADD *** 02/18/05 *********************************************
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C                   if        eupcm = '5'
     C                   if        euprc2 <> *blanks
     C                   move      'BQ'          bipc1q
     C                   endif
     C                   if        euprc3 <> *blanks
     C                   move      'BQ'          bipc2q
     C                   endif
     C                   if        euprc4 <> *blanks
     C                   move      'BQ'          bipc3q
     C                   endif
     C                   if        euprc5 <> *blanks
     C                   move      'BQ'          bipc4q
     C                   endif
     C                   if        euprc6 <> *blanks
     C                   move      'BQ'          bipc5q
     C                   endif
     C                   endif
     C                   if        eupcm = '9'
    C*** BEG CHG *** 04/16/15 ****************************************
     C                   if        Euicdind <> 'Y'
    C*** END CHG *** 04/16/15 ****************************************
     C                   if        euprc2 <> *blanks
     C                   move      'BO'          bipc1q
     C                   endif
     C                   if        euprc3 <> *blanks
     C                   move      'BO'          bipc2q
     C                   endif
     C                   if        euprc4 <> *blanks
     C                   move      'BO'          bipc3q
     C                   endif
     C                   if        euprc5 <> *blanks
     C                   move      'BO'          bipc4q
     C                   endif
     C                   if        euprc6 <> *blanks
     C                   move      'BO'          bipc5q
     C                   endif
    C*** BEG CHG *** 04/16/15 ****************************************
     C                   endif
    C*** END CHG *** 04/16/15 ****************************************
     C                   endif
     C                   movel     euprc2        bipcd1
     C                   movel     euprc3        bipcd2
     C                   movel     euprc4        bipcd3
     C                   movel     euprc5        bipcd4
     C                   movel     euprc6        bipcd5
     C                   endif
     C                   if        *in10 = *on
    C*** BEGIN DLT *** 05/24/00 **************************************
    C******************** MOVELEHDIA1    BIPDIA
    C******************** MOVELEHDIA2    BIDIA1
    C******************** MOVELEHDIA3    BIDIA2
    C******************** MOVELEHDIA4    BIDIA3
    C******************** MOVEL*BLANKS   BIDIA4
    C*** END DLT ***** 05/24/00 **************************************
    C*** BEGIN ADD *** 05/24/00 **************************************
     C                   clear                   bipdia
    C*** Beg Add ***  04/16/15 **************************************
     C                   if        EHIcdInd <> 'Y'
    C*** End Add ***  04/16/15 **************************************
     C                   movel     ehdia1        bidia1
     C                   movel     ehdia2        bidia2
     C                   movel     ehdia3        bidia3
     C                   movel     ehdia4        bidia4
    C*** Beg Add ***  04/16/15 **************************************
     C                   EndIf
    C*** End Add ***  04/16/15 **************************************
    C*** END ADD ***** 05/24/00 **************************************
     C                   endif
     C                   if        *in11 = *on
    C*** BEG CHG *** 04/16/15 ****************************************
     C                   if        Euicdind <> 'Y'
    C*** END CHG *** 04/16/15 ****************************************
     C                   movel     eudia1        bipdia
     C                   movel     euadia        biadia
     C                   movel     eudia2        bidia1
     C                   movel     eudia3        bidia2
     C                   movel     eudia4        bidia3
     C                   movel     eudia5        bidia4
     C                   movel     eudia6        bidia5
     C                   movel     eudia7        bidia6
     C                   movel     eudia8        bidia7
     C                   movel     eudia9        bidia8
     C                   clear                   bidia9
    C*** BEG CHG *** 04/16/15 ****************************************
     C                   endif
    C*** END CHG *** 04/16/15 ****************************************
     C                   endif
     C                   movel     lgohiw        biiwri
     C                   clear                   biposc
     C                   move      '99'          biposc
     C                   clear                   bitrk#
     C                   clear                   bipbl#
    C*** BEG ADD ***** 02/18/05 **************************************
     C                   if        eontwk <> *blanks
    C*** BEG ADD ***** 12/16/10 **************************************
     C     nwkey         chain     pnw01                              99
     C                   if        *in99 = *off  and nwfl02 <> *blanks
     C                   movel(p)  nwfl02        bipagr
     C                   else
     C                   movel(p)  'Y'           bipagr
     C                   endif
    C*** END ADD ***** 12/16/10 **************************************
     C                   else
     C                   movel(p)  'N'           bipagr
     C                   endif
     C                   movel(p)  lgpsig        bipsig
     C*
     C                   if        qqjst = 'TX'
     C*
     C                   clear                   biposc
     C                   move      lgposc        biposc
     C*** BEG ADD *** 01/25/07 *********************************************
     C                   if        biposc < '11'
     C                   move      '12'          biposc
     C                   endif
     C*** END ADD *** 01/25/07 *********************************************
     C*
     C                   endif
    C*** BEG ADD ***** 05/24/00 **************************************
     C                   if        *in13 = *on
    C*** Beg Add ***  04/16/15 **************************************
     C                             and EDIcdInd <> 'Y'
    C*** End Add ***  04/16/15 **************************************
     C                   movel     eddia1        bipdia
     C                   endif
    C*** END ADD ***** 02/18/05 **************************************
    C*** BEG ADD ***** 11/13/05 **************************************
     C*** Begin Chg *** 08/06/13 **************************************
     C**********         if        *in12 = *on
     C**********         movel     lgbpid        bipbl#
     C**********         endif
     C                   movel     lgbpid        bipbl#
     C*** End   Chg *** 08/06/13 **************************************
    C*** END ADD ***** 11/13/05 **************************************
    C*
    C*** BEG ADD *** 01/19/07 ****************************************
     C                   if        qqjst = 'CA'
     C                   if        *in13 = *on
    C*** Beg Add ***  04/16/15 **************************************
     C                             and EDIcdInd <> 'Y'
    C*** End Add ***  04/16/15 **************************************
     C                   movel     eddia1        bidia1
     C                   endif
     C     euk1          chain     peu02                              99
     C                   if        *in99 = *off
     C                   movel(p)  e1ctcd        bictcd
     C                   endif
     C                   movel(p)  eobtyc        bibtyp
     C*** Begin Del *** 08/06/13 **************************************
     C**********         movel(p)  eoblid        bipbl#
     C*** End   Del *** 08/06/13 **************************************
     C*
     C                   if        *in11 = *on
     C                   move      e1drg#        bidrg
     C                   endif
     C*
     C                   endif
     C*** BEG ADD *** 07/03/08 ****************************************
     C                   if        qqjst = 'TX' and *in11 = *on
     C     euk1          chain     peu02                              99
     C                   if        *in99 = *off
     C                   eval      xxdrg  = %editc(e1drg# : 'D')
     C                   movel     xxdrg         bidrg
     C                   eval      bidrg = %trim(bidrg)
     C                   endif                                                  *in99 = *off
     C                   endif                                                  st = 'TX' btyp = 'H'
     C*** END ADD *** 07/03/08 ****************************************
    C*** END ADD *** 01/19/07 ****************************************
    C*
    C*** BEG ADD *** 01/29/09 ****************************************
     C                   if        *in11 = *on and qqjst = 'TX'
     C                             and e1drg# <> 0
     C                   movel     '01'          bictcd
     C                   endif
    C*** END ADD *** 01/29/09 ****************************************
    C*** BEG ADD *** 04/16/15 ****************************************
      /Free
       //  Hospital Bills - ICD10

       If *in11 = *on;

          If Euicdind =  'Y';

      //*** Beg Add *** 06/11/15 ****************************************
            //  ICD-10 Indicator
            BIiCDInd = 'Y';
      //*** End Add *** 06/11/15 ****************************************
            //  Diag Codes

            BIDIA110 = EUICD1001;
            BIDIA210 = EUICD1002;
            BIDIA310 = EUICD1003;
            BIDIA410 = EUICD1004;
            BIDIA510 = EUICD1005;
            BIDIA610 = EUICD1006;
            BIDIA710 = EUICD1007;
            BIDIA810 = EUICD1008;
            BIDIA910 = EUICD1009;
            //*** Beg Add *** 10/21/15 ************************************
            BIDIA010 = EUICD1010;
            //*** End Add *** 10/21/15 ************************************

            // Admitting Diag Code

            BIADIA10 = EUADIA1001;

            // Proc codes

            BIPRC110 = EUPRC1001;
            BIPRC210 = EUPRC1002;
            BIPRC310 = EUPRC1003;
            BIPRC410 = EUPRC1004;
            //*** Beg Add *** 10/21/15 ************************************
            BIPRC510 = EUPRC1005;
            //*** End Add *** 10/21/15 ************************************

            If EUPCM = '9';

              If EUPRC1001 <> *Blanks;
                 BIPqal = 'BO';
              EndIf;
              If EUPRC1002 <> *Blanks;
                 BIPc1q = 'BO';
              EndIf;
              If EUPRC1003 <> *Blanks;
                 BIPc2q = 'BO';
              EndIf;
              If EUPRC1004 <> *Blanks;
                 BIPc3q = 'BO';
              EndIf;
              If EUPRC1005 <> *Blanks;
                 BIPc4q = 'BO';
              EndIf;
              If EUPRC1006 <> *Blanks;
                 BIPc5q = 'BO';
              EndIf;

            EndIf;

          EndIf;

       EndIf;

       //  Physician Bills - ICD10

       If *in10 = *on and EHIcdInd = 'Y';
      //*** Beg Add *** 06/11/15 ****************************************
            //  ICD-10 Indicator
            BIiCDInd = 'Y';
      //*** End Add *** 06/11/15 ****************************************

            //*** Beg Chg *** 10/21/15 ************************************
            //***** BIDIA110 = EHICD1001;
            //***** BIDIA210 = EHICD1002;
            //***** BIDIA310 = EHICD1003;
            //***** BIDIA410 = EHICD1004;
            BIDIA210 = EHICD1001;
            BIDIA310 = EHICD1002;
            BIDIA410 = EHICD1003;
            BIDIA510 = EHICD1004;
            //*** End Chg *** 10/21/15 ************************************

       EndIf;

       //  Dental Bills - ICD10

       If *in13 = *on and EDIcdInd = 'Y';
      //*** Beg Add *** 06/11/15 ****************************************
            //  ICD-10 Indicator
            BIiCDInd = 'Y';
      //*** End Add *** 06/11/15 ****************************************

            //*** Beg Chg *** 10/21/15 ************************************
            //***** BIDIA110 = EDICD1001;
            //***** BIDIA210 = EDICD1002;
            //***** BIDIA310 = EDICD1003;
            //***** BIDIA410 = EDICD1004;
            BIDIA210 = EDICD1001;
            BIDIA310 = EDICD1002;
            BIDIA410 = EDICD1003;
            BIDIA510 = EDICD1004;
            //*** End Chg *** 10/21/15 ************************************

       EndIf;
      /End-Free
    C*** END ADD *** 04/16/15 ****************************************

     C                   movel     bil837        f1data

     C                   write     pf101
    C*
     C     enxbil        endsr
    C*----------------------------------------------------------------
    C*  SRXHOS - HOSPITAL RECORD
    C*----------------------------------------------------------------
     C     srxhos        begsr
    C*
     C                   clear                   f1data
     C                   clear                   hosp
    C*
     C                   movel     '41'          hoqual
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   if        euadty <> *blanks
    C*** END ADD *** 02/18/05 ****************************************
     C                   movel     euadty        hoatyp
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   else
     C                   movel     '9'           hoatyp
     C                   endif
    C*** END ADD *** 02/18/05 ****************************************
     C                   movel     euadsc        hoasrc
     C                   movel     eupats        hopsts
     C                   clear                   hodayc
     C                   move      eudycv        hodayc
     C                   clear                   hodayn
     C                   move      eunocv        hodayn
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        eudycv = *zeros
     C                   clear                   hodayc
     C                   endif
     C                   if        eunocv = *zeros
     C                   clear                   hodayn
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     hosp          f1data
     C                   write     pf101
    C*
     C     enxhos        endsr
    C*----------------------------------------------------------------
    C*  SRXPHY - PHYSICIAN/BILL LEVEL
    C*----------------------------------------------------------------
     C     srxphy        begsr
    C*
     C*** BEG DEL *** 02/18/05 *********************************************
     C****************** clear                   #1data
     C****************** clear                   phy837
    C******************
     C****************** move      '42'          phqual
     C****************** move      '85'          phrole
     C****************** move      '1'           phenty
     C*** END DEL *** 02/18/05 *********************************************
     C                   select
     C*
    C*    - Billing provider
    C*
     C                   when      phrole = '85'
     C*
     C                   movel     lgbpln        phlgnm
     C                   movel     lgbpfn        phfsn
     C                   movel     lgbpmi        phmid
     C                   movel     lgbpsx        phlsfx
     C                   movel     lgpa#         phphid
     C                   movel     lgbpfe        phfein
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgbpfe = *zeros
     C                   clear                   phfein
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C                   movel     lgbpa1        phpadr
     C                   movel     lgbpa2        phsadr
     C                   movel     lgbpct        phcity
     C                   movel     lgbpst        phstat
     C                   movel     lgbpzp        phpcod
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        lgbpzp = *zeros
     C                   clear                   phpcod
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C                   movel     lgbpcr        phccod
    C*
     C                   if        lgbpsc <> *blanks
    C*
     C                   movel(p)  'KYSC'        kktbid
     C     giky          chain     pgi01                              99
     C                   if        *in99 = *on
     C                   movel(p)  'ZZSC'        kktbid
     C                   endif
    C*
     C                   movel(p)  lgbpsc        kkkey
     C     gtky          chain     pgt01                              99
     C                   if        *in99 = *off
     C                   movel     gtcf01        phpspc
     C                   endif
    C*
     C                   endif
    C*
     C                   if        *in10 = *on
     C                   movel     ehalic        phalic
     C                   endif
     C                   if        *in11 = *on
     C                   movel     eualic        phalic
     C                   movel     eumcar        phmed#
     C                   endif
     C                   movel     lgnat#        phnpid
     C                   movel     lgslic        phspli
     C                   if        *in10 = *on
     C                   movel     ehlic#        phstli
     C                   endif
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   if        eoast = 'TX'
     C                   if        *in10 = *on
     C                   movel(p)  ehflic        phstli
     C                   endif
     C                   if        *in11 = *on
     C                   movel(p)  euflic        phstli
     C                   endif
     C*** begin del *** 02/25/09 *******************************************
     C**********         if        *in12 = *on
     C**********         movel(p)  epplic        phspli
     C**********         endif
     C*** end   del *** 02/25/09 *******************************************
     C                   if        *in13 = *on
     C                   movel(p)  edflic        phstli
     C                   endif
     C                   endif
    C*** END ADD *** 02/18/05 ****************************************
    C*** BEG DEL *** 05/13/06 ****************************************
     C****************** movel     lggtki        phgati
    C*** END DEL *** 05/13/06 ****************************************
    C**************       MOVELLGGTK#    PHIND#
     C*
    C*** BEG ADD *** 01/19/07 ****************************************
     C                   if        EOAST = 'CA'
     C                   MOVEL(P)  EOBLIC        PHSTLI
     C                   endif
    C*** BEG END *** 01/19/07 ****************************************
     C*
    C*** BEG ADD *** 02/18/05 ****************************************
     C*
    C*    - Referring provider
    C*
     C                   when      phrole = 'DN'
     C*
     C                   movel     lgfpln        phlgnm
     C                   movel     lgfpfn        phfsn
     C                   movel     lgfpmi        phmid
     C                   movel     lgfpsx        phlsfx
     C                   movel     lgfpfe        phfein
    C*
     C                   if        lgfpfe = *zeros
     C                   clear                   phfein
     C                   endif
    C*
     C                   movel     lgflic        phstli
    C*** BEG DEL *** 05/13/06 ****************************************
     C****************** movel     lggtki        phgati
    C*** END DEL *** 05/13/06 ****************************************
     C*** BEG ADD *** 09/12/06 *********************************************
     C*
     C*** BEG ADD *** 07/03/08 ****************************************
     C     dpky          chain     pdp01                              99
     C                   if        *in99 = *off
     C     ddk1          chain     pdp02                              99
     C                   if        *in99 = *off
     C                   movel     d1rnpi        phnpid
     C                   endif
     C                   endif
     C*** END ADD *** 07/03/08 ****************************************
    C*** BEG ADD *** 05/18/09 ****************************************
     C                   if        *in12 = *on and eoast = 'TX'
     C     euk1          chain     pep02                              99
     C                   if        e2rfnp <> ' '
     C                   movel(p)  e2rfnp        phnpid
     C                   endif                                                  e2rfnp <> ' '
     C                   endif                                                  eoast = 'TX' & *in12
    C*** END ADD *** 05/18/09 ****************************************
    C*** Beg Del *** 04/03/13 ****************************************
     C********           if        phnpid = *blanks
     C********           movel     'XX9999999'   phnpid
     C********           endif
    C*** End Del *** 04/03/13 ****************************************
     C*
     C*** END ADD *** 09/12/06 *********************************************
    C*** BEG ADD *** 12/13/08 ****************************************
     C                   if        eoast = 'TX'
     C*
     C                   if        *in11 = *on
    C*** BEG CHG *** 02/12/09 ****************************************
     C****************   movel(p)  eurlic        phstli
     C     euk1          chain     peu02                              99
     C                   if        *in99 = *off
     C                   movel(p)  e1rlic        phstli
    C*** Beg add *** 04/03/13 ****************************************
     C                   movel(p)  e1anpi        phnpid
    C*** End add *** 04/03/13 ****************************************
     C                   endif
    C*** END CHG *** 02/12/09 ****************************************
     C                   endif
     C*
     C*** begin del *** 02/25/09 *******************************************
     C**********         if        *in12 = *on
     C**********         movel(p)  epplic        phstli
     C**********         endif
     C*** end   del *** 02/25/09 *******************************************
     C*** begin add *** 02/25/09 *******************************************
     C                   if        *in12 = *on and eoast = 'TX'
     C                   movel(p)  *blanks       phstli
     C                   endif
     C*** end   add *** 02/25/09 *******************************************
     C*
     C                   endif
    C*** END ADD *** 12/13/08 ****************************************
    C*** BEG ADD *** 11/13/05 ****************************************
    C*** BEG ADD *** 05/13/06 ****************************************
     C*
    C*    - Rendering provider
    C*
     C                   when      phrole = '82'
     C*
     C                   movel     lgrpln        phlgnm
     C                   movel     lgrpfn        phfsn
     C                   movel     lgrpmi        phmid
     C                   movel     lgrpsx        phlsfx
     C                   movel     lgrpfe        phfein
    C*
     C                   if        lgfpfe = *zeros
     C                   clear                   phfein
     C                   endif
    C*** END ADD *** 05/13/06 ****************************************
     C*** BEG ADD *** 05/28/08 ****************************************
     C*
     C* Rendering Provider State for TX Dental
     C*
     C                   if        eoast='TX' and eobtyp='D'
     C                   eval      phstat='TX'
     C*** BEG CHG *** 10/30/08 ***************************************
     C***********        movel(p)  eorlic        phstli
     C     ehky2         chain     peh02                              99
     C                   if        *in99 = *off
     C                   movel(p)  e2rnlc        phstli
     C                   endif
     C*** END CHG *** 10/30/08 ***************************************
     C                   endif
     C*** END ADD *** 05/28/08 ****************************************
    C*
    C*** BEG ADD *** 01/19/07 ****************************************
     C*
     C                   if        eoast = 'CA'
     C*
     C                   movel(p)  eornam        phlgnm
     C                   movel(p)  eospcd        phpspc
     C                   movel(p)  eoblic        phstli
     C                   movel     eozip         phpcod
     C*
     C                   endif
     C*
    C*** END ADD *** 01/19/07 ****************************************
    C*** BEG ADD *** 12/13/08 ****************************************
     C                   if        eoast = 'TX'
     C*
     C                   if        *in10 = *on
    C*** BEG CHG *** 02/12/09 ****************************************
     C***************    movel(p)  ehrnlc        phstli
     C     ehky2         chain     peh02                              99
     C                   if        *in99 = *off
     C                   movel(p)  e2rnlc        phstli
    C*** Beg Add *** 04/03/13 ****************************************
     C                   movel(p)  e2rnpi        phnpid
    C*** End Add *** 04/03/13 ****************************************
     C                   endif
    C*** END CHG *** 02/12/09 ****************************************
     C                   endif
     C*
    C*** BEG ADD *** 08/11/11 ****************************************
     C                   if        *in12 = *on
     C     euk1          chain     pep02                              99
     C                   if        *in99 = *off
     C                   eval      phnpid = epphar
     C                   endif                                                  if in99 = off
     C                   endif                                                  if in12 = on
    C*** END ADD *** 08/11/11 ****************************************
     C*
     C                   if        *in13 = *on
    C*** BEG CHG *** 02/12/09 ****************************************
     C****************   movel(p)  edrnlc        phstli
     C     edky2         chain     ped02                              99
     C                   if        *in99 = *off
     C                   movel(p)  e3rnlc        phstli
     C                   endif
    C*** END CHG *** 02/12/09 ****************************************
    C*** BEG ADD *** 08/11/11 ****************************************
     C     edky          chain     ped01                              99
     C                   if        *in99 = *off
     C                   eval      phnpid = edtnpi
     C                   endif                                                  in99 = off
    C*** END ADD *** 08/11/11 ****************************************
     C                   endif
     C*
     C                   endif
    C*** END ADD *** 12/13/08 ****************************************
     C*
    C*    - Facility
    C*
     C                   when      phrole = '61'
     C*
     C                   movel     lgfnam        phlgnm
    C*** BEG ADD *** 07/08/06 ****************************************
     C                   movel     lgbpa1        phpadr
     C                   movel     lgbpa2        phsadr
     C                   movel     lgbpct        phcity
     C                   movel     lgbpst        phstat
     C                   movel     lgbpzp        phpcod
     C                   movel     lgbpcr        phccod
    C*** END ADD *** 07/08/06 ****************************************
     C*
    C*** BEG ADD *** 01/19/07 ****************************************
     C                   if        lgjst = 'CA'
     C                   movel     eoffei        phfein
     C                   movel     eofnam        phlgnm
     C                   movel     eofzip        phpcod
     C                   endif
    C*** END ADD *** 01/19/07 ****************************************
     C*
     C                   endsl
    C*** END ADD *** 02/18/05 ****************************************
    C*
     C                   movel     phy837        f1data
     C                   write     pf101
    C*** BEG ADD *** 05/13/06 ****************************************
     C                   clear                   f1data
     C                   clear                   phy837
    C*** END ADD *** 05/13/06 ****************************************
    C*
     C     enxphy        endsr
    C*----------------------------------------------------------------
    C*  SRXHLT - HEALTH INFORMATION RECORD
    C*----------------------------------------------------------------
     C     srxhlt        begsr
    C*
     C                   clear                   hinfo
     C                   clear                   f1data
     C                   move      '43'          hiqual
     C                   movel     hinfo         f1data
`    C                   write     pf101
    C*
     C     enxhlt        endsr
    C*----------------------------------------------------------------
    C*  SRXTYE - CLAIM LEVEL EOB TYPE/BILL ADJ REASON CODES
    C*----------------------------------------------------------------
     C     srxtye        begsr
    C*
     C                   clear                   tyeob
     C                   clear                   f1data
    C*
     C                   move      '44'          tyqual
    C*
    C*  Only one group code can be sent.
    C*
     C                   if        (x = 1) or (lgjst = 'CA')
     C                   movel     lgbag1        tyajgc
     C                   endif
    C*
     C                   movel     lgbar1        tyajrc
     C*** BEG ADD *** 02/18/05 *********************************************
     C*
     C* Texas does not require bill level adjustments
     C*
     C                   if        lgjst <> 'TX'
    C*
     C* California specifics
    C*
     C*** BEG ADD *** 01/19/07 *********************************************
    C*
     C                   if        lgjst = 'CA'
     C                   eval      tyaja1 = %char(eochrg - eopaid)
     C                   movel     *all'0'       tyaju1
     C                   endif
    C*
     C*** END ADD *** 01/19/07 *********************************************
    C*
     C                   movel     tyeob         f1data
     C                   write     pf101
    C*
     C                   endif
     C*** END ADD *** 02/18/05 *********************************************
    C*
     C     enxtye        endsr
    C*----------------------------------------------------------------
    C*  SRXSV# - SERVICE NUMBER RECORD
    C*----------------------------------------------------------------
     C     srxsv#        begsr
    C*
     C                   clear                   serv#
     C                   clear                   f1data
    C*
     C                   move      '50'          sequal
     C                   clear                   selin#
     C                   if        *in10 = *on
     C                   move      dhline        selin#
     C                   endif
     C                   if        *in11 = *on
     C                   move      duline        selin#
     C                   endif
     C                   if        *in12 = *on
     C                   move      dpline        selin#
     C                   endif
     C                   if        *in13 = *on
     C                   move      ddline        selin#
     C                   endif
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        selin# = *zeros
     C                   clear                   selin#
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     serv#         f1data
     C                   write     pf101
    C*
     C     enxsb#        endsr
    C*----------------------------------------------------------------
    C*  SRXPSV - PROFESSIONAL SERVICE
    C*----------------------------------------------------------------
     C     srxpsv        begsr
    C*
     C                   clear                   pserv
     C                   clear                   f1data
    C*
     C                   move      '51'          psqual
     C                   if        dhbprc <> *blanks
    C********** DHRCFL    IFEQ 'C'
    C**********           MOVE 'ER'      PSSVQ1
    C**********           ENDIF                           DHRCFL = C
    C********** DHRCFL    IFEQ 'Y'
     C                   move      'HC'          pssvq1
    C**********           ENDIF                           DHRCFL = H
     C                   endif
     C                   movel     dhbprc        psprbc
    C*
     C                   if        dhprcd <> *blanks
    C********** DHRCFL    IFEQ 'C'
    C**********           MOVE 'ER'      PSSVQ2
    C**********           ENDIF                           DHRCFL = C
    C********** DHRCFL    IFEQ 'Y'
     C                   move      'HC'          pssvq2
    C**********           ENDIF                           DHRCFL = H
     C                   endif
     C                   movel     dhprcd        psproc
    C*
     C                   if        psprbc = *blanks
     C                   move      pssvq2        pssvq1
     C                   movel     psproc        psprbc
     C                   endif
    C*
     C*** BEGIN UND *** 08/10/11 *******************************************
     C*** BEGIN DEL *** 06/16/08 *******************************************
     C                   movel     dhmod1        pssmd1
     C                   movel     dhmod2        pssmd2
     C                   movel     dhmod3        pssmd3
     C                   movel     dhmod4        pssmd4
     C*** END   DEL *** 06/16/08 *******************************************
     C*** END   UND *** 08/10/11 *******************************************
     C                   movel     dhprds        pspdsc
    C*
     C                   clear                   xxdip
     C                   if        dhdxr1 = 'X'
     C                   movel     '1'           xxdip
     C                   endif
     C                   if        dhdxr2 = 'X'
     C                   cat       '2':0         xxdip
     C                   endif
     C                   if        dhdxr3 = 'X'
     C                   cat       '3':0         xxdip
     C                   endif
     C                   if        dhdxr4 = 'X'
     C                   cat       '4':0         xxdip
     C                   endif
    C*
     C                   movea     xxdip         dip
     C                   movel     xxdip1        psdip1
     C                   movel     xxdip2        psdip2
     C                   movel     xxdip3        psdip3
     C                   movel     xxdip4        psdip4
    C**********           MOVELLGDIA1    PSPDIA
    C*** BEG CHG *** 05/13/06 ****************************************
     C****************** clear                   pschrg
     C                   move      *zeros        pschrg
    C*** END CHG *** 05/13/06 ****************************************
     C                   move      dhchrg        pschrg
    C*** BEG CHG *** 02/18/05 ****************************************
    C******************** MOVE *BLANKS   PSUBAS
    C*** BEG CHG *** 09/08/11 ****************************************
      *************      if        dhbprc >= '00100'
      *************                and dhbprc <= '01999'
     C                   if        dhprcd >= '00100'
     C                             and dhprcd <= '01999'
     C                   move      'MJ'          psubas
    C*
     C                   else
      *************      if        dhbprc <> *blanks
     C                   move      'UN'          psubas
    C*
      *************      else
      *************      clear                   psubas
    C*
      *************      endif
    C*** END CHG *** 09/08/11 ****************************************
    C*
     C                   endif
    C*** END CHG *** 02/18/05 ****************************************
     C                   clear                   psuniq
    C*** BEG CHG *** 02/18/05 ****************************************
     C****************** move      dhunts        psuniq
    C******************** MOVELDHPOS     PSPSLC
     C                   z-add     dhunts        xxunts
     C                   move      xxunts        psuniq
     C                   move      dhpos         pspslc
    C*** END CHG *** 02/18/05 ****************************************
     C*** BEG ADD *** 07/18/06 *********************************************
     C*** BEG DEL *** 01/25/07 *********************************************
     C*****************  if        dhpos = ' 99'
     C*****************  move      '11'          pspslc
     C*****************  endif
     C*** END DEL *** 01/25/07 *********************************************
     C*** END ADD *** 07/18/06 *********************************************
     C*** BEG ADD *** 01/25/07 *********************************************
     C                   if        dhpos = ' 99' or dhpos < ' 11'
     C                   move      '11'          pspslc
     C                   endif
     C*** END ADD *** 01/25/07 *********************************************
     C                   movel     dhtos         psstyp
     C                   clear                   psmprc
     C                   clear                   psncam
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dhchrg = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   PSCHRG
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   if        eoast = 'KY'
    C*** END ADD *** 02/18/05 ****************************************
     C                   clear                   psuniq
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   endif
    C*** END ADD *** 02/18/05 ****************************************
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     pserv         f1data
     C                   write     pf101
    C*
     C     enxpsv        endsr
    C*----------------------------------------------------------------
    C*  SRXIST - INSTITUTIONAL SERVICE (HOSPITAL)
    C*----------------------------------------------------------------
     C     srxist        begsr
    C*
     C                   clear                   iserv
     C                   clear                   f1data
    C*
     C                   move      '52'          isqual
     C                   if        dubrat <> *blanks
    C*** BEGIN DLT *** 12/14/00 **************************************
    C********** DUBRVC    ORNE *ZEROS
    C*** END DLT ***** 12/14/00 **************************************
    C********** DURCFL    IFEQ 'C'
    C**********           MOVE 'ER'      ISSVQ1
    C**********           ENDIF                           DURCFL = C
    C********** DURCFL    IFEQ 'Y'
     C*** BEG ADD *** 01/25/07 *********************************************
     C                   if        %subst(eublty:1:2) = '13' or
     C                             %subst(eublty:1:2) = '83' or
     C                             %subst(eublty:1:1) = '7'
     C*** END ADD *** 01/25/07 *********************************************
     C                   move      'HC'          issvq1
     C*** BEG ADD *** 12/04/06 *********************************************
     C                   else
     C                   eval      issvq1 = *blanks
     C*
     C                   endif
     C*** END ADD *** 12/04/06 *********************************************
    C**********           ENDIF                           DURCFL = H
     C                   endif
     C                   if        dubrat <> *blanks
    C*** BEGIN ADD *** 11/16/06 **************************************
     C                             and dubrat <> 'ROOMM' and dubrat <> 'ROOMS'
    C*** END   ADD *** 11/16/06 **************************************
     C                   movel     dubrat        isprbc
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dubrat = *zeros
     C                   clear                   isprbc
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN DLT *** 12/14/00 **************************************
    C**********           ELSE
    C**********           MOVELDUBRVC    ISPRBC
    C*** BEGIN ADD *** 08/07/00 **************************************
    C********** DUBRVC    IFEQ *ZEROS
    C**********           MOVE *BLANKS   ISPRBC
    C**********           END
    C*** END ADD ***** 08/07/00 **************************************
    C*** END DLT ***** 12/14/00 **************************************
     C                   endif
    C*
     C                   if        durate <> *blanks
    C*** BEGIN DLT *** 12/14/00 **************************************
    C********** DURVCD    ORNE *ZEROS
    C*** END DLT ***** 12/14/00 **************************************
    C********** DURCFL    IFEQ 'C'
    C**********           MOVE 'ER'      ISSVQ2
    C**********           ENDIF                           DURCFL = C
    C********** DURCFL    IFEQ 'Y'
     C                   move      'HC'          issvq2
    C**********           ENDIF                           DURCFL = H
     C                   endif
     C                   if        durate <> *blanks
    C*** BEGIN ADD *** 11/16/06 **************************************
     C                             and durate <> 'ROOMM' and durate <> 'ROOMS'
    C*** END   ADD *** 11/16/06 **************************************
     C                   movel     durate        isproc
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        durate = *zeros
     C                   clear                   isproc
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEGIN DLT *** 12/14/00 **************************************
    C**********           ELSE
    C**********           MOVELDURVCD    ISPROC
    C*** BEGIN ADD *** 08/07/00 **************************************
    C********** DURVCD    IFEQ *ZEROS
    C**********           MOVE *BLANKS   ISPROC
    C**********           END
    C*** END ADD ***** 08/07/00 **************************************
    C*** END DLT ***** 12/14/00 **************************************
     C                   endif
    C*
     C                   if        isprbc = *blanks
     C                   move      issvq2        issvq1
     C                   movel     isproc        isprbc
     C                   endif
    C*
     C                   movel     dudesc        ispdsc
     C*** BEG ADD *** 01/25/07 *********************************************
     C*
    C* If not CA and this is not an outpatient bill clear the procedure code fld
     C*
     C                   if        %subst(eublty:1:2) <> '13' and
     C                             %subst(eublty:1:2) <> '83' and
     C                             %subst(eublty:1:1) <> '7'  and
     C                             eoast = 'TX'
     C*
     C                   clear                   issvq1
     C                   clear                   issvq2
     C                   clear                   isprbc
     C                   clear                   isproc
     C                   clear                   ispdsc
     C*
     C                   endif
     C*
     C                   if        eoast = 'CA'
     C                   eval      issvq1 = 'HC'
     C                   eval      issvq2 = 'HC'
     C                   endif
     C*
     C*** END ADD *** 01/25/07 *********************************************
    C**********           MOVELLGDIA1    ISPDIA
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   ischrg
     C                   move      *zeros        ischrg
    C*** END CHG ***** 05/13/06 **************************************
     C                   move      duchrg        ischrg
    C*** BEG CHG *** 02/18/05 ****************************************
    C******************** MOVE *BLANKS   ISUBAS
     C                   if        durate = *blanks
     C                   movel     'DA'          isubas
     C                   else
     C                   movel     'UN'          isubas
     C                   endif
    C*** END CHG *** 02/18/05 ****************************************
     C                   clear                   isuniq
    C*** BEG CHG *** 02/18/05 ****************************************
     C****************** move      duunts        isuniq
     C                   z-add     duunts        xxunts
     C                   move      xxunts        isuniq
    C*** END CHG *** 02/18/05 ****************************************
     C                   clear                   isdrat
     C                   clear                   isncam
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        duchrg = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   ISCHRG
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
    C*** BEG DEL *** 02/18/05 ****************************************
     C****************** clear                   isuniq
    C*** END DEL *** 02/18/05 ****************************************
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     iserv         f1data
     C                   write     pf101
    C*
     C     enxist        endsr
    C*----------------------------------------------------------------
    C*  SRXRX  - PHARMACY RECORD
    C*----------------------------------------------------------------
     C     srxrx         begsr
    C*
     C                   clear                   rxinfo
     C                   clear                   f1data
    C*
     C                   move      '53'          rxqual
     C                   movel     dprx#         rxprs#
     C                   if        dpbndc <> *blanks
     C                   move      'ND'          rxsvq1
     C                   endif
     C                   movel     dpbndc        rxprbc
     C                   if        dpndc <> *blanks
     C                   move      'ND'          rxsvq2
     C                   endif
     C                   movel     dpndc         rxproc
     C*** BEG ADD *** 02/23/07 *********************************************
     C                   if        dpndc = 'RX' or dpndc = 'E1399'
     C                   move      'ND'          rxsvq1
     C                   move      'ND'          rxsvq2
     C                   movel     '9999999999'  rxproc
     C                   movel     rxproc        rxprbc
     C                   movel     '1'           rxprs#
     C                   endif
     C*** END ADD *** 02/23/07 *********************************************
    C*
     C                   if        rxprbc = *blanks
     C                   move      rxsvq2        rxsvq1
     C                   movel     rxproc        rxprbc
     C                   endif
    C*
     C                   movel     dpdesc        rxdesc
     C                   clear                   rx#rfl
     C                   move      dprxrf        rx#rfl
     C                   movel     dprxgb        rxgenr
    C*** BEG CHG *** 02/18/05 ****************************************
    C******************** MOVELDPRXDI    RXDISW
     C                   movel     '1'           rxdisw
    C*** END CHG *** 02/18/05 ****************************************
     C                   clear                   rx#dsu
     C                   move      dprxdy        rx#dsu
     C                   eval      srdti = dpfrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         rxpldt
     C*** BEG CHG *** 05/19/2006 *******************************************
     C****************** clear                   rxchrg
     C                   move      *zeros        rxchrg
     C*** END CHG *** 05/19/2006 *******************************************
     C                   move      dpchrg        rxchrg
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dprxrf = *zeros
     C                   clear                   rx#rfl
     C                   endif
     C                   if        dprxdy = *zeros
     C*** BEG CHG *** 09/12/06 *********************************************
     C****************** clear                   rx#dsu
     C                   movel     *blanks       rx#dsu
     C                   movel     '1'           rx#dsu
     C*** END CHG *** 09/12/06 *********************************************
     C                   endif
     C                   if        dpfrdt = *zeros
     C                   clear                   rxpldt
     C                   endif
     C                   if        dpchrg = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   RXCHRG
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEG ADD *** 02/18/05 ****************************************
     C                   z-add     dpunts        xxunts
     C                   move      xxunts        rxqtyd
    C*** END ADD *** 02/18/05 ****************************************
    C*
     C     ddk1          chain     pdp02                              99
     C                   if        *in99 = *off
    C*** Beg Add *** 11/10/15 ****************************************
     C                   Eval      rxdspf='00000000000'
    C*** End Add *** 11/10/15 ****************************************
     C                   move      d1dfee        rxdspf
     C                   move      d1bcdt        rxbcdt
     C                   endif
    C*
    C*** BEG ADD *** 02/17/09 ****************************************
     C                   if        lgjst = 'TX'
     C                   Eval      rxbcdt = *blanks
     C                   endif
    C*** END ADD *** 02/17/09 ****************************************
     C                   movel     rxinfo        f1data
     C                   write     pf101
    C*
     C     enxrx         endsr
    C*----------------------------------------------------------------
    C*  SRXDEN - DENTAL RECORD
    C*----------------------------------------------------------------
     C     srxden        begsr
    C*
     C                   clear                   dental
     C                   clear                   f1data
    C*
     C                   move      '55'          dequal
     C                   if        ddbprc <> *blanks
     C                   if        ddrcfl = 'C'
     C                   move      'AD'          desvq1
     C                   else
     C                   move      'DE'          desvq1
     C                   endif
     C                   endif
     C                   movel     ddbprc        deprbc
     C                   if        ddprcd <> *blanks
     C                   if        ddrcfl = 'C'
     C                   move      'AD'          desvq2
     C                   else
     C                   move      'DE'          desvq2
     C                   endif
     C                   endif
     C                   movel     ddprcd        deproc
    C*
     C                   if        deprbc = *blanks
     C                   move      desvq2        desvq1
     C                   movel     deproc        deprbc
     C                   endif
    C*
     C*** BEG CHG *** 01/19/07 *********************************************
     C                   if        lgjst = 'CA'
     C                   move      'DE'          desvq1
     C                   endif
     C*** END CHG *** 01/19/07 *********************************************
    C*
     C                   movel     ddprds        dedesc
     C                   movel     ddpos         depslc
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** clear                   dechrg
     C                   move      *zeros        dechrg
    C*** END CHG ***** 05/13/06 **************************************
     C*** BEG ADD *** 07/18/06 *********************************************
     C                   if        ddpos = '99 '
     C                   movel     '11'          depslc
     C                   endif
     C*** END ADD *** 07/18/06 *********************************************
     C                   move      ddchrg        dechrg
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        ddchrg = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   DECHRG
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   movel     dental        f1data
     C                   write     pf101
    C*
     C     enxden        endsr
    C*----------------------------------------------------------------
    C*  SRXDME - DME RECORD
    C*----------------------------------------------------------------
     C     srxdme        begsr
    C*
     C                   clear                   dme
     C                   clear                   f1data
     C                   move      '57'          dmqual
     C*
    C*** BEG ADD *** 04/10/07 ****************************************
     C*
     C                   move      'DM'          dmsvq1
     C                   move      EUPRC2        dmprbc
     C*
     C* Check value of Purchase/Rent flag to determine field to populate.
     C*
     C                   select
     C                   when      d1prin = 'R'
     C                   move      DPCHRG        dmchrr
     C                   move      'DA'          dmubas
     C                   when      d1prin = 'P'
     C                   move      DPCHRG        dmchrp
     C                   move      'UN'          dmubas
     C                   other
     C                   endsl
     C*
     C                   move      lgfreq        dmbfcd
     C                   move      dpunts        dmuqty
     C*
    C*** END ADD *** 04/10/07 ****************************************
     C*
     C                   movel     dme           f1data
     C                   write     pf101
    C*
     C     enxdme        endsr
    C*----------------------------------------------------------------
    C*  SRXCOM - COMMON SERVICE RECORD
    C*----------------------------------------------------------------
     C     srxcom        begsr
    C*
     C                   clear                   comser
     C                   clear                   f1data
     C*** BEGIN ADD *** 06/16/08 *******************************************
     C                   eval      comod1=' '
     C                   eval      comod2=' '
     C                   eval      comod3=' '
     C                   eval      comod4=' '
     C*** END   ADD *** 06/16/08 *******************************************
    C*
     C                   move      '60'          coqual
    C*
     C                   select
    C*
     C                   when      *in10 = *on
     C                   eval      srdti = dhfrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cobded
     C                   eval      srdti = dhtodt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdat8
     C                   move      xxdat8        cobded
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dhfrdt = *zeros
     C                             or dhtodt = *zeros
     C                   clear                   cobded
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C*** BEGIN ADD *** 06/16/08 *******************************************
     C                   eval      comod1=dhmod1
     C                   eval      comod2=dhmod2
     C                   eval      comod3=dhmod3
     C                   eval      comod4=dhmod4
     C*** END   ADD *** 06/16/08 *******************************************
    C*** BEG ADD ***** 08/11/11 **************************************
     C                   eval      cornat = dhnpir
     C                   eval      corstl = dhrnlc
    C*** END ADD ***** 08/11/11 **************************************
    C*
     C                   when      *in11 = *on
     C                   eval      srdti = dufrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cobded
     C                   eval      srdti = dutodt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdat8
     C                   move      xxdat8        cobded
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dufrdt = *zeros
     C                             or dutodt = *zeros
     C                   clear                   cobded
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C*** BEGIN ADD *** 06/16/08 *******************************************
     C     duky2         chain     pdu02
     C                   if        %found
     C                   eval      comod1=d2pmod
     C                   eval      comod2=d2mod2
     C                   endif
     C*** END   ADD *** 06/16/08 *******************************************
    C*
     C                   when      *in12 = *on
     C                   eval      srdti = dpfrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cobded
     C                   eval      srdti = dptodt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdat8
     C                   move      xxdat8        cobded
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        dpfrdt = *zeros
     C                             or dptodt = *zeros
     C                   clear                   cobded
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   when      *in13 = *on
     C                   eval      srdti = ddfrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         cobded
     C                   eval      srdti = ddtodt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdat8
     C                   move      xxdat8        cobded
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        ddfrdt = *zeros
     C                             or ddtodt = *zeros
     C*** BEG ADD *** 05/13/06 ****************************************
     C                             or bibtyp = 'RX'
     C*** END ADD *** 05/13/06 ****************************************
     C                   clear                   cobded
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
    C*
     C                   endsl
    C*
     C*** BEG CHG *** 05/13/06 *********************************************
     C****************** clear                   copaid
     C                   move      *zeros        copaid
     C*** END CHG *** 05/13/06 *********************************************
     C                   clear                   cotusp
     C                   clear                   coppay
     C                   clear                   cooexp
     C                   if        *in10 = *on
     C*** Beg Add *** 02/20/12 **********************************************
     C                   eval      xxpaid = dhpaid
     C                   exsr      srfmt
     C*** End Add *** 02/20/12 **********************************************
     C*** Beg Del *** 02/20/12 **********************************************
     C******             move      dhpaid        copaid
     C*** End Del *** 02/20/12 **********************************************
     C                   endif
     C                   if        *in11 = *on
     C*** Beg Add *** 02/20/12 **********************************************
     C                   eval      xxpaid = dupaid
     C                   exsr      srfmt
     C*** End Add *** 02/20/12 **********************************************
     C*** Beg Del *** 02/20/12 **********************************************
     C******             move      dupaid        copaid
     C*** End Del *** 02/20/12 **********************************************
     C                   endif
     C                   if        *in12 = *on
     C*** Beg Add *** 02/20/12 **********************************************
     C                   eval      xxpaid = dppaid
     C                   exsr      srfmt
     C*** End Add *** 02/20/12 **********************************************
     C*** Beg Del *** 02/20/12 **********************************************
     C******             move      dppaid        copaid
     C*** End Del *** 02/20/12 **********************************************
     C                   endif
     C                   if        *in13 = *on
     C*** Beg Add *** 02/20/12 **********************************************
     C                   eval      xxpaid = ddpaid
     C                   exsr      srfmt
     C*** End Add *** 02/20/12 **********************************************
     C*** Beg Del *** 02/20/12 **********************************************
     C******             move      ddpaid        copaid
     C*** End Del *** 02/20/12 **********************************************
     C                   endif
     C                   if        *in10 = *on
     C                   move      dhunts        cotusp
     C                   endif
     C                   if        *in11 = *on
     C                   move      duunts        cotusp
     C                   endif
     C                   if        *in12 = *on
     C                   move      dpunts        cotusp
     C                   endif
     C                   if        *in13 = *on
     C                   move      ddunts        cotusp
     C                   endif
     C                   if        *in10 = *on
     C                   move      dhppd         coppay
     C                   endif
     C                   if        *in11 = *on
     C                   move      duppd         coppay
     C                   endif
     C                   if        *in12 = *on
     C                   move      dpppd         coppay
     C                   endif
     C                   if        *in13 = *on
     C                   move      ddppd         coppay
     C                   endif
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        copaid = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   COPAID
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
     C                   clear                   cotusp
     C                   if        coppay = *zeros
    C*** BEGIN DLT *** 01/29/01 **************************************
    C**********           MOVE *BLANKS   COPPAY
    C*** END DLT ***** 01/29/01 **************************************
     C                   endif
     C                   clear                   cooexp
    C*** END ADD ***** 08/07/00 **************************************
    C*** BEG ADD ***** 02/18/05 **************************************
     C                   if        *in10 = *on
     C                   movel     dheob1        coeob1
     C                   movel     dheob2        coeob2
     C                   movel     dheob3        coeob3
     C                   movel     dheob4        coeob4
     C                   movel     dheob5        coeob5
     C                   movel     dheob6        coeob6
     C                   movel     dheob7        coeob7
     C                   movel     dheob8        coeob8
     C                   movel     dheob9        coeob9
     C                   movel     dheo10        cosagc
     C                   endif
     C                   if        *in11 = *on
     C                   movel     dueob1        coeob1
     C                   movel     dueob2        coeob2
     C                   movel     dueob3        coeob3
     C                   movel     dueob4        coeob4
     C                   movel     dueob5        coeob5
     C                   movel     dueob6        coeob6
     C                   movel     dueob7        coeob7
     C                   movel     dueob8        coeob8
     C                   movel     dueob9        coeob9
     C                   movel     dueo10        cosagc
     C                   endif
     C                   if        *in12 = *on
     C                   movel     dpeob1        coeob1
     C                   movel     dpeob2        coeob2
     C                   movel     dpeob3        coeob3
     C                   movel     dpeob4        coeob4
     C                   movel     dpeob5        coeob5
     C                   movel     dpeob6        coeob6
     C                   movel     dpeob7        coeob7
     C                   movel     dpeob8        coeob8
     C                   movel     dpeob9        coeob9
     C                   movel     dpeo10        cosagc
     C                   endif
     C                   if        *in13 = *on
     C                   movel     ddeob1        coeob1
     C                   movel     ddeob2        coeob2
     C                   movel     ddeob3        coeob3
     C                   movel     ddeob4        coeob4
     C                   movel     ddeob5        coeob5
     C                   movel     ddeob6        coeob6
     C                   movel     ddeob7        coeob7
     C                   movel     ddeob8        coeob8
     C                   movel     ddeob9        coeob9
     C                   movel     ddeo10        cosagc
     C                   endif
    C*** BEG ADD ***** 01/25/07 **************************************
     C*
     C* For historical we will only send 3 EOB codes
     C*
     C                   movel     *blanks       coeob4
     C                   movel     *blanks       coeob5
     C                   movel     *blanks       coeob6
     C                   movel     *blanks       coeob7
     C                   movel     *blanks       coeob8
     C                   movel     *blanks       coeob9
     C*
     C* For historical use EOB code 144 instead of 137
     C*
     C                   if        coeob1 = '137'
     C                   movel     '144'         coeob1
     C                   endif
     C*
     C                   if        coeob2 = '137'
     C                   movel     '144'         coeob2
     C                   endif
     C*
     C                   if        coeob3 = '137'
     C                   movel     '144'         coeob3
     C                   endif
    C*** END ADD ***** 01/25/07 **************************************
     C*
     C* Adjustment 1
     C*
     C                   eval      xxsadj = *zeros
     C                   eval      cosadj = *zeros
     C                   if        *in10 = *on
     C                   eval      xxsadj = dhchrg - dhpaid
     C                   endif
     C                   if        *in11 = *on
     C                   eval      xxsadj = duchrg - dupaid
     C                   endif
     C                   if        *in12 = *on
     C                   eval      xxsadj = dpchrg - dppaid
     C                   endif
     C                   if        *in13 = *on
     C                   eval      xxsadj = ddchrg - ddpaid
     C                   endif
    C*** BEG ADD ***** 12/04/06 **************************************
     C                   eval      xxsadj = %abs(xxsadj)
    C*** END ADD ***** 12/04/06 **************************************
     C                   if        xxsadj = 0
     C                   movel     *blanks       cosadj
     C                   else
     C                   move      xxsadj        cosadj
     C                   endif
    C*** BEG ADD ***** 07/18/06 **************************************
     C                   if        xxsadj <  *zeros
     C                   movel     *blanks       cosadj
     C                   movel     *blanks       cosunt
     C                   endif
     C*
     C                   if        cosadj =  *blanks
     C                   move      *blanks       coeob1
     C                   move      *blanks       cosagc
     C                   endif
     C*
     C* For historical default the EOB code and group if blank
     C*
     C                   if        cosadj <> *blanks and coeob1 = *blanks
     C                   movel     'W1'          coeob1
     C                   movel     'OA'          cosagc
     C                   endif
    C*** END ADD ***** 07/18/06 **************************************
     C*
     C* Adjustment 2
     C*
     C                   if        coeob2 <> *blanks
     C*
     C                   move      '0000000'     cosun2
     C*
     C                   eval      xxsadj = *zeros
     C                   eval      cosad2 = *zeros
     C                   if        *in10 = *on
     C                   eval      xxsadj = dhchrg - dhpaid
     C                   endif
     C                   if        *in11 = *on
     C                   eval      xxsadj = duchrg - dupaid
     C                   endif
     C                   if        *in12 = *on
     C                   eval      xxsadj = dpchrg - dppaid
     C                   endif
     C                   if        *in13 = *on
     C                   eval      xxsadj = ddchrg - ddpaid
     C                   endif
    C*** BEG ADD ***** 12/04/06 **************************************
     C                   eval      xxsadj = %abs(xxsadj)
    C*** END ADD ***** 12/04/06 **************************************
     C                   if        xxsadj = 0
     C                   movel     *blanks       cosad2
     C                   else
     C                   move      xxsadj        cosad2
     C                   endif
    C*** BEG ADD ***** 07/18/06 **************************************
     C*
     C                   if        cosad2 =  *blanks
     C                   move      *blanks       coeob2
     C                   move      *blanks       cosun2
     C                   endif
    C*** END ADD ***** 07/18/06 **************************************
     C*
     C                   endif
     C*
     C* Adjustment 3
     C*
     C                   if        coeob3 <> *blanks
     C*
     C                   move      '0000000'     cosun3
     C*
     C                   eval      xxsadj = *zeros
     C                   eval      cosad3 = *zeros
     C                   if        *in10 = *on
     C                   eval      xxsadj = dhchrg - dhpaid
     C                   endif
     C                   if        *in11 = *on
     C                   eval      xxsadj = duchrg - dupaid
     C                   endif
     C                   if        *in12 = *on
     C                   eval      xxsadj = dpchrg - dppaid
     C                   endif
     C                   if        *in13 = *on
     C                   eval      xxsadj = ddchrg - ddpaid
     C                   endif
    C*** BEG ADD ***** 12/04/06 **************************************
     C                   eval      xxsadj = %abs(xxsadj)
    C*** END ADD ***** 12/04/06 **************************************
     C                   if        xxsadj = 0
     C                   movel     *blanks       cosad3
     C                   else
     C                   move      xxsadj        cosad3
     C                   endif
    C*** BEG ADD ***** 01/25/07 **************************************
     C*
     C                   if        cosad3 =  *blanks
     C                   move      *blanks       coeob3
     C                   move      *blanks       cosun3
     C                   endif
     C*
    C*** END ADD ***** 01/25/07 **************************************
     C*
     C                   endif
     C*
     C                   if        *in11 = *on
     C                   movel     dubrvc        corvbc
     C                   movel     durvcd        corvpc
     C                   endif
    C*** BEG CHG ***** 11/13/05 **************************************
     C****************** movel     *blanks       cosunt
    C*** BEG CHG ***** 05/13/06 **************************************
     C****************** movel     *zeros        cosunt
     C                   movel     *blanks       cosunt
    C*** END CHG ***** 05/13/06 **************************************
    C*** END CHG ***** 11/13/05 **************************************
     C                   movel(p)  bipagr        copagr
    C*** END ADD ***** 02/18/05 **************************************
    C*** BEG ADD ***** 05/13/06 **************************************
     C                   if        xxsadj > 0 or
     C                             cosagc <> *blanks or coeob1 <> *blanks
     C                   move      xxunts        cosunt
     C                   endif
    C*** END ADD ***** 05/13/06 **************************************
    C*** BEGIN ADD *** 08/07/00 **************************************
     C                   if        *in11 = *on
     C                   if        dubrvc = *zeros
     C                   clear                   corvbc
     C                   endif
     C                   if        durvcd = *zeros
     C                   clear                   corvpc
     C                   endif
     C                   endif
    C*** END ADD ***** 08/07/00 **************************************
     C*** Beg Add **** 11/16/16 ***************************************
      /Free
       //*** Beg Chg *** 06/28/17 *************************************
       //***   If ##clt# = '025' and DHnpir <> *Blanks;
          If ##clt# = '025' and DHnpir <> *Blanks and *in10 = *on;
       //*** End Chg *** 06/28/17 *************************************
             Monitor;
                strNPIR = %Dec(%Subst(%Trim(DHnpir):1:10):10:0);
             On-Error;
                strNPIR = *Zeros;
             EndMon;
             Chain (strNPIR) trpnpimstr;
             If %Found(trpnpimst);
                COrcty = NPcity  ;
                COrcnt = NPcou   ;
                COrfei = *Blanks ;
                COrfnm = NPfname ;
                COrlnm = NPsuffix;

                If NPlname <> *Blanks;
                   COrgnm = NPlname ;
                Elseif NPoname <> *Blanks;
                   COrgnm = NPoname ;
                EndIf;

                COrmed = *Blanks ;
                COrmid = NPmname ;
                COrpos = NPzip   ;
                COrad1 = NPaddr1 ;
                COrspc = *Blanks ;
                COrad2 = NPaddr2 ;

          //*** Beg Del *** 07/06/17 ********************************
          //***
          //*** If COrstl = *Blanks;
          //***    Select;
          //***    When NPlics1 = 'TX'  ;
          //***       COrstl = NPlic1   ;
          //***       COrst  = NPlics1  ;
          //***    When NPlics2 = 'TX'  ;
          //***       COrstl = NPlic2   ;
          //***       COrst  = NPlics2  ;
          //***    When NPlics3 = 'TX'  ;
          //***       COrstl = NPlic3   ;
          //***       COrst  = NPlics3  ;
          //***    When NPlics4 = 'TX'  ;
          //***       COrstl = NPlic4   ;
          //***       COrst  = NPlics4  ;
          //***    When NPlics5 = 'TX'  ;
          //***       COrstl = NPlic5   ;
          //***       COrst  = NPlics5  ;
          //***    When NPlics6 = 'TX'  ;
          //***       COrstl = NPlic6   ;
          //***       COrst  = NPlics6  ;
          //***    When NPlics7 = 'TX'  ;
          //***       COrstl = NPlic7   ;
          //***       COrst  = NPlics7  ;
          //***    When NPlics8 = 'TX'  ;
          //***       COrstl = NPlic8   ;
          //***       COrst  = NPlics8  ;
          //***    When NPlics9 = 'TX'  ;
          //***       COrstl = NPlic9   ;
          //***       COrst  = NPlics9  ;
          //***    When NPlics10 = 'TX' ;
          //***       COrstl = NPlic10  ;
          //***       COrst  = NPlics10 ;
          //***    When NPlics11 = 'TX' ;
          //***       COrstl = NPlic11  ;
          //***       COrst  = NPlics11 ;
          //***    When NPlics12 = 'TX' ;
          //***       COrstl = NPlic12  ;
          //***       COrst  = NPlics12 ;
          //***    When NPlics13 = 'TX' ;
          //***       COrstl = NPlic13  ;
          //***       COrst  = NPlics13 ;
          //***    When NPlics14 = 'TX' ;
          //***       COrstl = NPlic14  ;
          //***       COrst  = NPlics14 ;
          //***    When NPlics15 = 'TX' ;
          //***       COrstl = NPlic15  ;
          //***       COrst  = NPlics15 ;
          //***    EndSl;
          //*** EndIf;
          //*** End Del *** 07/06/17 ********************************
             EndIf;
          EndIf;
      /End-Free
     C*** End Add **** 11/16/16 ***************************************
    C*
     C                   movel     comser        f1data
     C                   write     pf101
    C*
     C     enxcom        endsr
    C*----------------------------------------------------------------
    C*  SRXTRL - TRAILER RECORD
    C*----------------------------------------------------------------
     C     srxtrl        begsr
    C*
     C                   clear                   trail
     C                   clear                   f1data
    C*
     C                   movel     'TR1'         trqual
     C                   clear                   trdcnt
     C                   move      xxrcnt        trdcnt
    C*
     C                   movel     trail         f1data
     C                   write     pf101
    C*
     C     enxtrl        endsr
     C*** Beg Add *** 02/20/12 **********************************************
    C*----------------------------------------------------------------
    C*  SrFmt - Format negative amount
    C*----------------------------------------------------------------
     C     SrFmt         begsr
    C*
     C                   if        xxpaid < 0
    C*
     C                   eval      xxpchr = %char(xxpaid)
     C                   eval      xxpos = 14- (%len(%trim(xxpchr)) - 2)
    C*
     C                   eval      xxpaid1 = %abs(xxpaid)
    C*
     C                   move      xxpaid1       copaid
     C                   eval      copaid =  %replace('-':copaid:xxpos:1)
     C                   else
    C*
     C                   move      xxpaid        copaid
     C                   endif
    C*
     C                   endsr
    C*----------------------------------------------------------------
    C*  SrFmtLg - Format negative amount for LGPAID
    C*----------------------------------------------------------------
     C     SrFmtLg       begsr
    C*
     C                   if        xxpaid < 0
    C*
     C                   eval      xxpchr = %char(xxpaid)
     C                   eval      xxpos = 14- (%len(%trim(xxpchr)) - 2)
    C*
     C                   eval      xxpaid1 = %abs(xxpaid)
    C*
     C                   move      xxpaid1       bipaid
     C                   eval      bipaid =  %replace('-':bipaid:xxpos:1)
     C                   else
    C*
     C                   move      xxpaid        bipaid
     C                   endif
    C*
     C                   endsr
     C*** End Add *** 02/20/12 **********************************************

     C*** Beg Add *** 11/16/16 ****************************
    C*-------------------------------------------------------------------------
    C*  srEnd - Close TRPNPIMST (NPI Master database file for business address
    C*-------------------------------------------------------------------------
     C     srEnd         begsr
      /Free
          If ##clt# = '025';
             If %Open(trpnpimst);
                Close trpnpimst;
             EndIf;
          EndIf;
      /End-Free
     C                   endsr
     C*** End Add *** 11/16/16 ****************************
