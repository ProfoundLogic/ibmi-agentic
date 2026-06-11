     D WriteRmk        PR             1a
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

     D WriteAddInfo    PR
     D  eKey                         40a                                        Key
     D  eSequence                     5p 0                                      Sequence
     D  eAllowCF                      1a                                        Remark
     D  eDataEntryP                  10a                                        Remark date
     D  eDataEntryK                  40a                                        Remark code
     D  eProgram                     10a                                        Maint program
     D  eUser                        10a                                        Maint user

     D WriteAddl       PR             1a
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

      *** BEG ADD *** 03/13/12 ****************************************
     D ResSplDDoc      pr
     D  esplf                        10a
     D  eque                         10a
     D  errn                          9p 0
      *** END ADD *** 03/13/12 ****************************************
      *** BEG ADD *** 03/16/12 ****************************************
     d WriteDarts      pr
     d eKey                          40                                         01
     d eComm                        256                                         02
     d eCdat                          8                                         03
     d eDcid                         12                                         04
     d ePnam                         10                                         05
     d eSecl                          3                                         06
     d eUser                         10                                         07
     d ePgm                          10                                         08
     d eFold                         80                                         09
     d eDifl                          1                                         10
     d eIpgm                         10                                         11
     d eFl01                          1                                         12
     d eFl02                          1                                         13
     d eRmcd                          5                                         14
     d eSeq                           5                                         15
     d eSbyl                          1    options(*nopass)                     16
     d eCom2                         19    options(*nopass)                     17
     d eApid                          2    options(*nopass)                     18
     d eIdoc                         12    options(*nopass)                     19
     d eIque                         10    options(*nopass)                     20
     d eImgf                          1    options(*nopass)                     21
     d eIpth                         10    options(*nopass)                     22
     d ePidx                          5    options(*nopass)                     23
     d eQpty                          1    options(*nopass)                     24
     d eDctp                          5    options(*nopass)                     25
     d eCsid                          2    options(*nopass)                     26
     d eCode                          2    options(*nopass)                     27
     d eTrdt                          7    options(*nopass)                     28
     d eTrtm                          6    options(*nopass)                     29
     d eStat                          1    options(*nopass)                     30
     d eIusr                         10    options(*nopass)                     31
     d eDcdt                          7    options(*nopass)                     32
     d eFl03                          1    options(*nopass)                     33
     d eFl04                          1    options(*nopass)                     34
     d eFl05                          1    options(*nopass)                     35
     d eIdt                           7    options(*nopass)                     36
     d eVcfn                          1    options(*nopass)                     37
     d eDpgm                         10    options(*nopass)                     38
     d eDkey                         40    options(*nopass)                     39
     d ePath                         80    options(*nopass)                     40
     d eRpyn                          1    options(*nopass)                     41
      *** END ADD *** 03/16/12 ****************************************
