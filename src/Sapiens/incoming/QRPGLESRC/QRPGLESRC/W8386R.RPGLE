     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h/TITLE W8386R - FROI Import
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio:*srcstmt)
      //
      //********************************************************************************************
      //  DESCRIPTION:  This program reads the flat file WXXFRP2, generates
      //                a JSON to interface with WIFROI which will edit
      //                and submit FROI.
      //
      //  Called from:  W8386C - Override
      //  Calls      :  None
      //********************************************************************************************
      //  Created on 03/31/2020 by C:180 for Project 129WVX
      //********************************************************************************************
      // Revisions:
      // 07.27.21 C:180  Support 97009
      //                 Pass value FROIimport to WIFROI.
      // 02.22.21 C:180  Support: 91730   Capture FROI errors for screen.   .
      // 10.13.20 C:180  Project: C129WVX Json Key build up per V20R4 WIFROI.
      //
      //
      //
      //
      //
      //********************************************************************************************
     fWXFRP2    if   e             disk
     fwxerp3    o    e             disk    Rename(per01:per0p) usropn

      *Entry
     dW8386R           pr
     d eUpld                          1
     d eRpDt                          7
     dW8386R           pi
     d eUpld                          1
     d eRpDt                          7

     dWIFROI           pr                  ExtPgm('WIFROI')
     d                              256
     d                               10
     d                                 a   len(8500000) varying
     d                                 a   len(8500000) varying
     d                               10

      //============================================================================================
      // Stand Alone
      //============================================================================================
     d SaveFROI        s            256a   inz('SaveFROI')
     d SubmitFROI      s            256a   inz('SubmitFROI')
     d InJson          s               a   len(8500000) varying inz
     d OutJson         s               a   len(8500000) varying inz
     d InJson2         s               a   len(8500000) varying inz
     d OutRtnCode      s             10A   inz
     d sysDateCymd     s              7s 0
     d errorCount      s              6  0 inz
     d sys_date        s               d   datfmt(*usa)
     d intrecord       s              9s 0 inz


      //============================================================================================
      // Constants
      //============================================================================================

      //============================================================================================
      // Data structures
      //============================================================================================
     d PSDS          ESDS                  extname(SZQ1P)

     D JsonDS          DS         32767
     d  JSON1                      1021
     d  JSON2                      1021
     d  JSON3                      1021
     d  JSON4                      1021
     d  JSON5                      1021
     d  JSON6                      1021
     d  JSON7                      1021
     d  JSON8                      1021
     d  JSON9                      1021
     d  JSON10                     1021

      //===============================================================
      // Main Processing
      //===============================================================
      /Free
       ProgramStart();
       ErrorCheckFile();
       If errorCount = *zero;
          eUpld = 'Y';
          eRpDt = %Editc(sysDateCymd:'X');
          ProcessWXFRP2();
       Else;
          eUpld = 'N';
       Endif;
       *Inlr = *On;
      /End-Free
      //------------------------------------------------------------------------*
      // processWXFRP2 - processes the flat file.
      //------------------------------------------------------------------------*
     p ProcessWXFRP2   b
     d ProcessWXFRP2   pi

     d intKeyBegin     s              5  0 Inz
     d intKeyEnd       s              5  0 Inz
     d intKeyLen       s              5  0 Inz
      /Free
       Setll 1 pfr01;
       Read pfr01;
       Dow Not %Eof;
          intRecord += 1;
          Exsr BuildFroiJson;
          OutJson = *blank;
          //*** Begin Change *** 07/27/21 ************************************
          //OutRtnCode = *blank;
          OutRtnCode = 'FROIimport';
          //*** End   Change *** 07/27/21 ************************************
          WIFROI(SaveFROI:Q1USER:InJson:OutJson:OutRtnCode);
          If OutRtnCode = *blanks;
             Exsr BuildFroiJson2;
             //*** Begin Add  *** 07/27/21 ************************************
             OutRtnCode = 'FROIimport';
             //*** End   Add  *** 07/27/21 ************************************
             WIFROI(SubmitFROI:Q1USER:InJson2:OutJson:OutRtnCode);
             If OutRtnCode <> *blanks;
                LoadFroiErrors();
                eUpld = 'N';
                eRpDt = *Zeros;
                Leave;
             Endif;
          Else;
           LoadFroiErrors();
           eUpld = 'N';
           eRpDt = *Zeros;
           Leave;
          Endif;
          Read pfr01;
       Enddo;

       If %open(WXERP3);
         Close WXERP3;
       Endif;

       *inlr = *On;
      //------------------------------------------------------------------------*
      // BuildFroiJson - Builds Json for FROI                                   *
      //------------------------------------------------------------------------*
       Begsr BuildFroiJson;
       InJson = *Blanks;
       InJson = '{   +
         "environment": "DVP",  +
         "company": { +
            "number":"' + %Editc(FRco#:'X') + '", +
            "name": "", +
            "fein": "", +
            "address": { +
               "address1": "", +
               "address2": "", +
               "city": "", +
               "state": "", +
               "zip": "" +
          } },' +
        '"group": { +
            "number": "' + %Editc(FRfnd:'X') + '", +
            "name": "", +
            "fein": "", +
            "carrierCode": "", +
            "address": { +
               "mailing": { +
               "address1": "", +
               "address2": "", +
               "city": "", +
               "state": "", +
               "zip": "" +
               }, +
               "physical": { +
                  "address1": "",+
                  "address2": "",+
                  "city": "", +
                  "state": "", +
                  "zip": "" +
                } +
            }, +
            "alternate": { +
               "name": "", +
               "address": { +
                  "address1": "", +
                  "address2": "", +
                  "city": "", +
                  "state": "", +
                  "zip": "" +
               } +
            } +
         },' +
        '"employer": { +
            "fein": "'+ %Editc(FRfein:'X') + '", +
            "number": "'+ %Editc(FRemp#:'X') + '", +
            "division": "' + %Editc(FRdiv:'X') + '", +
            "policyNumber": "", +
            "carrierPolicyNumber": "", +
            "name": { +
               "dbaPersonalName": "", +
               "dba": "", +
               "federalPersonalName": "", +
               "federal": "" +
            }, +
            "address": { +
               "mailing": { +
                  "address1": "", +
                  "address2": "", +
                  "city": "", +
                  "state": "", +
                  "zip": "" +
               }, +
               "physical": { +
                  "address1": "", +
                  "address2": "", +
                  "city": "", +
                  "state": "", +
                  "zip": "" +
               } +
            }, +
            "location": { +
               "number": "' + FRlocn +'", +
               "name": "", +
               "address": { +
                  "mailing": { +
                     "address1": "", +
                     "address2": "", +
                     "city": "", +
                     "state": "", +
                     "zip": "" +
                  }, +
                  "physical": { +
                     "address1": "", +
                     "address2": "", +
                     "city": "", +
                     "state": "", +
                     "zip": "" +
                  } +
               }, +
               "county": "" +
            }, +
            "phone": { +
               "areacode": "", +
               "phone": "", +
               "type": "" +
            },+
            "email": "",+
            "sic": { +
               "code": "", +
               "description": "" +
            }, +
            "contact": "", +
            "selfInsured": "Y", +
            "natureOfBusiness": "", +
            "typeOfEmployer": "", +
            "uiAccountNumber": "", +
            "stateRiskId": "", +
            "policyDates": "" +
         },' +
        '"agent": { +
            "number": "", +
            "name": "" +
         },' +
        '"claimant": { +
            "ssn": "' + FRssn + '", +
            "name": { +
               "first": "' + FRemfn + '", +
               "last": "' + FRemln + '", +
               "middle": "' + FRemmi + '", +
               "full": "' + %trim(FRemfn) + ' ' + FRemln + '" +
            }, +
            "dob": "' + FRemdob + '", +
            "dod": "' + FRemdod + '", +
            "address": { +
               "address1": "' + FRemadr1 + '", +
               "address2": "' + FRemadr2 + '", +
               "city": "' + FRemcity + '", +
               "state": "' + FRemst + '", +
               "zip": "' + %Editc(FRemzip:'X') + '" +
            },+
            "phone": [{ +
               "phone1": { +
               "areacode": "' + %Editc(FRemarea:'X') + '", +
               "phone": "' + %Editc(FRemphnbr:'X') + '", +
               "type": "" +
               } +
            }, { +
               "work": { +
                  "areacode": "", +
                  "phone": "", +
                  "type": "" +
               } +
            }], +
            "email": "", +
            "maritalStatus": "' + FRemsts + '", +
            "numberDependents": "' + %Editc(FR#dep:'X') + '", +
            "stateOfHire": "' + FRsthire + '", +
            "gender": "' + FRemgen + '", +
            "occupationDescription": "' + FRoccdsc + '", +
            "employmentStatus": { +
               "code": "' + FRempsts + '", +
               "description": "" +
            } +
         },' +
        '"physician": { +
            "name": "' + FRprvnam + '", +
            "address": { +
               "address1": "' + FRphyadr1 + '", +
               "address2": "' + FRphyadr2 + '", +
               "city": "' + FRphycity + '", +
               "state": "' + FRphyst + '", +
               "zip": "' + %Editc(FRphyzip:'X') + '" +
            }, +
            "phone": { +
               "areacode": "' + %Editc(FRphyarea:'X') + '", +
               "phone": "' + %Editc(FRphyphnbr:'X') + '", +
               "type": "" +
            }, +
            "extraInfo": "" +
         },' +
        '"hospital": { +
            "name": "' + FRhosnam + '", +
            "address": { +
               "address1": "' + FRhosadr + '", +
               "address2": "", +
               "city": "' + FRhoscity + '", +
               "state": "' + FRhosst + '", +
               "zip": "' + %Editc(FRhoszip:'X') + '" +
             },+
             "phone": { +
                "areacode": "", +
                "phone": "",+
                "type": "" +
             }, +
            "extraInfo": "" +
         },' +
        '"preparer": { +
            "name": "' + FRprenam +'", +
            "title": "", +
            "phone": { +
               "areacode": "' + %Subst(FRprephnbr:1:3) + '", +
               "phone": "' + %Subst(FRprephnbr:4:7) + '", +
               "type": "" +
            }, +
            "extension": "", +
            "preparedDate": "' + FRpredat + '" +
         },' +
        '"accident": { +
             "date": "' + FRaccdat + '", +
             "time": "' + %Editc(FRacctim:'X') + '", +
             "address": { +
                "address1": "' + FRaccadr1 + '", +
                "address2": "' + FRaccadr2 + '", +
                "city": "' + FRacccity + '", +
                "state": "' + FRaccst + '", +
                "zip": "' + %Editc(FRacczip:'X') + '" +
              } +
         },' +
        '"year": "' + %Editc(FRfndyr:'X') +'", +
         "case": "' + %Editc(FRcase#:'X') + '", +
         "jurisdictionState": "' + FRjrst + '", +
         "agencyClaimNumber": "", +
         "class": { +
            "code": "' + %Editc(FRclasscod:'X') + '", +
            "description": "" +
         },' +
        '"wage": { +
             "period": "' + FRwgppd + '", +
             "fullPaid": "' + FRpdInjD + '", +
             "averageWeekly": { +
                "wage": "' + %Editc(FRAWW:'X') + '", +
                "estimateActual": "" }, +
             "fullSalaryContinued": "' + FRfsal + '",+
             "daysOverAWeek": "' + %Editc(FRdyovrwk:'X') + '",+
             "beginTime": "' + %Editc(FRbegwtim:'X') + '", +
             "atSame": "", +
             "preInjuryWeekly": "", +
             "postInjuryWeekly": "", +
             "perHour": "", +
             "perDay": "",  +
             "fiftyTwoWeeksPrior": "" +
         },' +
        '"nature": { +
             "code": "' + FRnatcod + '", +
             "description": "", +
             "otherDescription": "" +
         }, +
         "part": { +
             "code": "' + FRpartcod + '", +
             "description": "", +
             "otherDescription": "" +
         }, +
         "cause": { +
             "code": "' + FRcauscod + '", +
             "description": "", +
             "otherDescription": "" +
         },' +
        '"dates": { +
             "lastWorked": "' + FRlstwdate + '",+
             "reported": "' + FRreptdate + '", +
             "disability": "' + FRdisdate + '", +
             "returnToWork": "' + FRrtndate + '", +
             "reportedAsWorkRelated": "", +
             "adminNotified": "", +
             "employeeProvidedClaimForm": "", +
             "hire": "' + FRhiredate + '", +
             "preperation": "", +
             "lastFullDayPaid": "", +
             "recieved": "' + FRrcvdate + '", +
             "fullSalary": "", +
             "fifthDayOfTotalOrPartial": "", +
             "eightDayOfDisability": "" +
         },' +
        '"contact": { +
             "name": { +
                "first": "' + FRcontfn + '", +
                "last": "' + FRcontln + '", +
                "middle": "' + FRcontmi + '" +
             }, +
             "phone": { +
                "areacode": "' + FRcontarea + '", +
                "phone": "' + FRcontph# + '", +
                "type": "" +
             } +
         },' +
        '"employerPremises": "' + FRempprem + '", +
         "initialTreatment": { +
             "code": "' + FRinittre + '", +
             "description": "" +
         },' +
        '"safeguard": { +
             "provided": "' + FRsafequp + '", +
             "used": "' + FRsafequus + '" +
         },' +
        '"equipmentUsed": "' + FRequpmat + '",+
         "employeeActivity": "' + FRacteng + '", +
         "workProcess": "' + FRwrkProc + '", +
         "howInjuryOccurred": "' + FRinjoccd + '", +
         "equipmentMaterialsChemicals": "", +
         "specificActivity": "", +
         "accidentDescrption": "", +
         "sourceOfInjury": "", +
         "witnesses": "", +
         "thirdParty": "", +
         "relative": "", +
         "witness": "" +
       }';
       JSONDS = InJson;
       Endsr;
      //------------------------------------------------------------------------*
      // BuildFroiJson2 - Builds Json for Submit FROI                           *
      //------------------------------------------------------------------------*
       Begsr BuildFroiJson2;
       InJson2 = *Blanks;

       //*** Begin Change *** 10/13/20 ************************************
       //InJson2 = '{   +
       //  "company": "' + %Editc(FRco#:'X') +'",  +
       //  "claimantSSN": "' + FRssn + '", +
       //  "accidentDate":"' + %char(%date(FRaccdat:*Iso0):*Usa0)+ '", +
       //  "initialDate": "' + %char(sys_date:*Usa0) + '" }';

       intKeyBegin = %scan('"keyInfo':OutJson);
       intKeyEnd = %scan('}':OutJson:intKeyBegin + 1);
       If intKeyBegin > *Zero and intKeyEnd > intKeyBegin;
          intKeyLen = intKeyEnd - intKeyBegin + 1;
          InJson2 ='{' + %subst(OutJson:intKeyBegin:intKeyLen) + '}';
       Endif;
       //*** End   Change *** 10/13/20 ************************************


       Endsr;
      /End-Free
     p ProcessWXFRP2   e
      //---------------------------------------------------------------
      // errorCheckFile - Reads thru WXFRP2 and looks for errors
      //---------------------------------------------------------------
     p ErrorCheckFile  b
     d ErrorCheckFile  pi
      /Free
       // Check for errors in the file

       //No Data
       Setll 1 pfr01;
       Read pfr01;
       If %Eof;
          errorCount = errorCount + 1;
       Endif;
      /End-Free
     p ErrorCheckFile  E
      //---------------------------------------------------------------
      // LoadFroiErrors - Load Froi Erros
      //---------------------------------------------------------------
     p LoadFroiErrors  b
     d LoadFroiErrors  pi
     d isDone          s               n   inz
     d intEnd          s              9s 0 inz
     d strErrors       s           5000a   inz
     d intLength       s              9s 0 inz
     d intstart        s              9s 0 inz
     d idx             s              9s 0 inz
     d strAcdt         s              8a   inz
      *** Begin Add *** 07/27/21 *************************************
     d intOffSet       s              9s 0 inz
      *** End Add ***** 07/27/21 *************************************
      /Free
       clear JsonDs;
       JsonDs = OutJson;
       idx = %Scan('accidentDate':outJson:1);
       //*** Begin Change *** 07/27/21 **********************************
       //intStart = idx + 16;
       //strAcdt = %Subst(outJson:intStart:8);
       If idx > *Zero;
          intOffSet = 16;
       Else;
          idx = %Scan('accident": {"date": "':outJson:1);
          If idx > *Zero;
             intOffSet = 21;
          Endif;
       Endif;

       If idx > *Zero;
          intStart = idx + intOffSet;
          strAcdt = %Subst(outJson:intStart:8);
       Else;
          strAcdt = 'Not Found';
       Endif;
       //*** End   Change *** 07/27/21 **********************************

       idx      = %Scan('errorMsg':outJson:1);
       intStart  = %Scan('[':outJson:idx) + 1;
       intEnd  = %Scan(']':outJson:idx);
       intLength = intEnd - intStart;
       strErrors = %Subst(outJson:intStart:intlength);

       // Write to Error File
       isDone = *Off;
       Dow not isDone;
          Clear per0p;
          ERrrn = intRecord;
          ERfsn = FRemfn;
          ERlsn = FRemln;
          ERmid = FRemmi;
          ERacdt = strAcdt;

          idx = %Scan(',':strErrors:1);
          If idx = 0;
             ERerr = strErrors;
             isDone  = *On;
          Else;
             ERerr  = %Subst(strErrors:1:idx-1);
             strErrors = %Subst(strErrors:idx+1);
          Endif;
          Write per0p;
       EndDo;
      /End-Free
     p LoadFroiErrors  e

      //---------------------------------------------------------------
      // programStart - Initialization
      //---------------------------------------------------------------
     p ProgramStart    b
     d programStart    pi
      /Free
       sys_Date = %date();
       sysDateCymd = %Dec(sys_Date:*cymd);
       If not %open(WXERP3);
          Open WXERP3;
       Endif;
      /End-Free
     p ProgramStart    e
