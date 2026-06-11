     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
      *=========================================================================
     H/TITLE -  Run SQL statements program to update employer number           *
      *=========================================================================
      *  PGM: FIX73451R                                                        *
      *                                                                        *
      *  DESCRIPTION: Run SQL statements to update employer number             *
      *                                                                        *
      *  CREATED:  10/09/2018  C:267 SUPPORT: 73451                            *
      *     NOTE:  WXMUP file does not exist for ERS client (065)              *
      *            Check for PGM name to ensure record would not be re-updated *
      *  10/25/18  Added claims file WMCMP                                     *
      *=========================================================================
      * Revisions:                                                             *
      *                                                                        *
      * 10/25/18 C:267 Support: 73451                                          *
      *                Added claims file WMCMP                                 *
      *                Added new procedure to check for file objects           *
      *=========================================================================
     FFIXEMPP   IF   E             DISK

     d CheckObjInLibl  pr                  ExtPgm('SRCHKOBJ')
     d  objName                      10    const
     d  objType                       7    const
     d  objNotinLibl                  1
      *
     D strobjTyp       s              7    inz('*FILE  ')
     D InFile          s             10
     D notAvail        s              1
     D decCount        s              3  0
     D tm6Current      s              6  0
     D dt7Current      s              7  0
     D i1              s              3  0
     D isDelete        s              1n   inz('0')
     D isDrop          s              1n   inz('1')
     D isFinalCopy     s              1n   inz('1')
     D intCONr         s              3  0
     D intGroup        s              3  0
     D intEmp          s              9  0
     D intNewEmp       s              9  0
     D strLib          s             10
     D strLibFr        s             10    inz
     D strLibTo        s             10    inz
     D strQtemp        s             10    inz('QTEMP')
     D strSqlCopy      s            500
     D strSqlCreate    s            500
     D strSqlDelete    s            500
     D strSqlDrop      s            500
     D strSqlUpdate    s            500
     D strSqlWhere     s            500
      *
     D #ofFiles        c                   59
     D arrFileInfo     s                   like(dsFileInfo)                     OPTIONS,FUNCTIONS
     D                                     dim(#ofFiles) ctdata perrcd(1)
     D dsFileInfo      ds                  qualified
     D  file                         10
     D  co#Field                     10
     D  grpField                     10
     D  empField                     10
     D  initUsFld                    10
     D  initPgFld                    10
     D  initTmFld                    10
     D  initDtFld                    10

     D*FIX73451R      uds                  dtaara(fix73451r)
     D* inLibFr                      10
     D* inLibTo                      10
     D* inDelete                      1n
     D* inDrop                        1n

     D psds          esds                  extname(szq1p)

     DGetFileLib       pr            10
     D inFile                        10

     D/copy sPrDate
     D/copy sPrString

       //------------------------------------------------------------------
      *entry

     DEmpNbrSQL        pr                  extpgm('FIX73451R')
     D inLibr                        10

     DEmpNbrSQL        pi
     D inLibr                        10

       //------------------------------------------------------------------
      /Free
       Exec sql set option COMMIT = *NONE;

       dt7Current = DateTo7(SystemDate);
       tm6Current = TimeTo6(SystemTime);
       //outError = *off;

       if inLibr <> *blanks;
          strLib = inLibr;
       Else;
          strLib = GetFileLib(dsFileInfo.file);
       EndIf;

       //setLL *loval fxm01;
       Read fxm01;
       Dow not %EOF;
          intCONr   = FXco#;
          intGroup  = FXfnd;
          intEmp    = FXemp#;
          intNewEmp = FXers#;

          For i1 = 1 to #ofFiles;
             dsFileInfo = arrFileInfo(i1);

             If dsFileInfo.file = *blank;
                Iter;
             Else;
                InFile = dsFileInfo.file;
             EndIf;

             CheckObjInLibl(InFile:strobjTyp:notAvail);
             If notAvail <> '1';

                strSqlUpdate = 'Update ' + %trim(strLib) + '/' +
                   %trim(dsFileInfo.file) + ' set '  +
                   %trim(dsFileInfo.empField) + ' = ' +
                      %editc(intNewEmp:'X');

                Exsr MaintFields;

                strSqlWhere = 'where ' + ' ' +
                  %trim(dsFileInfo.co#Field) + ' = ' + %editc(intCONr:'X') +
                  ' and ' + %trim(dsFileInfo.grpField) + ' = ' +
                  %editc(intGroup:'X') + ' and ' +
                  %trim(dsFileInfo.empField) + ' = ' + %editc(intEmp:'X') +
                  ' and ' + %trim(dsFileInfo.initPgFld) + ' <> ' +
                  '''' + q1pgm  + '''';

                strSqlUpdate = %trim(strSqlUpdate) + ' ' +
                               %trim(strSqlWhere);

                Exec sql execute immediate :strSqlUpdate;

                //Select;
                //When outError = *on;
                //   Leave;
                //Endsl;

             EndIf;

          Endfor;
          Read fxm01;
       EndDo;
       *inlr = *on;

       //==================================================================
       // Maintenance Fields
       //==================================================================
       Begsr MaintFields;
        If dsFileInfo.initDtFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initDtFld) + ' = ' +
            %editc(dt7Current:'X');
        Endif;
        If dsFileInfo.initTmFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initTmFld) + ' = ' +
            %editc(tm6Current:'X');
        Endif;
        If dsFileInfo.initUsFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initUsFld) + ' = ' +
            '''' + q1user + '''';
        Endif;
        If dsFileInfo.initPgFld <> *blank;
           strSqlUpdate = %trim(strSqlUpdate) +
            ', ' + %trim(dsFileInfo.initPgFld) + ' = ' +
            '''' + q1pgm  + '''';
        Endif;
       Endsr;
      /End-free
      //==================================================================
      // GetFileLib
      //==================================================================
     PGetFileLib       b
     DGetFileLib       pi            10
     D inFile                        10

     DRtvObjLib        pr                  extpgm('RTVOBJLIB')
     D outFile                       10    const
     D outType                        7    const
     D inLib                         10

     DstrLib           s             10
      /Free
       RtvObjLib(inFile:'*FILE':strLib);
       Return strLib;
      /End-free
     PGetFileLib       e
      //==================================================================
** File   CO#Fild   GrpField  EmpField  InitUsFld InitPgFld InitTmFld InitDtFld
WABLP     BLCO#     BLFND     BLEMP#    BLUSER    BLPGM     BLTIME    BLDT
WAOFP     OFCO#     OFFND     OFEMP#    OFUSER    OFPGM     OFTIME    OFDT
WAPPP     PPCO#     PPFND     PPEMP#    PPUSER    PPPGM     PPTIME    PPDT
WASRP     SRCO#     SRFND     SREMP#    SRUSER    SRPGM     SRTIME    SRDT
WAQTP     QTCO#     QTFND     QTEMP#    QTUSER    QTPGM     QTTIME    QTDT
WDAEP     AECO#     AEFND     AEEMP#    AEUSER    AEPGM     AETIME    AEDT
WDCXP     CXCO#     CXFND     CXEMP#    CXUSER    CXPGM     CXTIME    CXDT
WDDSP     DSCO#     DSFND     DSEMP#    DSUSER    DSPGM     DSTIME    DSDT
WDEAP     EACO#     EAFND     EAEMP#    EAUSER    EAPGM     EATIME    EADT
WDEHP     EHCO#     EHFND     EHEMP#    EHUSER    EHPGM     EHTIME    EHDT
WDELP     ELCO#     ELFND     ELEMP#    ELUSER    ELPGM     ELTIME    ELDT
WDILP     ILCO#     ILFND     ILEMP#    ILUSER    ILPGM     ILTIME    ILDT
WDLDP     LDCO#     LDFND     LDEMP#    LDUSER    LDPGM     LDTIME    LDDT
WDL2P     L2CO#     L2FND     L2EMP#    L2USER    L2PGM     L2TIME    L2DT
WDMPP     MPCO#     MPFND     MPEMP#    MPUSER    MPPGM     MPTIME    MPDT
WDNDP     NDCO#     NDFND     NDEMP#    NDUSER    NDPGM     NDTIME    NDDT
WDPPP     PPCO#     PPFND     PPEMP#    PPUSER    PPPGM     PPTIME    PPDT
WDRTP     RTCO#     RTFND     RTEMP#    RTUSER    RTPGM     RTTIME    RTDT
WDVHP     VHCO#     VHGRP     VHEMP#    VHUSER    VHPGM     VHTIME    VHDT
WDXHP     XHCO#     XHFND     XHEMP#    XHUSER    XHPGM     XHTIME    XHDT
WDXSP     XSCO#     XSFND     XSEMP#    XSUSER    XSPGM     XSTIME    XSDT
WMA#P     EACO#     EAFND     EAEMP#    EAUSER    EAPGM     EATIME    EADT
WMA3P     A3CO#     A3FND     A3EMP#    A3USER    A3PGM     A3TIME    A3DT
WMAHP     AHCO#     AHFND     AHEMP#    AHUSER    AHPGM     AHTIME    AHDT
WMALP     ALCO#     ALFND     ALEMP#    ALUSER    ALPGM     ALTIME    ALDT
WMANP     ANCO#     ANFND     ANEMP#    ANUSER    ANPGM     ANTIME    ANDT
WMAUP     AUCO#     AUFND     AUEMP#    AUUSER    AUPGM     AUTIME    AUDT
WMCEP     CECO#     CEFND     CEEMP#    CEUSER    CEPGM     CETIME    CEDT
WMDBP     DBCO#     DBFND     DBEMP#    DBUSER    DBPGM     DBTIME    DBDT
WMDPP     DPCO#     DPFND     DPEMP#    DPUSER    DPPGM     DPTIME    DPDT
WMEAP     EACO#     EAFND     EAEMP#    EAUSER    EAPGM     EATIME    EADT
WMECP     ECCO#     ECFND     ECEMP#    ECUSER    ECPGM     ECTIME    ECDT
WMEEP     EECO#     EEFND     EEEMP#    EEUSER    EEPGM     EETIME    EEDT
WMEFP     EFCO#     EFFND     EFEMP#    EFUSER    EFPGM     EFTIME    EFDT
WMENP     ENCO#     ENFND     ENEMP#    ENUSER    ENPGM     ENTIME    ENDT
WMEMP     EMCO#     EMFND     EMEMP#    EMUSER    EMPGM     EMTIME    EMDT
WMEPP     EPCO#     EPFND     EPEMP#    EPUSER    EPPGM     EPTIME    EPDT
WMEWP     EWCO#     EWFND     EWEMP#    EWUSER    EWPGM     EWTIME    EWDT
WMLDP     LDCO#     LDFND     LDEMP#    LDUSER    LDPGM     LDTIME    LDDT
WMMPP     MPCO#     MPFND     MPEMP#    MPUSR     MPPGM     MPTIM     MPDT
WMMUP     MUCO#     MUFND     MUEMP#    MUUSER    MUPGM     MUTIME    MUDT
WMNBP     NBCO#     NBFND     NBEMP#    NBUSER    NBPGM     NBTIME    NBDT
WMNEP     NECO#     NEFND     NEEMP#    NEUSER    NEPGM     NETIME    NEDT
WMOOP     OOCO#     OOFND     OOEMP#    OOUSER    OOPGM     OOTIME    OODT
WMRPP     RPCO#     RPFND     RPEMP#    RPUSER    RPPGM     RPTIME    RPDT
WMSHP     SHCO#     SHFND     SHEMP#    SHUSER    SHPGM     SHTIME    SHDT
WMWVP     WVCO#     WVFND     WVEMP#    WVUSER    WVPGM     WVTIME    WVDT
WMXAP     XACO#     XAFND     XAEMP#    XAUSER    XAPGM     XATIME    XADT
WXELP     ELCO#     ELFND     ELEMP#    ELUSER    ELPGM     ELTIME    ELDT
WXFMP     FMCO#     FMFND     FMEMP#    FMUSER    FMPGM     FMTIME    FMDT
WXFMP1    F3CO#     F3FND     F3EMP#    F3USER    F3PGM     F3TIME    F3DT
WXFMP2    F4CO#     F4FND     F4EMP#    F4USER    F4PGM     F4TIME    F4DT
WXNAP     NACO#     NAFND     NAEMP#    NAUSER    NAPGM     NATIME    NADT
WXNAP1    NACO#     NAFND     NAEMP#    NAUSER    NAPGM     NATIME    NADT
WXNEP     NECO#     NEFND     NEEMP#    NEUSER    NEPGM     NETIME    NEDT
WXNHP     NHCO#     NHFND     NHEMP#    NHUSER    NHPGM     NHTIME    NHDT
WXNXP     NXCO#     NXFND     NXEMP#    NXUSER    NXPGM     NXTIME    NXDT
WZEXP     EXCO#     EXFND     EXEMP#    EXUSER    EXPGM     EXTIME    EXDT
WMCMP     CMCO#     CMFND     CMEMP#    CMUSER    CMPGM     CMTIME    CMDT
