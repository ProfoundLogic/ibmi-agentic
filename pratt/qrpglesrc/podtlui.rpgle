       CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
       CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);

       // *ENTRY Prototype
       // ----------------------------------------------------------------------
       DCL-PI *N;
         @EOJ       CHAR(1);
         @RET       ZONED(5); // Current record #, 0 exits, >1 shows pager
         @SIZ       ZONED(5); // Total # of records
         @OHYY      CHAR(2);
         @OH#       ZONED(6); // 0 = New PO Mode
       END-PI;

       DCL-PR PRHDTLUI EXTPGM;
         @EOJ       CHAR(1);
         @RET       ZONED(5); // Current record #
         @SIZ       ZONED(5) CONST; // Total # of records
         @ORYY      CHAR(2) CONST;
         @OR#       ZONED(6) CONST;
       END-PR;
       DCL-PR RTVPOTOT EXTPGM;
         @OHYY      CHAR(2) CONST;
         @OH#       ZONED(6) CONST;
         @POTOT     PACKED(22:2);
         @POMAX     PACKED(22:2);
         @LIMIT     IND;
       END-PR;
       DCL-PR USRRTV EXTPGM;
         @USER      CHAR(10) CONST;
         @PGM       CHAR(10) CONST;
         @LVL       CHAR(10);
       END-PR;
       DCL-PR RTVPOCOST EXTPGM;
         @OHYY     CHAR(2) CONST;
         @OH#      ZONED(6) CONST;
         @OILIN#   ZONED(3) CONST;
         @OIPART   CHAR(30) CONST;
         @OIUNIT   PACKED(15:5);
       END-PR;
       DCL-PR RTVSAPACCT EXTPGM;
         @DC        CHAR(2) CONST;
         @PART      CHAR(30) CONST;
         SAPPURG    ZONED(3); // PURCHASING GROUP
         SAPMTLG    CHAR(7);  // MATERIAL GROUP
         SAPMTLN    CHAR(9); // MATERIAL NUMBER
         SAPCSTC    CHAR(7); // COST CENTER
         SAPGLACT   ZONED(6); // GL ACCOUNT
       END-PR;
       DCL-PR LOCKMSG EXTPGM;
         @STATUS    LIKEDS(STATUSS);
       END-PR;
       DCL-PR CXOHPP EXTPGM;
         @OHYY      CHAR(2);
         @OHPP      CHAR(2);
         @OH#       PACKED(6);
         @TOPP      CHAR(2);
       END-PR;
       //DCL-PR APPOAPVCL EXTPGM;
       //  @SEND      CHAR(1);
       //  @USER      CHAR(10);
       //END-PR;
       DCL-PR APPOPRTCL EXTPGM;
         @OHYY      CHAR(2);
         @OHPP      CHAR(2);
         @OH#       PACKED(6);
         @OHDC      CHAR(2);
         @OHMETH    CHAR(1);
         @SEND      CHAR(1);
         @OHSTAT    CHAR(1);
       END-PR;
       DCL-PR WHSETCL EXTPGM;
         @WH        CHAR(2);
       END-PR;
       DCL-PR OILEN EXTPGM;
         @OHYY      CHAR(2);
         @OHPP      CHAR(2);
         @OH#       PACKED(6);
         @LEN       ZONED(2);
       END-PR;
       //DCL-PR PABYALTCL EXTPGM;
       //  @VYDEL     CHAR(1);
       //  @VYVEND    ZONED(6);
       //END-PR;
       DCL-PR SAPPOINT EXTPGM;
         @OHYY      CHAR(2);
         @OHPP      CHAR(2);
         @OH#       ZONED(6);
       END-PR;
       DCL-PR OHMGRCLO EXTPGM;
         @UPDAT     CHAR(1) CONST;
       END-PR;
       DCL-PR OMMGRCL EXTPGM('OMMGRCLO');
         @UPDAT     CHAR(1) CONST;
       END-PR;
       DCL-PR VNDMGRCL EXTPGM;
         @UPDATE    CHAR(1) CONST;
       END-PR;
       DCL-PR MGRCL EXTPGM;
         @MGR       CHAR(6) CONST;
       END-PR;
       DCL-PR VPMGRCL EXTPGM;
         @UPDAT     CHAR(1) CONST;
       END-PR;
       DCL-PR WHMGRCL EXTPGM('WHMGRCLO');
         @W         CHAR(2) CONST;
         @UPDAT     CHAR(1) CONST;
         @SORP      CHAR(1) CONST;
       END-PR;
       DCL-PR WHSELUI EXTPGM;
         @EOJ       CHAR(1);
         @LOAD      CHAR(1) CONST;
         @WH        CHAR(2);
       END-PR;
       DCL-PR WPMGRCL EXTPGM('WPMGRCLO');
         @WP        CHAR(2) CONST;
         @UPDAT     CHAR(1) CONST;
         @CHORA     CHAR(1) CONST;
         @SORP      CHAR(1) CONST;
         @LVL       CHAR(1) CONST;
       END-PR;

       // File Declarations
       // ----------------------------------------------------------------------
       DCL-F PODTLUI WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
         SFILE(SFLFMT:SFLR#);

       DCL-F ORHDRL4    KEYED USAGE(*UPDATE:*OUTPUT); // OHFMT
       DCL-F ORHTX      KEYED USAGE(*UPDATE:*OUTPUT); // OTFMT
       DCL-F ORITML5    KEYED USAGE(*UPDATE:*OUTPUT); // OIFMT
       DCL-F ORITX      KEYED USAGE(*UPDATE:*OUTPUT); // OITFMT
       DCL-F VENDPART   KEYED USAGE(*UPDATE:*OUTPUT); // VRFMT
       DCL-F VENDPACO   KEYED USAGE(*UPDATE:*OUTPUT); // VOFMT
       DCL-F VENDPABL6  KEYED USAGE(*UPDATE:*OUTPUT); // VYFMT
       DCL-F MASTSPEP   KEYED USAGE(*UPDATE:*OUTPUT); // MQFMT
       DCL-F WIPJOBS    KEYED USAGE(*UPDATE:*OUTPUT); // WPFMT
       DCL-F WIPSHIP    KEYED USAGE(*UPDATE:*OUTPUT); // WSFMT
       DCL-F COADDRES   KEYED USAGE(*UPDATE); // COAFMT
       DCL-F ORMSRR     KEYED USAGE(*UPDATE); // OMFMT;
       DCL-F ORECT      KEYED; // ORFMT
       DCL-F VENDMST    KEYED; // VENDFMT
       DCL-F SAPINCO    KEYED; // INCOFMT
       DCL-F WAREHOUS   KEYED; // WHFMT
       DCL-F MASTSAZZ   KEYED; // QZZFMT
       DCL-F CUSTSHPP   KEYED; // CSFMT
       DCL-F VENDSAZZ   KEYED; // VZZFMT
       DCL-F USERIDS    KEYED; // UIFMT
       DCL-F SAPPOHDCP  KEYED; // SAPHFMT
       DCL-F LOCXREF    KEYED; // L0FMT
       DCL-F ORECOMM    KEYED RENAME(ORFMT:ECFMT) PREFIX(EC:2);
       DCL-F LOCXREFLF  KEYED RENAME(L0FMT:INTEDI);
       DCL-F WIPJOBH    KEYED RENAME(WPFMT:WPHIST);
       DCL-F ZCCITY     KEYED
                        RENAME(ZCITYR:ZCFMT);

       // Data Declarations
       // ----------------------------------------------------------------------
       // Data ON the screen including pending updates
       DCL-DS OHDS      EXTNAME('ORHDRL4':'OHFMT':*ALL) INZ;
       END-DS;
       DCL-DS OTDS      EXTNAME('ORHTX':'OTFMT':*ALL) INZ;
       END-DS;
       DCL-DS OIDS      EXTNAME('ORITML5':'OIFMT':*ALL) INZ;
       END-DS;
       DCL-DS OITDS     EXTNAME('ORITX':'OITFMT':*ALL) INZ;
         OITXC     CHAR(560) POS(48);
         OITXT     CHAR(70) DIM(8) POS(48);
         OITXTUC   CHAR(8) POS(631);
         OITXTU    CHAR(1) DIM(8) POS(631);
       END-DS;
       // Snapshot record(s) from screen load QUALIFIED
       DCL-DS OHSAV     LIKEREC(OHFMT) INZ;
       DCL-DS OTSAV     LIKEREC(OTFMT) INZ;
       DCL-DS OISAV     LIKEREC(OIFMT) DIM(*AUTO:999) INZ;
       DCL-DS OITSAV    LIKEREC(OITFMT) DIM(*AUTO:999) INZ;
       // For new records - save old parameters in case of cancel
       DCL-S @OHYYSAV   LIKE(@OHYY);
       DCL-S @OH#SAV    LIKE(@OH#);
       DCL-S @RETSAV    LIKE(@RET);
       DCL-S @SIZSAV    LIKE(@SIZ);

       // ...DIS is "disabled" property
       // ...VIS is "visibility" property
       // ...DSC is a text field "description"
       // ...UPD is toggling a field between read only and "changeable"
       // ...TT is the tooltip for the field
       // Buttons to disable in update mode
       DCL-DS CTLUPDDS INZ;
         CTLNEWDIS  IND;
         CTLRCPTDIS IND;
         CTLPRHDIS  IND;
         CTLSENDDIS IND;
         CTLWIPDIS  IND;
         CTLUPDDIS  IND;
         CTLADDDIS  IND;
         CTLCOPYDIS IND;
         CTLHIDEDIS IND;
         CTLBACKDIS IND;
         CTLEXITDIS IND;
         CTLVNDDIS  IND;
         CTLCSDIS   IND;
       END-DS;
       // Header Disable fields by user level
       // Every disable capable field should be in one of these
       // CTLDIS0 = Always Enabled (only disabled by override)
       // CTLDIS1 = Disable for level 1 (NOV)
       // CTLDIS2 = Disable for levels 1-2 (NOV, INT)
       // CTLDIS3 = Disable for levels 1-3 (NOV, INT, EXP)
       // CTLDIS9 = Always Disabled (only enabled by override)
       DCL-DS CTLDIS0 INZ;
         OHSATNDIS  IND;
         OHTXT1DIS  IND;
       END-DS;
       DCL-DS CTLDIS1 INZ;
         OHSTAT_DIS IND;
         OHSTATCDIS IND;
         OHSTATHDIS IND;
         OHSTATXDIS IND;
       END-DS;
       DCL-DS CTLDIS2 INZ;
         OHVENDDIS  IND;
         OHFRTDIS   IND;
         OHDCDIS    IND;
         OHNNDIS    IND;
         OHCUIDDIS  IND;
         OHLOCDIS   IND;
         OHSHIPDIS  IND;
         OHSADRDIS  IND;
         OHSCTYDIS  IND;
         OHSSTDIS   IND;
         OHSZP5DIS  IND;
         OHSZP4DIS  IND;
         OHSTCTRDIS IND;
         OHSTPCDDIS IND;
       END-DS;
       DCL-DS CTLDIS3 INZ;
         OHCODEDIS  IND;
       END-DS;
       DCL-DS CTLDIS9 INZ;
         OHPPDIS    IND; // Only enabled for New POs
       END-DS;
       // All customizable tooltips in CTLFMT
       DCL-DS CTLTT INZ;
         CTLNEWTT   CHAR(100);
         CTLADDTT   CHAR(100) INZ('Add Line Item');
         CTLUPDTT   CHAR(100) INZ('Update Header');
         CTLCOPYTT  CHAR(100);
         CTLSENDTT  CHAR(100) INZ('Approve/Send/Print PO');
         CTLWIPTT   CHAR(100) INZ('Create WIP Jobs for LTM');
         OHCODETT   CHAR(100);
         OHSTAT_TT  CHAR(100);
         OHSTATHTT  CHAR(100);
         OHSTATCTT  CHAR(100);
         OHSTATXTT  CHAR(100);
         OHPPTT     CHAR(100) INZ('Business Area');
         OHVENDTT   CHAR(100);
         OHFRTTT    CHAR(100);
       END-DS;
       // All the error flags in CTLFMT
       DCL-DS CTLERR INZ;
         OHPPERR    IND;
         OHSTATERR  IND;
         OHCODEERR  IND;
         OHVENDERR  IND;
         POTOTERR   IND;
         OHFRTERR   IND;
         OHDCERR    IND;
         OHSATNERR  IND;
         OHNNERR    IND;
         OHCUIDERR  IND;
         OHLOCERR   IND;
         OHSHIPERR  IND;
         OHSADRERR  IND;
         OHSCTYERR  IND;
         OHSSTERR   IND;
         OHSZP5ERR  IND;
         OHSZP4ERR  IND;
         OHSTCTRERR IND;
         OHSTPCDERR IND;
         OHTXT1ERR  IND;
       END-DS;
       // All the error messages in CTLFMT
       DCL-DS CTLERM INZ;
         OHPPERM    CHAR(100);
         OHSTATERM  CHAR(100);
         OHCODEERM  CHAR(100);
         OHVENDERM  CHAR(100);
         POTOTERM   CHAR(100);
         OHFRTERM   CHAR(100);
         OHDCERM    CHAR(100);
         OHSATNERM  CHAR(100);
         OHNNERM    CHAR(100);
         OHCUIDERM  CHAR(100);
         OHLOCERM   CHAR(100);
         OHSHIPERM  CHAR(100);
         OHSADRERM  CHAR(100);
         OHSCTYERM  CHAR(100);
         OHSSTERM   CHAR(100);
         OHSZP5ERM  CHAR(100);
         OHSZP4ERM  CHAR(100);
         OHSTCTRERM CHAR(100);
         OHSTPCDERM CHAR(100);
         OHTXT1ERM  CHAR(100);
       END-DS;
       // All the error formats in CTLFMT
       DCL-C ERFERROR 'pui-tip-error';
       DCL-C ERFINFO 'pui-tip-info';
       DCL-DS CTLERF INZ;
         OHPPERF    CHAR(15) INZ(ERFERROR);
         OHSTATERF  CHAR(15) INZ(ERFERROR);
         OHCODEERF  CHAR(15) INZ(ERFERROR);
         OHVENDERF  CHAR(15) INZ(ERFERROR);
         POTOTERF   CHAR(15) INZ(ERFERROR);
         OHFRTERF   CHAR(15) INZ(ERFERROR);
         OHDCERF    CHAR(15) INZ(ERFERROR);
         OHSATNERF  CHAR(15) INZ(ERFERROR);
         OHNNERF    CHAR(15) INZ(ERFERROR);
         OHCUIDERF  CHAR(15) INZ(ERFERROR);
         OHLOCERF   CHAR(15) INZ(ERFERROR);
         OHSHIPERF  CHAR(15) INZ(ERFERROR);
         OHSADRERF  CHAR(15) INZ(ERFERROR);
         OHSCTYERF  CHAR(15) INZ(ERFERROR);
         OHSSTERF   CHAR(15) INZ(ERFERROR);
         OHSZP5ERF  CHAR(15) INZ(ERFERROR);
         OHSZP4ERF  CHAR(15) INZ(ERFERROR);
         OHSTCTRERF CHAR(15) INZ(ERFERROR);
         OHSTPCDERF CHAR(15) INZ(ERFERROR);
         OHTXT1ERF  CHAR(15) INZ(ERFERROR);
       END-DS;
       // All the "changed" flags - header
       DCL-DS OHFMTCHG INZ;
         OHPPCHG    IND;
         OHSTATCHG  IND;
         OHCODECHG  IND;
         OHVENDCHG  IND;
         OHFRTCHG   IND;
         OHDCCHG    IND;
         OHSATNCHG  IND;
         OHNNCHG    IND;
         OHCUIDCHG  IND;
         OHLOCCHG   IND;
         OHSHIPCHG  IND;
         OHSADRCHG  IND;
         OHSCTYCHG  IND;
         OHSSTCHG   IND;
         OHSZP5CHG  IND;
         OHSZP4CHG  IND;
         OHSTCTRCHG IND;
         OHSTPCDCHG IND;
       END-DS;
       DCL-DS OTFMTCHG INZ;
         OHTXT1CHG  IND;
       END-DS;

       // Buttons to disable in update mode (header or line)
       DCL-DS SFLUPDDS INZ;
         SFLUPDDIS  IND;
         SFLNOTEDIS IND;
         SFLVPDIS   IND;
         SFLWHDIS   IND;
       END-DS;
       // Subfile Disable fields by user level
       // Every disable capable field should be in one of these
       // SFLDIS0 = Always Enabled (only disabled by override)
       // SFLDIS1 = Disable for level 1 (NOV)
       // SFLDIS2 = Disable for levels 1-2 (NOV, INT)
       // SFLDIS3 = Disable for levels 1-3 (NOV, INT, EXP)
       // SFLDIS9 = Always Disabled (only enabled by override)
       DCL-DS SFLDIS0 INZ;
         OITXTDIS   IND;
         OIPARTDIS  IND;
         OIDESCDIS  IND;
         OIQTYDIS   IND;
         OIUOMDIS   IND;
         OIDDATDIS  IND;
         OIUNITDIS  IND;
       END-DS;
       DCL-DS SFLDIS1 INZ;
         OISTAT_DIS IND;
         OISTATCDIS IND;
         OISTATXDIS IND;
       END-DS;
       DCL-DS SFLDIS2 INZ;
       END-DS;
       DCL-DS SFLDIS3 INZ;
         OILIN#DIS  IND;
       END-DS;
       DCL-DS SFLDIS9 INZ;
       END-DS;
       // All customizable tooltips in SFLFMT
       DCL-DS SFLTT INZ;
         SFLUPDTT   CHAR(100) INZ('Update Line');
         OISTAT_TT  CHAR(100) INZ;
         OISTATCTT  CHAR(100) INZ;
         OISTATXTT  CHAR(100) INZ;
         OIPARTTT   CHAR(100) INZ;
         OIDESCTT   CHAR(100) INZ;
         OIQTYTT    CHAR(100) INZ;
         OIUOMTT    CHAR(100) INZ;
         OIUNITTT   CHAR(100) INZ;
         OIDDATTT   CHAR(100) INZ;
       END-DS;
       // All the error flags in SFLFMT
       DCL-DS SFLERR INZ;
         OILIN#ERR  IND;
         OIQTYERR   IND;
         OIPARTERR  IND;
         OIDESCERR  IND;
         OIDDATERR  IND;
         OIUNITERR  IND;
         OIUOMERR   IND;
         OICSTCERR  IND;
         OIGLACTERR IND;
       END-DS;
       // All the error messages in SFLFMT
       DCL-DS SFLERM INZ;
         OILIN#ERM  CHAR(100);
         OIQTYERM   CHAR(100);
         OIPARTERM  CHAR(100);
         OIDESCERM  CHAR(100);
         OIDDATERM  CHAR(100);
         OIUNITERM  CHAR(100);
         OIUOMERM   CHAR(100);
         OICSTCERM  CHAR(100);
         OIGLACTERM CHAR(100);
       END-DS;
       // All the error formats in SFLFMT
       DCL-DS SFLERF INZ;
         OILIN#ERF  CHAR(15) INZ(ERFERROR);
         OIQTYERF   CHAR(15) INZ(ERFERROR);
         OIPARTERF  CHAR(15) INZ(ERFERROR);
         OIDESCERF  CHAR(15) INZ(ERFERROR);
         OIDDATERF  CHAR(15) INZ(ERFERROR);
         OIUNITERF  CHAR(15) INZ(ERFERROR);
         OIUOMERF   CHAR(15) INZ(ERFERROR);
         OICSTCERF  CHAR(15) INZ(ERFERROR);
         OIGLACTERF CHAR(15) INZ(ERFERROR);
       END-DS;
       // All the "changed" flags - line
       DCL-DS OIFMTCHG INZ;
         OILIN#CHG  IND;
         OIQTYCHG   IND;
         OIPARTCHG  IND;
         OIDESCCHG  IND;
         OISTATCHG  IND;
         OIDDATCHG  IND;
         OIUNITCHG  IND;
         OIUOMCHG   IND;
       END-DS;
       DCL-DS OITFMTCHG INZ;
         OITXTU1CHG IND;
         OITXT1CHG  IND;
         OITXTU2CHG IND;
         OITXT2CHG  IND;
         OITXTU3CHG IND;
         OITXT3CHG  IND;
         OITXTU4CHG IND;
         OITXT4CHG  IND;
         OITXTU5CHG IND;
         OITXT5CHG  IND;
         OITXTU6CHG IND;
         OITXT6CHG  IND;
         OITXTU7CHG IND;
         OITXT7CHG  IND;
         OITXTU8CHG IND;
         OITXT8CHG  IND;
       END-DS;

       // Generic fields for program operation
       DCL-S SFLSIZ     LIKE(SFLR#);
       DCL-S VFYERR     IND; // Fatal error on verification, resets with Verify SR
       DCL-S SAVERR     IND; // Fatal error on save, resets with save button
       DCL-S USER       CHAR(10) INZ(*USER);
       DCL-S USRLVL     CHAR(10); // NOV, INT, EXP, PGMR...
       DCL-S USRMODE    PACKED(1); // Number version of above
       DCL-S CMD        VARCHAR(50);
       DCL-S NEWREC     IND; // NEW record created
       DCL-S NEWLIN     IND; // NEW line created
       DCL-S HIDEMSG    IND; // Don't run Verify SRs

       // Fields From OHMGR/OIMGR
       DCL-DS ZIPCITYDS;
         ZCODE      CHAR(5);
         STATE      CHAR(2);
         CITY       CHAR(28);
         PFST       CHAR(2);
         PFCITY     CHAR(28);
         CITY20     CHAR(20);
       END-DS;
       DCL-DS VZZDS      EXTNAME('VENDSAZZ');
         VZZ        CHAR(15) DIM(26) POS(21);
       END-DS;
       DCL-S VZZVAL     LIKE(VZZ1);
       DCL-S SVDDAT     LIKE(OIDDAT);
       DCL-DS STATUSS    PSDS LEN(333);
         @PROC      *PROC;
         @CPF#      CHAR(7) POS(40);
         @CPFMSG    CHAR(80) POS(91);
         @USER      CHAR(10) POS(254);
       END-DS;
       DCL-DS VRDS       EXTNAME('VENDPART':'VRFMT':*ALL) INZ;
         MAJMIN     CHAR(6) POS(103);
         UOM        CHAR(4) POS(310) DIM(5) ASCEND;
         QTY        PACKED(7) POS(372) DIM(5) ASCEND;
         CST        PACKED(11:2) POS(392) DIM(5) ASCEND;
         VRF        PACKED(11:2) POS(437) DIM(5) ASCEND;
         VRA        PACKED(11:2) POS(467) DIM(5) ASCEND;
       END-DS;
       DCL-DS VODS       EXTNAME('VENDPACO':'VOFMT':*ALL) INZ;
         VOQ        PACKED(7) POS(68) DIM(5) ASCEND;
         VOC        PACKED(11:2) POS(88) DIM(5);
         VOF        PACKED(11:2) POS(133) DIM(5);
         VOA        PACKED(11:2) POS(163) DIM(5);
       END-DS;
       DCL-DS VYDS       EXTNAME('VENDPABL6':'VYFMT':*ALL) OCCURS(2) INZ;
         VYXT       CHAR(50) POS(152) DIM(4);
         VYQ        PACKED(7) POS(411) DIM(5) ASCEND;
         VYC        PACKED(11:2) POS(431) DIM(5);
         VYF        PACKED(11:2) POS(476) DIM(5);
         VYA        PACKED(11:2) POS(506) DIM(5);
       END-DS;

       DCL-S POMAX      PACKED(22:2);
       DCL-S POMAXA     PACKED(22:2) INZ(200000);
       DCL-S POLIMIT    IND;
       DCL-S SVPOTOT    LIKE(POTOT);
       DCL-S FRTLIN     IND; // Current line is freight line
       DCL-S HASFRT     IND; // PO has ANY valid freight line
       DCL-S @WH        CHAR(2);
       DCL-S WIPREC     IND;
       // Dummy @RET value to turn off paging down stream (like PRHDTLUI)
       DCL-S @NORET     LIKE(@RET) INZ(1);
       DCL-C CSFRGHT 'CSFREIGHTCHARGES';
       DCL-C CSFRFSC 'CSFREIGHTFSC';
       DCL-C ESFRGHT 'ESFREIGHT';
       DCL-C CSTARIF 'CSTARIFF';
       DCL-C CDSALES 'CSCDSALES';
       DCL-C PPSALES 'CSPPSALES';
       DCL-C ESPROCS 'ESPROCESSINGFEE';
       DCL-C CSRUSH 'CSRUSHFEE';


       // ------------------------------------------------------------------- //
       // *INZSR equivalent before DO loop
       // Retrieve user update level
       USRRTV(USER:'OHMGR':USRLVL);
       SELECT;
       WHEN USRLVL = 'NOV';
         USRMODE = 1;
       WHEN USRLVL = 'INT';
         USRMODE = 2;
       WHEN USRLVL = 'EXP';
         USRMODE = 3;
       WHEN USRLVL = 'PGMR';
         USRMODE = 4;
       OTHER;
         USRMODE = 0;
       ENDSL;
       // get user name info for UISAP
       CHAIN USER UIFMT;
       // Load UofM translation tables
       VZZKEY = 'BASEUOFM';
       CHAIN VZZKEY VZZFMT;
       IF @OH# = 0;
         NEWREC = *ON;
       ENDIF;

       DOU *INLR = *ON;
         IF @EOJ = 'Y'; // (Keep this first) Exit button from downstream
           *INLR = *ON;
           LEAVE;
         ENDIF;
         // Before screen write //
         IF NEWREC = *ON;
           NewRecord();
         ENDIF;
         IF @SIZ > 1;
           CTLCTR = %CHAR(@RET) + '/' + %CHAR(@SIZ);
           CTLPREVDIS = *OFF;
           CTLNEXTDIS = *OFF;
         ELSE;
           CTLCTR = *BLANKS;
           CTLPREVDIS = *ON;
           CTLNEXTDIS = *ON;
         ENDIF;
         LoadRecord();
         SetAllowedModes();
         IF HIDEMSG = *OFF;
           CTLHIDEVAL = 'Hide Msgs (F1)';
           CTLHIDEICO = 'material:do_not_disturb_on';
           CTLHIDEDIS = *ON;
           VerifyRecord();
           IF CTLERR <> *ZEROS;
             CTLHIDEDIS = *OFF;
           ENDIF;
         ELSE;
           CTLHIDEVAL = 'Show Msgs (F1)';
           CTLHIDEICO = 'material:do_not_disturb_off';
         ENDIF;
         LoadLines(); // CTLHIDEDIS also in here!
         IF NEWLIN = *ON;
           NewLine();
         ENDIF;

         // Screen write & read, or branch to update //
         SELECT;
         WHEN CTLUPDBTN = *ON OR NEWREC = *ON;
           CTLUPD = *ON;
           UpdateMode();
           NEWREC = *OFF;
           CTLUPD = *OFF;
         WHEN SFLUPDR# > 0 OR NEWLIN = *ON;
           LineUpdateMode();
           NEWLIN = *OFF;
           SFLUPDR# = 0;
         OTHER;
           EXFMT CTLFMT;
         ENDSL;

         // After screen read //
         SELECT;
         WHEN CTLBACK = *ON;
           @RET = 0;
           *INLR = *ON;
         WHEN CTLEXIT = *ON;
           @EOJ = 'Y';
           @RET = 0;
           *INLR = *ON;
         WHEN CTLPREV = *ON;
           IF @RET = 1;
             @RET = @SIZ;
           ELSE;
             @RET -= 1;
           ENDIF;
           *INLR = *ON;
         WHEN CTLNEXT = *ON;
           IF @RET = @SIZ;
             @RET = 1;
           ELSE;
             @RET += 1;
           ENDIF;
           *INLR = *ON;
         WHEN CTLNEW = *ON;
           NEWREC = *ON;
         WHEN CTLADD = *ON;
           NEWLIN = *ON;
         WHEN CTLRCPT = *ON;
           OHMGRCLO(' ');
         WHEN CTLPRH = *ON;
           PRHDTLUI(@EOJ:@NORET:1:OHYY:OH#);
         WHEN CTLUPD = *ON;
           ITER;
         WHEN CTLCOPY = *ON;
           SAVERR = *OFF;
           CheckModifiedRecord();
           CheckModifiedLines();
           IF SAVERR = *ON;
             CTLFMTERR = *ON;
             CTLFMTERM = 'PO was changed since it was opened! ' +
                         'Copy failed!';
             ITER;
           ENDIF;
           CopyPO();
           IF SAVERR = *ON;
             CTLFMTERR = *ON;
             CTLFMTERM = 'Error copying PO!';
             ITER;
           ENDIF;
         WHEN CTLSEND = *ON;
           SAVERR = *OFF;
           CheckModifiedRecord();
           CheckModifiedLines();
           IF SAVERR = *ON;
             CTLFMTERR = *ON;
             CTLFMTERM = 'PO was changed since it was opened! ' +
                         'Send failed!';
             ITER;
           ENDIF;
           SendPO();
         WHEN CTLWIP = *ON;
           SAVERR = *OFF;
           CheckModifiedRecord();
           CheckModifiedLines();
           IF SAVERR = *ON;
             CTLFMTERR = *ON;
             CTLFMTERM = 'PO was changed since it was opened! ' +
                         'WIP Create failed!';
             ITER;
           ENDIF;
           PO2WIP();
         WHEN CTLVNDMGR = *ON;
           VNDMGRCL(' ');
         WHEN CTLCSMGR = *ON;
           MGRCL('CUMGR');
         WHEN CTLHIDE = *ON;
           IF HIDEMSG = *OFF;
             HIDEMSG = *ON;
           ELSE;
             HIDEMSG = *OFF;
           ENDIF;
         OTHER;
           HIDEMSG = *OFF; // Turn messages back on when screen reloads
           ReadSubfile();
         ENDSL;
       ENDDO;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC LoadRecord;
         CHAIN(N) (@OHYY:@OH#) OHFMT;
         CLEAR *ALL OTFMT;
         CHAIN(N) (OHYY:OHPP:OH#:*BLANKS) OTFMT;
         // Save records for comparison in UPDATE
         OHSAV = OHDS;
         OTSAV = OTDS;
         // Get related records
         CLEAR ECFMT;
         IF OHCODE = 'E';
           CHAIN (OHORDID) ECFMT;
         ENDIF;
         CLEAR VENDFMT;
         CHAIN (OHVEND) VENDFMT;
         CLEAR INCOFMT;
         CHAIN (OHFRT) INCOFMT;
         CLEAR QZZFMT;
         CHAIN ('COUNTRY' + OHSTCTRY) QZZFMT;
         CLEAR OMFMT;
         CHAIN(N) (OIYY: OIREQ) OMFMT;
         // Custom fields
         RTVPOTOT(OHYY:OH#:POTOT:POMAX:POLIMIT);
         SVPOTOT = POTOT; // Save for comparison
         PONUM = 'PR' + OHYY + %CHAR(OH#);
         SELECT;
         WHEN OHSTAT = ' ';
           OHSTATDSC = 'Open';
         WHEN OHSTAT = 'C';
           OHSTATDSC = 'Closed';
         WHEN OHSTAT = 'H';
           OHSTATDSC = 'On Hold';
         WHEN OHSTAT = 'X';
           OHSTATDSC = 'Cancelled';
         ENDSL;
         SELECT;
         WHEN OHMETH = 'Y';
           OHMETHDSC = 'YES';
         WHEN OHMETH = 'N';
           OHMETHDSC = 'NO';
         WHEN OHMETH = 'A';
           OHMETHDSC = 'APPROVED';
         ENDSL;
         IF %FOUND(MASTSAZZ);
           CTRYNM = QZZDTA;
         ELSE;
           CTRYNM = *BLANKS;
         ENDIF;
         // Show/hide fields on screen
         IF OHCODE = 'E' AND %FOUND(ORECOMM);
           CTLECVIS = *ON;
         ENDIF;
         IF (OHCODE = 'D' OR OHDC='MS' OR OHDC='DS');
           CTLDSVIS = *ON;
           CTLCSVIS = *ON;
         ELSE;
           CTLDSVIS = *OFF;
           CTLCSVIS = *OFF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       // Reset to Detail mode, check permissions based on data and user level//
       DCL-PROC SetAllowedModes;
         DCL-S OIRUSRLVL  CHAR(10); // User level for receipts

         RESET CTLTT;
         // Set Record DETAIL Mode
         CTLHDR = 'Purchase Order Detail';
         CTLHDRCOL = '';
         CTLUPDDS = *OFF;
         CTLCNCLUPD = *OFF;
         CTLSAVUPD = *OFF;
         CTLCNCLDIS = *ON;
         CTLSAVDIS = *ON;

         CTLDIS0 = *ON;
         CTLDIS1 = *ON;
         CTLDIS2 = *ON;
         CTLDIS3 = *ON;
         CTLDIS9 = *ON;

         // Custom DETAIL fields
         // Toggles status text/buttons
         OHSTATUPD = *OFF;

         //--Update Mode Check--//
         // No update or add if no perms
         IF USRMODE < 1;
           CTLNEWDIS = *ON;
           CTLNEWTT = 'You do not have the authority to create POs';
           CTLUPDDIS = *ON;
           CTLUPDTT = 'You do not have the authority to change POs';
           CTLADDDIS = *ON;
           CTLADDTT = 'You do not have the authority to change POs';
           CTLCOPYDIS = *ON;
           CTLCOPYTT = 'You do not have the authority to copy POs';
         ENDIF;
         IF USRMODE < 2;
           CTLSENDDIS = *ON;
           CTLSENDTT = 'You do not have the authority to approve POs';
           CTLWIPDIS = *ON;
           CTLWIPTT = 'You do not have the authority to create jobs';
         ENDIF;
         // IF USER = 'KPANDOLF';
         IF USER = 'MJEFFERS';
           CTLCOPYDIS = *ON;
           CTLCOPYTT = 'You do not have the authority to copy POs';
         ENDIF;
         // No receipts if they don't have permission
         USRRTV(USER:'OIRMGR':OIRUSRLVL);
         IF OIRUSRLVL = 'NOV' OR UISAP = *BLANKS;
           CTLRCPTDIS = *ON;
           CTLRCPTTT = 'Not authorized to receive or no username set!';
         ENDIF;
         // Must be valid SAP to create or copy
         IF UISAP = *BLANKS;
           CTLNEWDIS = *ON;
           CTLNEWTT = 'Bad SAP ID or Authority Level';
           CTLCOPYDIS = *ON;
           CTLCOPYTT = 'Bad SAP ID or Authority Level';
         ENDIF;
         // Disallow copy/send if vendor is inactive
         IF OHSTAT <> ' ';
           CHAIN OHVEND VENDFMT;
           IF NOT %FOUND(VENDMST) OR VNCODE <> ' ';
             CTLCOPYDIS = *ON;
             CTLCOPYTT = 'Vendor blank or inactive - no copy';
             CTLSENDDIS = *ON;
             CTLSENDTT = 'Vendor #' + %TRIM(%CHAR(OHVEND)) + ' inactive';
           ENDIF;
         ENDIF;
         // Must have freight line to send (PPA or CPU)
         IF HASFRT = *OFF AND
            (OHFRT = 'PPA'  OR (OHFRT = 'CPU' AND OHCODE='I'));
           CTLSENDDIS = *ON;
           CTLSENDTT = 'Freight Code requires freight line';
         ENDIF;
         // Can only copy Inventory POs
         IF (OHCODE <> 'I' AND OHCODE <> *BLANK);
           CTLCOPYDIS = *ON;
           CTLCOPYTT = 'Copy only allowed for Inventory POs';
         ENDIF;
         // No update or add for ECOM
         IF OHCODE = 'E' AND USRMODE < 3;
           CTLUPDDIS = *ON;
           CTLUPDTT = 'No authority to change ECOM POs';
           CTLADDDIS = *ON;
           CTLADDTT = 'No authority to change ECOM POs';
         ENDIF;
         // only auto-create jobs if vendor is LTM
         IF NOT (OHVEND = 7805 OR OHVEND = 7751 OR
                 OHVEND = 7799 OR OHVEND = 7750);
           CTLWIPDIS = *ON;
           CTLWIPTT = 'Must be LTM vendor to create jobs';
         ENDIF;
         // No receipts if on hold, no approve or send if over limit
         IF OHSTAT = 'H';
           CTLRCPTDIS = *ON;
           CTLRCPTTT = 'Unapproved PO - Cannot enter receipts';
           IF POLIMIT = *ON;
             CTLSENDDIS = *ON;
             IF OHCODE <> 'E';
               CTLSENDTT = 'PO $' + %TRIM(%EDITC(POTOT: '1')) +
                           ' > max $' + %TRIM(%EDITC(POMAX: '1'));
             ELSE;
               CTLSENDTT = 'PO $' + %TRIM(%EDITC(POTOT:'4')) +
                           ' exceeds Ecomm max$';
             ENDIF;
           ENDIF;
         ENDIF;
         // No receipt if closed
         IF OHSTAT = 'C';
           CTLRCPTDIS = *ON;
           CTLRCPTTT = 'PO is closed - cannot receive';
         ENDIF;
         // checks for active records, no call to PRHST if non found
         SETLL (OHYY:OH#:OHPP) OIFMT;
         DOW 'X' = 'X';
           READE(N) (OHYY:OH#:OHPP) OIFMT;
           IF %EOF(ORITML5);
             LEAVE;
           ENDIF;
           IF OISTAT = *BLANKS;
             LEAVE;
           ENDIF;
         ENDDO;
         IF %EOF(ORITML5) OR OISTAT = *BLANKS;
           SETLL (OHYY:OHPP:OH#) ORFMT;
           IF NOT %EQUAL(ORECT);
             CTLPRHDIS = *ON;
             CTLPRHTT = 'No Receipt History found for this PO';
           ENDIF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC LoadLines;
         // Calculating and hiding/showing fields on screen
         SFLCLR = *ON;
         WRITE CTLFMT;
         SFLCLR = *OFF;
         SETLL (OHYY:OH#) OIFMT;
         FOR SFLR# = 1 TO 999;
           READE(N) (OHYY:OH#) OIFMT;
           IF %EOF(ORITML5);
             LEAVE;
           ENDIF;
           SFLSIZ = SFLR#;
           RESET SFLTT;

           CLEAR *ALL OITFMT;
           CHAIN(N) (OIYY:OIPP:OI#:OILIN#:OIPART:*BLANKS) OITFMT;
           // Save records for comparison
           OISAV(SFLR#) = OIDS;
           OITSAV(SFLR#) = OITDS;
           // Get related records
           CLEAR VRFMT;
           CHAIN(N) (OIPART) VRFMT;
           // Load custom fields
           SELECT;
           WHEN OISTAT = ' ';
             OISTATDSC = 'Open';
           WHEN OISTAT = 'C';
             OISTATDSC = 'Closed';
           WHEN OISTAT = 'D';
             OISTATDSC = 'Deleted';
           WHEN OISTAT = 'X';
             OISTATDSC = 'Cancelled';
           OTHER;
             OISTATDSC = *BLANKS;
           ENDSL;
           EVAL(H) EXTTOT = OIQTY * OIUNIT;
           PCSTL= VRUPTL * VRUNIQ;
           // Show/hide fields on screen
           IF OITXC <> *BLANKS;
             SFLNOTEVIS = *ON;
           ELSE;
             SFLNOTEVIS = *OFF;
           ENDIF;
           IF OIWSJOB > 0;
             OIWSVIS = *ON;
           ELSE;
             OIWSVIS = *OFF;
           ENDIF;
           IF OIREQ > 0;
             OIREQVIS = *ON;
           ELSE;
             OIREQVIS = *OFF;
           ENDIF;
           IF OHCODE = 'E';
             OIAKQYVIS = *ON;
             OIAKDTVIS = *ON;
           ELSE;
             OIAKQYVIS = *OFF;
             OIAKDTVIS = *OFF;
           ENDIF;

           // Is this a freight line?
           IF (OIPART = 'CSFREIGHTCHARGES' OR
               OIPART = 'CSFREIGHTFSC' OR
               OIPART = 'ESFREIGHT' OR
               OIPART = 'CSTARIFF')
              AND OISTAT <> 'X';
             FRTLIN = *ON;
             HASFRT = *ON;
           ELSE;
             FRTLIN = *OFF;
           ENDIF;
           // Save due date for reuse
           IF NOT FRTLIN;
             SVDDAT = OIDDAT;
           ENDIF;

           // Set Line DETAIL Mode
           SFLUPDDS = *OFF;
           SFLDIS0 = *ON;
           SFLDIS1 = *ON;
           SFLDIS2 = *ON;
           SFLDIS3 = *ON;
           SFLDIS9 = *ON;

           // Custom line DETAIL fields
           // OISTATUPD toggles status text/buttons
           OISTATUPD = *OFF;

           IF HIDEMSG = *OFF;
             VerifyLine();
             IF SFLERR <> *ZEROS;
               CTLHIDEDIS = *OFF;
             ENDIF;
           ELSE;
             RESET SFLERR;
           ENDIF;
           // Disable line update if record updated disabled
           IF CTLUPDDIS = *ON;
             SFLUPDDIS = *ON;
           ENDIF;
           // Disable update if closed
           IF OHSTAT = 'C';
             SFLUPDDIS = *ON;
             SFLUPDTT = 'Reopen PO Header to update the lines';
           ENDIF;
           WRITE SFLFMT;
         ENDFOR;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC ReadSubfile;
         // Only one update record, so no need for loop
         READC SFLFMT;
         IF %EOF();
           RETURN;
         ENDIF;
         SELECT;
         WHEN SFLUPD = *ON;
           SFLUPDR# = SFLR#;
           SFLUPD = *OFF;
         WHEN SFLNOTE = *ON;
           ShowLineText();
           SFLNOTE = *OFF;
         WHEN SFLVP = *ON;
           VPMGRCL('X');
           SFLVP = *OFF;
         WHEN SFLWH = *ON;
           WHMGRCL(' ':'X':'S');
           SFLWH = *OFF;
         WHEN SFLWS = *ON;
           WPMGRCL(' ':'U':'C':'S':'1');
           SFLWS = *OFF;
         WHEN SFLREQ = *ON;
           OMMGRCL('I');
           SFLREQ = *OFF;
         ENDSL;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC UpdateMode;
         // Signal changes pending on screen
         DCL-S OHPEND     IND;
         DCL-S OTPEND     IND;

         IF NEWREC = *ON;
           CTLHDR = 'NEW Purchase Order';
           CTLHDRCOL = 'lightgreen';
         ELSE;
           CTLHDR = 'Purchase Order UPDATE';
           CTLHDRCOL = 'gold';
         ENDIF;
         SetUpdateFields();
         OHPEND = *OFF;
         OTPEND = *OFF;
         VFYERR = *OFF;
         // PRIMARY loop for editing records
         DOW 1 = 1;
           RESET OHFMTCHG;
           RESET OTFMTCHG;
           // Avoid error spam when creating a new record
           IF NEWREC = *OFF OR VFYERR = *ON;
             VerifyRecord();
             CTLSAVDIS = VFYERR; // Fatal errors = no save
           ELSE;
             CTLSAVDIS = *OFF;
           ENDIF;
           EXFMT CTLFMT;
           IF OHFMTCHG <> *OFF OR OTFMTCHG <> *OFF;
             ShowChanges();
             IF OHFMTCHG <> *OFF;
               OHPEND = *ON;
             ENDIF;
             IF OTFMTCHG = *ON;
               OTPEND = *ON;
             ENDIF;
             IF CTLSAV = *ON;
               VFYERR = *ON;
             ENDIF;
           ENDIF;
           SELECT;
           WHEN CTLCNCL = *ON;
             IF NEWREC = *ON;
               IF @OH#SAV > 0;
                 // Reset to old record
                 @OHYY = @OHYYSAV;
                 @OH# = @OH#SAV;
                 @RET = @RETSAV;
                 @SIZ = @SIZSAV;
                 @OHYYSAV = *BLANKS;
                 @OH#SAV = 0;
                 @RETSAV = 0;
                 @SIZSAV = 0;
               ELSE;
                 // Called directly in new mode, exit
                 *INLR = *ON;
               ENDIF;
             ENDIF;
             RETURN;
           WHEN CTLSAV = *ON;
             IF NEWREC = *ON OR VFYERR = *ON;
               VerifyRecord();
               IF VFYERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Errors found, changes not saved!';
                 ITER;
               ENDIF;
             ENDIF;
             SAVERR = *OFF;
             IF NEWREC = *OFF;
               CheckModifiedRecord();
               CheckModifiedLines();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'PO was changed since it was opened! ' +
                             'Changes not saved!';
                 RETURN;
               ENDIF;
             ENDIF;
             IF OHPEND = *ON;
               PreSaveRecord();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error while saving changes!';
                 OHPEND = *OFF;
                 ITER;
               ENDIF;
               SaveRecord();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error saving changes!';
                 ITER;
               ENDIF;
               // Success
               PostSaveRecord();
               OHPEND = *OFF;
             ENDIF;
             IF OTPEND = *ON;
               SaveHeaderText();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error saving changes!';
                 OTPEND = *OFF;
                 ITER;
               ENDIF;
             ENDIF;
             IF NEWREC = *ON;
               // Reset program parameters to load in new record
               @RET = 0;
               @SIZ = 0;
               @OHYY = OHYY;
               @OH# = OH#;
               // Remove reference to saved record
               IF @OH#SAV > 0;
                 @OHYYSAV = *BLANKS;
                 @OH#SAV = 0;
                 @RETSAV = 0;
                 @SIZSAV = 0;
               ENDIF;
             ENDIF;
             RETURN;
           WHEN CTLCSMGR = *ON;
             MGRCL('CUMGR');
           WHEN CTLVNDMGR = *ON;
             VNDMGRCL(' ');
           OTHER;
           ENDSL;
         ENDDO;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SetUpdateFields;
         // Enable or disable fields based on user level or other reasons
         // ...DIS indicators to disable a field
         // ...TT fields sets the tool tip (i.e. reason why)
         // Set update mode
         FOR SFLR# = 1 TO SFLSIZ;
           CHAIN SFLR# SFLFMT;
           SFLUPDDS = *ON;
           UPDATE SFLFMT;
         ENDFOR;
         SFLR# = 0;
         CTLUPDDS = *ON;
         CTLPREVDIS = *ON;
         CTLNEXTDIS = *ON;
         CTLCNCLUPD = *ON;
         CTLSAVUPD = *ON;
         CTLCNCLDIS = *OFF;
         CTLSAVDIS = *OFF;

         RESET CTLTT;
         RESET CTLDIS0;
         RESET CTLDIS1;
         RESET CTLDIS2;
         RESET CTLDIS3;

         // Custom UPDATE fields
         // Toggles status text/buttons
         OHSTATUPD = *ON;

         // Disable fields or controls based on user level
         CTLDIS9 = *ON;
         IF USRMODE <= 3;
           CTLDIS3 = *ON;
         ENDIF;
         IF USRMODE <= 2;
           CTLDIS2 = *ON;
         ENDIF;
         IF USRMODE <= 1;
           CTLDIS1 = *ON;
         ENDIF;

         // OVERRIDE disabled/enabled fields
         // Set OHSTAT options based on current status
         // Only allow status and comment updates for C, X status
         SELECT;
         WHEN OHSTAT = 'C';
           CTLDIS1 = *ON;
           CTLDIS2 = *ON;
           CTLDIS3 = *ON;
           IF USRMODE > 1;
             OHSTAT_DIS = *OFF;
             OHSTATCDIS = *OFF;
             OHSTATXDIS = *ON;
             OHSTATXTT = 'Cannot cancel a closed PO';
             OHSTATHDIS = *ON;
             OHSTATHTT = 'Cannot hold a closed PO';
           ENDIF;
         WHEN OHSTAT = 'X';
           CTLDIS1 = *ON;
           CTLDIS2 = *ON;
           CTLDIS3 = *ON;
           OHSTAT_DIS = *OFF;
           OHSTATXDIS = *OFF;
           OHSTATCDIS = *ON;
           OHSTATCTT = 'Cannot close a cancelled PO';
           OHSTATHDIS = *ON;
           OHSTATHTT = 'Cannot hold a cancelled PO';
         ENDSL;
         // disallow reactivation of PO if vendor inactive
         IF OHSTAT <> ' ';
           CHAIN OHVEND VENDFMT;
           IF NOT %FOUND(VENDMST) OR VNCODE <> ' ';
             OHSTAT_DIS = *ON;
             OHSTAT_TT = 'Vendor #' + %TRIM(%CHAR(OHVEND)) +
                         ' inactive - cannot reopen';
           ENDIF;
         ENDIF;
         // disallow toggle of status from H if over approval limit
         // also disallow sending PO
         // also disallow receipts
         IF OHSTAT = 'H';
           OHSTATCDIS = *ON;
           OHSTAT_DIS = *ON;
           IF POLIMIT = *ON;
             OHSTAT_TT = 'PO $' + %EDITC(POTOT: '1') +
                         ' > max $' + %EDITC(POMAX: '1');
           ELSE;
             OHSTAT_TT = 'Transmit PO to Reopen';
           ENDIF;
         ENDIF;
         // disallow cancellation if partially received
         // int user level - can only close if all lines fully received
         IF OHSTAT = ' ';
           SETLL (OHYY: OH#: OHPP) OIFMT;
           DOW 'X'='X';
             READE(N) (OHYY: OH#: OHPP) OIFMT;
             IF %EOF(ORITML5);
               LEAVE;
             ENDIF;
             IF OIRQTY > *ZEROS;
               OHSTATXDIS = *ON;
               OHSTATXTT = 'Line ' + %TRIM(%CHAR(OILIN#)) +
                           ' partly received - Cannot cancel';
             ENDIF;
             IF (OIRQTY <> OIQTY AND USRMODE = 1);
               OHSTATCDIS = *ON;
               OHSTATCTT = 'Line ' + %TRIM(%CHAR(OILIN#)) +
                           ' Receive Qty <> Order Qty - Cannot close';
             ENDIF;
           ENDDO;
         ENDIF;
         // don't allow closing/holding open Ecom orders
         IF OHCODE = 'E';
           IF OHSTAT = ' ';
             OHSTATCDIS = *ON;
             OHSTATCTT = 'Ecom - Close not allowed';
             OHSTATHDIS = *ON;
             OHSTATHTT = 'Ecom - Hold not allowed';
           ELSEIF OHSTAT = 'C' AND USRMODE <= 2;
             OHSTAT_DIS = *ON;
           ENDIF;
         ENDIF;
         // disallow change of vendor and frt code if *any* receipts present
         SETLL (OHYY:OHPP:OH#) ORFMT;
         IF %EQUAL(ORECT);
           OHVENDDIS = *ON;
           OHVENDTT = 'Cannot change vendor - receipts found';
           OHFRTDIS = *ON;
           OHFRTTT = 'Cannot change freight code - receipts found';
         ENDIF;
         // disallow change of vendor if EDI
         IF OHTPKEY <> *BLANKS AND OHVENDCHG = *ON AND USRMODE < 4;
           OHVENDDIS = *ON;
           OHVENDTT = 'EDI - cannot change vendor!';
         ENDIF;
         // Disable address change if not EDI or MS PO
         IF OHCODE <> 'E' AND NOT (OHCODE = 'I' AND OHDC = 'MS');
           OHSHIPDIS = *ON;
           OHSADRDIS = *ON;
           OHSCTYDIS = *ON;
           OHSSTDIS = *ON;
           OHSZP5DIS = *ON;
           OHSZP4DIS = *ON;
           OHSTCTRDIS = *ON;
           OHSTPCDDIS = *ON;
         ENDIF;

         // NEW PO Overrides
         // Can't copy or send a new PO
         // Allow BusA change only for NEW POs
         // For new POs, can't change status
         IF NEWREC = *ON;
           OHPPDIS = *OFF;
           OHSTAT_DIS = *ON;
           OHSTATHDIS = *ON;
           OHSTATCDIS = *ON;
           OHSTATXDIS = *ON;
         ENDIF;

         // Keep these after anything that sets disabled fields
         // Enable customer picker for drop ship if it can be changed
         IF OHCODE = 'D' AND OHLOCDIS = *OFF;
           CTLCSDIS = *OFF;
         ENDIF;
         // Enable vendor manager if vendor can be changed
         IF OHVENDDIS = *OFF;
           CTLVNDDIS = *OFF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC ShowChanges;
         // Show pending changes, runs in UPDATE before verify
         // Use ...CHG indicators to detect a field change
         // Pay attention to order as further changes can be set or triggered!

         IF (OHCODECHG = *ON);
           // Show Drop Ship fields if type changes
           IF OHCODE = 'D' AND OHDC = *BLANKS;
             OHDC = 'DS';
             OHDCCHG = *ON;
           ENDIF;
         ENDIF;

         IF OHDCCHG = *ON;
           IF OHDC = 'MS' OR OHDC = 'DS';
             CTLDSVIS = *ON;
             CTLCSVIS = *ON;
             CTLCSDIS = *OFF;
           ELSE;
             CTLDSVIS = *OFF;
             CTLCSVIS = *OFF;
             CTLCSDIS = *ON;
           ENDIF;
         ENDIF;

         IF OHFRTCHG = *ON;
           CHAIN (OHFRT) INCOFMT;
         ENDIF;
         // Enable address change for MS PO
         IF OHDCCHG = *ON AND OHCODE = 'I';
           IF OHDC = 'MS';
             OHSHIPDIS = *OFF;
             OHSADRDIS = *OFF;
             OHSCTYDIS = *OFF;
             OHSSTDIS = *OFF;
             OHSZP5DIS = *OFF;
             OHSZP4DIS = *OFF;
             OHSTCTRDIS = *OFF;
             OHSTPCDDIS = *OFF;
           ELSE;
             OHSHIPDIS = *ON;
             OHSADRDIS = *ON;
             OHSCTYDIS = *ON;
             OHSSTDIS = *ON;
             OHSZP5DIS = *ON;
             OHSZP4DIS = *ON;
             OHSTCTRDIS = *ON;
             OHSTPCDDIS = *ON;
           ENDIF;
         ENDIF;
         // .....If PLT or Customer are not blank then retreive shipping address.
         IF OHDCCHG = *ON AND OHCODE='I' AND OHDC <> 'MS';
           CHAIN(N) OHDC COAFMT;
           IF %FOUND(COADDRES) AND (COADEL = 'W' OR COADEL=' ');
             OHSHIP = COANAM;
             OHSADR = STREET;
             OHSCTY = %SUBST(STCTST: 1: (%SCAN(',': STCTST) - 1));
             OHSST = %SUBST(STCTST: (%SCAN(',': STCTST) + 2): 2);
             OHSZP5 = %DEC(%SUBST(STCTST: (%SCAN('-': STCTST) - 5): 5): 5: 0);
             OHSZP4 = %DEC(%SUBST(STCTST: (%SCAN('-': STCTST) + 1): 4): 4: 0);
           ENDIF;
         ENDIF;
         IF OHLOCCHG = *ON AND OHCODE = 'D';
           IF OHNN <> '  ' AND OHCUID <> 0;
             CHAIN (OHNN:OHCUID:OHLOC) CSFMT;
             IF %FOUND(CUSTSHPP) AND CSX01 = 'T';
               CHAIN(N) CSR01 COAFMT;
               IF (%FOUND(CUSTSHPP) AND %FOUND(COADDRES));
                 OHSHIP = CSNAME;
                 OHSADR = CSADDR;
                 OHSCTY = CSCITY;
                 // usa address
                 IF CSCTRY = *BLANKS;
                   OHSST = CSST;
                   OHSZP5 = CSZIP5;
                   OHSZP4 = CSZIP4;
                   OHSTCTRY = *BLANKS;
                   OHSTPCD = *BLANKS;
                 ELSE;
                   // foreign address
                   OHSTCTRY = CSCTRY;
                   OHSTPCD = CSPOCD;
                   OHSST = CSST;
                   OHSZP5 = *ZEROS;
                   OHSZP4 = *ZEROS;
                 ENDIF;
                 IF OHDC = *BLANKS;
                   OHDC = CSR01;
                 ENDIF;
               ENDIF;
             ENDIF;
           ENDIF;
         ENDIF;

         // ZIPCITY data
         IF OHSZP5CHG = *ON;
           // clear out values for next zipcity check
           CLEAR ZIPCITYDS;
           IF OHSZP5 > 0;
             ZCODE = %CHAR(OHSZP5);
             // Find the city name from zip5
             // Do not call ZCLIB/ZC0200 due to use of "preferred name"
             SETLL ZCODE ZCFMT;
             IF %EQUAL(ZCCITY);
               DOW 'X' = 'X';
                 READE(E) ZCODE ZCFMT;
                 IF %ERROR() OR %EOF(ZCCITY);
                   LEAVE;
                 ENDIF;
                 CITY20 = ZCITY;
                 SELECT;
                 WHEN ZMIND = 'N';
                   CITY20 = *BLANKS;
                   ITER;
                 WHEN CITY20 = OHSCTY;
                   CITY = ZCITY;
                   STATE = ZSTATE;
                   LEAVE;
                 WHEN ZPREF = 'P';
                   PFCITY = ZCITY;
                   PFST = ZSTATE;
                 ENDSL;
               ENDDO;
               SELECT;
               WHEN CITY <> *BLANKS;
                 OHSCTY = CITY;
                 OHSST = STATE;
               WHEN PFCITY <> *BLANKS;
                 OHSCTY = PFCITY;
                 OHSST = PFST;
               WHEN CITY20 <> *BLANKS;
                 OHSCTY = ZCITY;
                 OHSST = ZSTATE;
               ENDSL;
             ENDIF;
             IF OHSST <> *BLANKS;
               OHSTCTRY = *BLANKS;
               OHSTPCD = *BLANKS;
             ENDIF;
           ENDIF;
           OHSCTY = %XLATE(',': ' ': OHSCTY);
         ENDIF;

         // Reload country name
         IF OHSTCTRCHG = *ON;
           CHAIN ('COUNTRY' + OHSTCTRY) QZZFMT;
           IF %FOUND(MASTSAZZ);
             CTRYNM = QZZDTA;
           ELSE;
             CTRYNM = *BLANKS;
           ENDIF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC VerifyRecord;
         // Verify data, runs when viewing or saving
         // Field name suffixes:
         // ...ERR - Show error for field
         // ...ERM - Error message to show
         // ...ERF - Error format, ERFINFO or ERFERROR (default)
         // ...UPD - Field is on if in update mode
         // ..SAV DS contains old record to compare value of updated fields
         // VFYERR - Fatal error, do not allow update
         RESET CTLERR;
         RESET CTLERM;
         RESET CTLERF;
         VFYERR = *OFF;

         // frt code cannot be blank
         IF OHFRT = *BLANKS;
           IF OHSTAT = ' ';
             OHFRTERR = *ON;
             OHFRTERM = 'Invalid Freight Terms';
             VFYERR = *ON;
           ELSE;
             OHFRTERR = *ON;
             OHFRTERM = 'Invalid Freight Terms';
             OHFRTERF = ERFINFO;
           ENDIF;
         ENDIF;

         // PO Limit reached
         IF POTOT > POMAX AND OHSTAT = 'H';
           POTOTERR = *ON;
           POTOTERM = 'PO $' + %TRIM(%EDITC(POTOT: '1')) +
                      ' > max $' + %TRIM(%EDITC(POMAX: '1'));
           POTOTERF = ERFINFO;
         // Not a fatal error, user can still update PO (but not approve/send)
         //    VFYERR = *ON;
         ENDIF;
         // Validate vendor name
         IF OHVEND > *ZEROS;
           CHAIN OHVEND VENDFMT;
           IF NOT %FOUND(VENDMST) OR VNCODE<>' ';
             //      OHVEND = 0;
             OHVENDERR = *ON;
             OHVENDERM = 'Invalid Vendor Number';
             OHVENDERF = ERFINFO;
             IF OHSTAT <> 'H';
               VFYERR = *ON;
               OHVENDERF = ERFERROR;
             ENDIF;
           ENDIF;
         ELSE;
           OHVENDERR = *ON;
           OHVENDERM = 'Invalid Vendor Number';
           VFYERR = *ON;
         ENDIF;
         // Invalid busA or cost center
         IF NOT(OHPP='93' OR OHPP='94' OR OHPP='95' OR
               OHPP='96' OR OHPP='97' OR OHPP='98');
           OHPPERR = *ON;
           OHPPERM = 'Invalid Business Area';
           VFYERR = *ON;
         ENDIF;
         CHAIN(N) (OHPP) COAFMT;
         IF NOT %FOUND(COADDRES) OR COADEL = 'D';
           OHDCERR = *ON;
           OHDCERM = 'Invalid Business Area';
           VFYERR = *ON;
         ENDIF;
         IF PAYOTQ = *BLANKS OR %DEC(PAYOTQ:10:0) < 700000
                             OR %DEC(PAYOTQ:10:0) > 810999;
           OHVENDERR = *ON;
           OHVENDERM = 'Invalid or missing cost center for DC';
           VFYERR = *ON;
         ENDIF;
         // Need customer for drop ship or MS!
         IF (OHCODE = 'D' OR OHDC='MS' OR OHDC='DS')
               AND OHNN  = *BLANKS
               AND (OHLOC = *BLANKS OR OHLOC = '000')
               AND OHCUID = *ZEROS;
           OHLOCERR = *ON;
           OHLOCERM = 'NN/ID/LOC Required for Drop Ship';
           VFYERR = *ON;
         ENDIF;
         // Check if drop ship customer has valid DC
         IF OHCODE='D' AND OHDC <> 'MS' AND
            OHNN <> *BLANKS AND OHCUID <> *ZEROS;
           CHAIN (OHNN:OHCUID:OHLOC) CSFMT;
           IF %FOUND(CUSTSHPP) AND CSX01 = 'T';
             CHAIN(N) CSR01 COAFMT;
             IF (NOT %FOUND(CUSTSHPP) OR NOT %FOUND(COADDRES));
               OHDC = *BLANKS;
               OHDCERR = *ON;
               OHDCERM = 'NN/ID/LOC has inactive DC';
               VFYERR = *ON;
             ENDIF;
           ENDIF;
         ENDIF;
         // Check if DC is valid
         IF OHCODE='I' AND OHDC <> 'MS';
           CHAIN(N) (OHDC) COAFMT;
           IF NOT %FOUND(COADDRES) OR (COADEL <> 'W' AND COADEL <> ' ');
             OHDCERR = *ON;
             OHDCERM = 'Invalid DC';
             VFYERR = *ON;
           ENDIF;
         ENDIF;

         // ZIPCITY check
         IF OHCODE='E' OR (OHCODE='I' AND OHDC='MS');
           IF OHSZP5 > 0;
             ZCODE = %CHAR(OHSZP5);
             // Find the city name from zip5
             // Do not call ZCLIB/ZC0200 due to use of "preferred name"
             SETLL ZCODE ZCFMT;
             IF NOT %EQUAL(ZCCITY);
               OHSZP5ERR = *ON;
               OHSZP5ERM = ZCODE + ' Zip not found; notify IT!';
               VFYERR = *ON;
             ENDIF;
             IF OHSST = *BLANKS;
               OHSSTERR = *ON;
               OHSSTERM = 'State is not entered or does not match ZIP';
               VFYERR = *ON;
             ENDIF;
           ELSE;
             IF OHSTCTRY = *BLANKS;
               OHSTCTRERR = *ON;
               OHSTCTRERM = 'Zip = 0, must specify Country Code';
               VFYERR = *ON;
             ENDIF;
           ENDIF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC PreSaveRecord;
         DCL-S OH#P       PACKED(6); // For conversion into CXOHPP

         // For EDI, clear info if changing to non-EDI vendor (level 4+ only)
         IF OHTPKEY <> *BLANKS;
           ASKHEADER = 'Change EDI Status?';
           ASKTEXT = 'This vendor was an EDI vendor. Changing the vendor ' +
                     'will erase the EDI relationship for this PO. ' +
                     'Are you sure?';
           ASKCHOICE = 'N Cancel the change,Y Proceed with the change';
           ASKVALUE = 'N,Y';
           ASKRETURN = 'N'; // Default
           EXFMT ASKFMT;
           IF ASKRETURN = 'Y';
             OHTPKEY = *BLANKS;
             OHEDOC = 0;
             OHESTS = *BLANKS;
             OHOBGRP = *LOVAL;
             OHOBACT = *LOVAL;
           ELSE;
             OHVENDERR = *ON;
             OHVENDERM = 'Cancelled changing EDI Vendor';
             OHVENDERF = ERFINFO;
             SAVERR = *ON;
             RETURN;
           ENDIF;
         ENDIF;

         // Cancel toggle logic from OHMGR
         IF (OHSTAT = ' ' AND OHSAV.OHSTAT = 'X') OR
            (OHSTAT = 'X' AND OHSAV.OHSTAT = ' ');
           SELECT;
           WHEN OHCODE <> 'E';
             // ask user if they want to uncancel all lines or just header
             IF OHSAV.OHSTAT = 'X';
               ASKHEADER = 'Reactivate all lines?';
               ASKTEXT = 'Do you want to Reactivate all lines on this PO?';
               ASKCHOICE = 'N No just the header,Y Reactivate all';
               ASKVALUE = 'N,Y';
               ASKRETURN = 'N'; // Default
               EXFMT ASKFMT;
             ENDIF;
             // update lines if user wanted all lines reopened or if cancelling
             IF ASKRETURN = 'Y' OR OHSAV.OHSTAT = ' ';
               // loop all order lines and set line status to match header
               FOR SFLUPDR# = 1 TO SFLSIZ;
                 OIDS = OISAV(SFLUPDR#);
                 OISTAT = OHSTAT;
                 SaveLine();
               ENDFOR;
               SFLUPDR# = 0;
             ENDIF;
             // warn user if toggling EDI
             IF OHTPKEY <> *BLANKS;
               CTLFMTERR = *ON;
               CTLFMTERM = 'EDI: contact vendor with changes';
             ENDIF;
           WHEN OHCODE='E' AND USRMODE > 2;
             // loop thru and flag all lines
             FOR SFLUPDR# = 1 TO SFLSIZ;
               OIDS = OISAV(SFLUPDR#);
               OISTAT = OHSTAT;
               SaveLine();
             ENDFOR;
             SFLUPDR# = 0;
           ENDSL;
         ENDIF;

         // Close toggle logic from OHMGR
         IF (OHSTAT = ' ' AND OHSAV.OHSTAT = 'C') OR
            (OHSTAT = 'C' AND OHSAV.OHSTAT = ' ');
           SELECT;
           WHEN OHCODE <> 'E';
             FOR SFLUPDR# = 1 TO SFLSIZ;
               OIDS = OISAV(SFLUPDR#);
               IF OHSTAT = ' ';
                 // allow manual close of order, set all lines to "C"
                 IF OISTAT <> 'X';
                   OISTAT = 'C';
                 ENDIF;
                 SaveLine();
               ELSE;
                 // reopen line only if not fully received
                 IF OIRQTY < OIQTY AND OISTAT <> 'X'
                    AND NOT (FRTLIN AND OHFRT = 'CPU');
                   OISTAT = ' ';
                   OISTAT = OHSTAT;
                   SaveLine();
                 ENDIF;
               ENDIF;
             ENDFOR;
             SFLUPDR# = 0;
         // since CXCOPY skipped processing closed POs, ensure OHPP set properly if toggling open
             IF OHSTAT=' ' AND OHPP='94';
               OH#P = OH#;
               CXOHPP(OHYY:OHPP:OH#P:OHPP);
             ENDIF;
           //
           // E-comm orders manual open
           WHEN OHCODE = 'E' AND OHSTAT = 'C';
             FOR SFLUPDR# = 1 TO SFLSIZ;
               OIDS = OISAV(SFLUPDR#);
               // reopen line only if not fully received
               IF OIRQTY <> OIQTY;
                 OISTAT = ' ';
                 OISTAT = OHSTAT;
                 SaveLine();
               ENDIF;
             ENDFOR;
             SFLUPDR# = 0;
           ENDSL;
         ENDIF;

         // toggle freight line, if frt code PPA or CPU
         IF OHFRT = 'PPA' OR OHFRT = 'CPU';
           FOR SFLUPDR# = 1 TO SFLSIZ;
             OIDS = OISAV(SFLUPDR#);
             IF OISTAT = 'X';
               ITER;
             ENDIF;
             IF FRTLIN;
               SETLL (OHYY: OHPP: OH#) ORFMT;
               IF %FOUND(ORITML5) AND OHFRT = 'PPA';
                 OISTAT = ' ';
                 IF NOT %EQUAL(ORECT);
                   OIRQTY = 0;
                 ENDIF;
               ELSE;
                 IF NOT %EQUAL(ORECT);
                   OISTAT = 'C';
                 ENDIF;
               ENDIF;
               SaveLine();
             ENDIF;
           ENDFOR;
           SFLUPDR# = 0;
         ENDIF;

         // New HEADER record
         IF NEWREC = *ON;
           OHYY = %SUBST(%CHAR(%DATE(): *ISO): 3: 2);
           // ALL PO NUMBERS GENERATED FROM WAREHOUSE 94 PRATT-ATLANTA
           CHAIN '94' COAFMT;
           // roll back po#, dont allow 7 digits
           IF COAPO# >= 999999;
             COAPO# = 100;
           ELSE;
             COAPO# += 1;
           ENDIF;
           OH# = COAPO#;
           UPDATE COAFMT;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SaveRecord;
         DCL-DS OHCUR     LIKEREC(OHFMT) INZ;

         // Allowance for LOCKMSG logic
         DOU NOT %ERROR();
           CHAIN(E) (OHYY:OH#) OHFMT OHCUR;
           IF %ERROR();
             LOCKMSG(STATUSS);
           ENDIF;
         ENDDO;

         OHCHDT = %DATE();
         OHCHTM = %TIME();
         OHCHBY = USER;
         OHX = 'X';

         OHCUR = OHDS;
         IF NOT %FOUND(ORHDRL4);
           IF NEWREC = *ON;
             WRITE OHFMT OHCUR;
           ELSE;
             SAVERR = *ON;
           ENDIF;
         ELSE;
           UPDATE OHFMT OHCUR;
         ENDIF;
         UNLOCK ORHDRL4;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC PostSaveRecord;
         // Old record is still in ...SAV

         // Send cancellations to SAP if interfaced
         IF (OHSTAT = 'X' AND OHSAV.OHSTAT = ' ');
           SETLL SPHPO# SAPHFMT;
           IF %EQUAL(SAPPOHDCP);
             SAPPOINT(OHYY:OHPP:OH#);
           ENDIF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SaveHeaderText;
         DCL-DS OTCUR     LIKEREC(OTFMT) INZ;

         // Allowance for LOCKMSG logic
         DOU NOT %ERROR();
           CHAIN(E) (OHYY:OHPP:OH#:*BLANKS) OTFMT OTCUR;
           IF %ERROR();
             LOCKMSG(STATUSS);
           ENDIF;
         ENDDO;
         OHTUSE = USER;
         OHTDAT = %DATE();
         IF NOT %FOUND(ORHTX); // New record
           OHTDEL = *BLANKS;
           OHTYY = OHYY;
           OHTPP = OHPP;
           OHT# = OH#;
           OHTCX = 'X';
           OHTXP = *BLANKS;
           OTCUR = OTDS;
           WRITE OTFMT OTCUR;
         ELSE;
           OTCUR = OTDS;
           UPDATE OTFMT OTCUR;
         ENDIF;
         UNLOCK ORHTX;
       END-PROC;
       // ------------------------------------------------------------------- //

       // PRIMARY loop for editing records
       // ------------------------------------------------------------------- //
       DCL-PROC LineUpdateMode;
         // Signal changes pending on screen
         DCL-S OIPEND     IND;
         DCL-S OITPEND    IND;
         // Dummy EOJ for WHSELUI, don't want to allow exit in UPDATE
         DCL-S NOEOJ      CHAR(1) INZ(' ');

         CTLHDR = 'Purchase Order UPDATE';
         CTLHDRCOL = 'orange';
         SetLineUpdateFields();
         OIPEND = *OFF;
         OITPEND = *OFF;
         VFYERR = *OFF;
         DOW 1 = 1;
           RESET OIFMTCHG;
           RESET OITFMTCHG;
           // Avoid error spam when creating a new record
           IF NEWLIN = *OFF OR VFYERR = *ON;
             VerifyLine();
             CTLSAVDIS = VFYERR; // Fatal errors = no save
           ELSE;
             CTLSAVDIS = *OFF;
           ENDIF;
           UPDATE SFLFMT;
           EXFMT CTLFMT;
           CHAIN SFLUPDR# SFLFMT;
           IF OIFMTCHG <> *OFF;
             ShowLineChanges();
             IF OIFMTCHG <> *OFF;
               OIPEND = *ON;
             ENDIF;
             IF CTLSAV = *ON;
               VFYERR = *ON;
             ENDIF;
           ENDIF;
           SELECT;
           WHEN CTLCNCL = *ON;
             NEWLIN = *OFF;
             RETURN;
           WHEN CTLSAV = *ON;
             IF NEWLIN = *ON OR VFYERR = *ON;
               VerifyLine();
               IF VFYERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Errors found, changes not saved!';
                 ITER;
               ENDIF;
             ENDIF;
             SAVERR = *OFF;
             CheckModifiedRecord();
             CheckModifiedLines();
             IF SAVERR = *ON;
               CTLFMTERR = *ON;
               CTLFMTERM = 'PO was changed since it was opened! ' +
                           'Changes not saved!';
               SFLUPDR# = 0;
               NEWLIN = *OFF;
               RETURN;
             ENDIF;
             IF OIPEND = *ON;
               PreSaveLine();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error while saving changes!';
                 ITER;
               ENDIF;
               SaveLine();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error saving changes!';
                 ITER;
               ENDIF;
               PostSaveLine();
             ENDIF;
             IF OITPEND = *ON;
               SaveLineText();
               IF SAVERR = *ON;
                 CTLFMTERR = *ON;
                 CTLFMTERM = 'Error saving changes!';
                 ITER;
               ENDIF;
             ENDIF;
             SFLUPDR# = 0;
             NEWLIN = *OFF;
             RETURN;
           WHEN SFLNOTE = *ON;
             OITPEND = ShowLineText();
           WHEN SFLVP = *ON;
             VPMGRCL('X');
             SFLVP = *OFF;
           WHEN SFLWH = *ON;
             IF OIPART = *BLANKS;
               WHSELUI(NOEOJ:'N':OHDC);
             ELSE;
               WHMGRCL(' ':'X':'S');
             ENDIF;
             SFLWH = *OFF;
           WHEN SFLWS = *ON;
             WPMGRCL(' ':'U':'C':'S':'1');
             SFLWS = *OFF;
           WHEN SFLREQ = *ON;
             OMMGRCL('I');
             SFLREQ = *OFF;
           OTHER;
           ENDSL;
         ENDDO;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SetLineUpdateFields;
         // Enable or disable fields based on user level or other reasons
         // ...DIS indicators to disable a field
         // ...TT fields sets the tool tip (i.e. reason why)
         // Set update mode
         FOR SFLR# = 1 TO SFLSIZ;
           CHAIN SFLR# SFLFMT;
           SFLUPDDS = *ON;
           UPDATE SFLFMT;
         ENDFOR;
         CTLUPDDS = *ON;
         CTLPREVDIS = *ON;
         CTLNEXTDIS = *ON;
         CTLCNCLUPD = *ON;
         CTLSAVUPD = *ON;
         CTLCNCLDIS = *OFF;
         CTLSAVDIS = *OFF;
         // Replace buffer with update line
         OIDS = OISAV(SFLUPDR#);
         OITDS = OITSAV(SFLUPDR#);
         // Position to update line
         CHAIN SFLUPDR# SFLFMT;

         RESET SFLTT;
         RESET SFLDIS0;
         RESET SFLDIS1;
         RESET SFLDIS2;
         RESET SFLDIS3;

         // Is this a freight line?
         IF (OIPART = 'CSFREIGHTCHARGES' OR
             OIPART = 'CSFREIGHTFSC' OR
             OIPART = 'ESFREIGHT' OR
             OIPART = 'CSTARIFF')
            AND OISTAT <> 'X';
           FRTLIN = *ON;
         ELSE;
           FRTLIN = *OFF;
         ENDIF;

         // Set Line UPDATE Mode

         // Custom line UPDATE Fields
         // OISTATUPD toggles status text/buttons
         OISTATUPD = *ON;
         // Can set/add notes in update
         SFLNOTEVIS = *ON;
         SFLNOTEDIS = *OFF;

         // Disable fields or controls based on user level
         SFLDIS9 = *ON;
         IF USRMODE <= 3;
           SFLDIS3 = *ON;
         ENDIF;
         IF USRMODE <= 2;
           SFLDIS2 = *ON;
         ENDIF;
         IF USRMODE <= 1;
           SFLDIS1 = *ON;
         ENDIF;

         // OVERRIDE disabled fields
         // Can't change part numbers if REQ attached
         IF NOT FRTLIN AND (OHCODE = 'E' OR OIREQ <> 0);
           OIPARTDIS = *ON;
           OIDESCDIS = *ON;
           OIPARTTT = 'Cannot change item - REQ attached';
         ENDIF;
         // warn if not fully received
         IF OIRQTY < OIQTY AND OHFRT <> 'CPU';
           //    OISTATCDIS = *ON;
           OISTATCTT = 'Receive Qty < Order Qty';
         ENDIF;
         // Cannot change line data if partly received
         //       Cannot cancel line if partly received
         IF OIRQTY > *ZEROS;
           OIPARTDIS = *ON;
           OIPARTTT = 'Cannot change - partly received';
           OIDESCDIS = *ON;
           OIDESCTT = 'Cannot change - partly received';
           OIDDATDIS = *ON;
           OIDDATTT = 'Cannot change - partly received';
           OIUOMDIS = *ON;
           OIUOMTT = 'Cannot change - partly received';
           OIUNITDIS = *ON;
           OIUNITTT = 'Cannot change - partly received';
           OISTATXDIS = *ON;
           OISTATXTT = 'Cannot cancel - partly received';
         ENDIF;
         // this section of coding will allow removal of "CPU" frght chrg
         // can't cancel a closed line
         IF OISTAT = 'C' AND NOT
           (FRTLIN AND OHCODE = 'I' AND OHFRT = 'CPU');
           OISTATXDIS = *ON;
           OISTATXTT = 'Cannot cancel a closed line';
         ENDIF;
         // can't close a cancelled line
         IF OISTAT = 'X';
           OISTATCDIS = *ON;
           OISTATCTT = 'Cannot close a cancelled line';
         ENDIF;
         //       Ecomm - only exp OR pgmr can change inactive order
         IF OHCODE='E' AND USRMODE <= 2;
           IF OISTAT <> ' ';
             SPHPO# = PONUM;
             SETLL SPHPO# SAPHFMT;
             IF %EQUAL(SAPPOHDCP);
               OISTAT_DIS = *ON;
               OISTAT_TT = 'PO sent to SAP, cannot reopen line';
               OISTATCDIS = *ON;
             ENDIF;
           ENDIF;
         ENDIF;
         // Keep this after anything that sets OIPARTDIS
         // Allow for choosing part from VPMGR if enabled
         IF OIPARTDIS = *OFF;
           SFLVPDIS = *OFF;
           SFLWHDIS = *OFF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC ShowLineChanges;
         // Show pending changes, runs in UPDATE before verify
         // Use ...CHG indicators to detect a field change
         // Pay attention to order as further changes can be set or triggered!
         DCL-S X          INT(5);

         IF NEWLIN = *ON AND OI# = 0;
           OIYY = OHYY;
           OI# = OH#;
           OIPP = OHPP;
         ENDIF;

         IF OISTATCHG = *ON;
           // Adjust total to show cancelled lines
           IF OISTAT = 'X';
             POTOT -= EXTTOT;
           ELSEIF OISTAT = ' ' AND OISAV(SFLUPDR#).OISTAT <> 'C';
             POTOT += EXTTOT;
           ENDIF;
         ENDIF;

         IF OIPARTCHG = *ON;
           IF NEWLIN = *ON;
             // determine next line #, assure no duplicates
             OILIN# = 1;
             IF (OIPART = 'CSFREIGHTCHARGES' OR
                 OIPART = 'CSFREIGHTFSC' OR
                 OIPART = 'ESFREIGHT' OR
                 OIPART = 'CSTARIFF');
               OILIN# = 900;
               FRTLIN = *ON;
             ENDIF;
             SETLL (OIYY:OI#:OIPP:OILIN#) OIFMT;
             DOW %EQUAL(ORITML5);
               OILIN# += 1;
               SETLL (OIYY:OI#:OIPP:OILIN#) OIFMT;
             ENDDO;
             OILIN#CHG = *ON;
           ENDIF;

           // Retrieve new part description
           CLEAR *ALL VRFMT;
           CHAIN(N) (OIPART) VRFMT;
           IF %FOUND(VENDPART);
             OISKU = VRIT;
             OIDESC = VQDESC;
             OIDESCCHG = *ON;
             OIUOM = VRPER1;
             OIUOMCHG = *ON;
             PCSTL = VRUPTL * VRUNIQ;
           ENDIF;

           IF OIDDAT = *LOVAL;
             IF NOT FRTLIN;
               OIDDAT = %DATE() + %DAYS(VRLEAD);
             ELSE;
               // For freight, find lowest due date
               OIDDAT = *HIVAL;
               FOR X = 1 TO SFLUPDR# - 1;
                 IF OISAV(X).OIDDAT < OIDDAT;
                   OIDDAT = OISAV(X).OIDDAT;
                 ENDIF;
               ENDFOR;
               IF OIDDAT = *HIVAL;
                 OIDDAT = %DATE();
               ENDIF;
             ENDIF;
             OIDDATCHG = *ON;
           ENDIF;

           // Get warehouse order qty
           CHAIN (OHDC:VRNN:VRID:VRIT) WHFMT;
           IF %FOUND(WAREHOUS);
             OIQTY = WHORDQ;
           ELSE;
             OIQTY = 0;
           ENDIF;
           OIQTYCHG = *ON;
           RTVPOCOST(OHYY:OH#:OILIN#:OIPART:OIUNIT);
           OIUNITCHG = *ON;
           OIUOMCHG = *ON;

           //if item found in vendpaby - use uom from there
           SETLL (OISKU: OHDC: OHVEND) VYFMT;
           DOW 'X'='X';
             READE(N) (OISKU: OHDC: OHVEND) VYFMT;
             IF %EOF(VENDPABL6) OR VYDEL<>'D';
               LEAVE;
             ENDIF;
           ENDDO;
           // only use uom from PABY table if D/C is a trading partner
           CHAIN(N) OHDC COAFMT;
           IF VYUOM <> *BLANKS AND NOT %EOF(VENDPABL6) AND OHTPKEY <> *BLANKS;
             OIUOM = VYUOM;
           ENDIF;

           // Retrieve SAP account numbers for WH/Part
           RTVSAPACCT(OHDC:OIPART:OIPURG:OIMTLG:OIMTLN:OICSTC:OIGLACT);
         ENDIF;

         IF OIDESCCHG = *ON AND OIDESC = *BLANKS AND OIPART <> *BLANKS;
           // Reload description
           CLEAR *ALL VRFMT;
           CHAIN(N) (OIPART) VRFMT;
           IF %FOUND(VENDPART);
             OIDESC = VQDESC;
             OIDESCCHG = *ON;
           ENDIF;
         ENDIF;

         IF (OIQTYCHG = *ON OR OIUNITCHG = *ON) AND NOT FRTLIN;
           // Recalculate pricing
           POTOT -= EXTTOT;
           EVAL(H) EXTTOT = OIQTY * OIUNIT;
           MONITOR;
             POTOT += EXTTOT;
           ON-ERROR;
             POTOT = *HIVAL;
           ENDMON;
           // Recalculate open qty
           OIBQTY = OIQTY - OIRQTY;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC VerifyLine;
         // Verify data, runs when viewing or saving
         // Field name suffixes:
         // ...ERR - Show error for field
         // ...ERM - Error message to show
         // ...ERF - Error format, ERFINFO or ERFERROR (default)
         // ...UPD - Field is on if in update mode
         // ..SAV DS contains old record to compare value of updated fields
         // VFYERR - Fatal error, do not allow update

         DCL-S OIWSTIMEP  PACKED(5); // For conversion of OIWSTIME
         DCL-S MAXQ       PACKED(7);
         DCL-S WJQTY      PACKED(7);
         DCL-S WSDC       LIKE(OHDC);

         RESET SFLERR;
         RESET SFLERM;
         RESET SFLERF;
         VFYERR = *OFF;

         // frt line not allowed for PPD or COL frt codes
         IF NEWLIN = *ON AND FRTLIN = *ON AND OHFRT <> 'PPA' AND OHFRT <> 'CPU';
           OIPARTERR = *ON;
           OIPARTERM = 'Freight charge allowed for CPU or PPA only';
           VFYERR = *ON;
         ENDIF;
         //::::: Perform validation on VR    info
         IF OHCODE = 'M' OR OHCODE = 'D' OR OHCODE = 'E' OR OHCODE = 'I';
           IF OIPART <> *BLANKS;
             CHAIN(N) OIPART VRFMT;
             IF %FOUND(VENDPART) AND FRTLIN = *OFF AND
                OISTAT <> 'X' AND VRNN <> OHPP;
               OIPARTERR = *ON;
               OIPARTERM = 'Part number not in this BusA!';
               OIPARTERF = ERFINFO;
             ENDIF;
             IF (NOT %FOUND(VENDPART) OR VRDEL='D' OR VRDEL='F') AND
                   OIPART<>'FREIGHT';
               RESET VRFMT;
               OIPARTERR = *ON;
               OIPARTERM = 'Invalid part number';
               VFYERR = *ON;
             ENDIF;
           ENDIF;
         ENDIF;
         // Validation on qty
         IF OIQTY <= 0;
           OIQTYERR = *ON;
           OIQTYERM = 'Order Quantity must be greater than 0';
           VFYERR = *ON;
         ENDIF;
         IF FRTLIN AND OIQTY > 1;
           OIQTYERR = *ON;
           OIQTYERM = 'Qty must be 1 for freight or tariff';
           VFYERR = *ON;
         ENDIF;
         IF POTOT = *HIVAL OR POTOT > POMAXA;
           EXTTOTERR = *ON;
           EXTTOTERM = 'PO total$ too large? Verify entries';
           VFYERR = *ON;
         ENDIF;
         // order qty cannot drop below receipt qty
         IF OIRQTY > (OIQTY * 1.1);
           OIQTYERR = *ON;
           OIQTYERM = 'Receipt Qty > Order Qty';
           VFYERR = *ON;
         ENDIF;
         // added 3/28/2013 - chk for missing costctr or glacct
         IF (OICSTC=*BLANKS OR OIGLACT=*ZEROS)
               AND OIPART <> 'FREIGHT';
           OICSTCERR = *ON;
           OICSTCERM = 'Missing Cost center or GL account';
           VFYERR = *ON;
         ENDIF;
         // Validate due date
         //  IF ((OIDDAT < %DATE() - %YEARS(1) OR OIDDAT > %DATE() + %YEARS(1))
         //       AND OIDDAT <> *LOVAL)
         //        OR (OIDDAT = *LOVAL AND OISAV.OIDDAT = *LOVAL)
         //        OR ((NEWLIN = *ON OR OIDDAT <> OISAV.OIDDAT)
         //          AND OIDDAT < %DATE());
         IF OISTAT = ' ' AND
           (OIDDAT = *LOVAL OR // No due date
           OIDDAT < %DATE() - %YEARS(1) OR // very old date
           OIDDAT > %DATE() + %YEARS(1) OR // too far in the future
           (SFLUPDR# > 0 AND OIDDATDIS = *OFF AND OIDDAT < %DATE())); // New line or in update mode
           OIDDATERR = *ON;
           OIDDATERM = 'Invalid Due Date';
           VFYERR = *ON;
         ENDIF;
         //::::: Perform validation on UOM.
         //if item found in vendpaby - use uom from there
         SETLL (OISKU: OHDC: OHVEND) VYFMT;
         DOW 'X'='X';
           READE(N) (OISKU: OHDC: OHVEND) VYFMT;
           IF %EOF(VENDPABL6) OR VYDEL <> 'D';
             LEAVE;
           ENDIF;
         ENDDO;
         // only use uom from PABY table if D/C is a trading partner
         CHAIN(N) OHDC COAFMT;
         IF VYUOM = *BLANKS OR NOT %EOF(VENDPABL6) OR OHTPKEY = *BLANKS;
           // otherwise verify uom is valid code from vendsazz table
           VZZVAL = OIUOM;
           IF %LOOKUP(VZZVAL:VZZ) = 0;
             OIUOMERR = *ON;
             OIUOMERM = 'UOM is invalid';
             VFYERR = *ON;
           ENDIF;
         ENDIF;

         IF NOT (FRTLIN OR OIPART = CDSALES OR OIPART = PPSALES OR
                 %SUBST(OIPART:1:2)='ES' OR OIPART = ESPROCS OR OIPART = CSRUSH)
               AND SFLUPDR# = SFLR#
               AND NEWLIN = *ON
               AND OHCODE = 'D';
           OIPARTERR = *ON;
           OIPARTERM = 'Drop Ship - only freight entry valid';
           VFYERR = *ON;
         ENDIF;

         IF SFLUPDR# = SFLR# AND
           ((OIUNIT <> OISAV(SFLUPDR#).OIUNIT
             AND OISAV(SFLUPDR#).OIUNIT <> *ZEROS) OR
            (OIQTY <> OISAV(SFLUPDR#).OIQTY
             AND OISAV(SFLUPDR#).OIQTY <> *ZEROS));
           IF OHCODE = 'I';
             WSDC = OHDC;
           ELSE;
             WSDC = 'DS';
           ENDIF;
           OIWSTIMEP = OIWSTIME;
           CHAIN(N) (OIWSNN:OIWSJOB:WSDC:OIWSYMD:OIWSLOC:OIWSTIMEP) WSFMT;
           IF NOT %FOUND(WIPSHIP);
             CLEAR *ALL WSFMT;
           ENDIF;
           CHAIN(N) (OIWSNN:OIWSJOB) WPFMT;
           IF NOT %FOUND(WIPJOBS);
             CLEAR *ALL WPFMT;
           ENDIF;
           IF %FOUND(WIPJOBS) AND WPJOBP <> 'P';
             CHAIN (WSNN:WSID:WSLOC) CSFMT;
             IF NOT %FOUND(CUSTSHPP);
               CLEAR *ALL CSFMT;
             ENDIF;
             EVAL(H) MAXQ = (WPORDQ * (CSMAXO + 1)) - WPSHIQ;
             //..calculate open qty for this job reading all WS records
             WJQTY = 0;
             SETLL (OIWSNN:OIWSJOB) WSFMT;
             IF %EQUAL(WIPSHIP);
               DOW 1 = 1;
                 READE(NE) (OIWSNN:OIWSJOB) WSFMT;
                 IF %ERROR OR %EOF(WIPSHIP);
                   LEAVE;
                 ENDIF;
                 IF WSDEL = 'D' OR WSDEL = 'W' OR WSDEL = 'P' OR
                    WSDEL = 'S' OR WSDEL = 'C' OR WSTO > *BLANKS;
                   ITER;
                 ENDIF;
                 WJQTY += WSQTY;
               ENDDO;
             ENDIF;
             MAXQ -= WJQTY;
             // ..factor in qty change
             MAXQ -= OIQTY - OISAV(SFLUPDR#).OIQTY;
             IF MAXQ < 0;
               OIQTYERR = *ON;
               OIQTYERM = 'ShipQ > OpenQ on this Job';
               IF USRMODE < 4;
                 VFYERR = *ON;
               ELSE;
                 OIQTYERF = ERFINFO;
               ENDIF;
             ENDIF;
           ENDIF;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC PreSaveLine;
         DCL-S OIWSTIMEP  PACKED(5); // For conversion of OIWSTIME
         DCL-S WSDC       LIKE(OHDC);

         IF OISTAT = 'X' AND OISAV(SFLUPDR#).OISTAT = ' ' AND OHCODE <> 'E';
           CHAIN (OIYY: OIREQ) OMFMT;
           IF %FOUND(ORMSRR) AND OIQTY = OMQTY;
             OMSTAT = 'A';
             OMAPP = *BLANKS;
             OMPOYY = *BLANKS;
             OMPOPP = *BLANKS;
             OMPO# = *ZEROS;
             OMPOIT = *BLANKS;
             OMPODD = *LOVAL;
             OMPOQ = *ZEROS;
             OMPOUM = *BLANKS;
             OMPOBY = *BLANKS;
             OMPODT = *LOVAL;
             UPDATE OMFMT;
             CTLFMTERR = *ON;
             CTLFMTERM = 'Line ' + %CHAR(OILIN#) +
                         ' cancelled, Req #' +
                         %TRIM(%CHAR(OIREQ)) + ' reopened';
           ENDIF;
           UNLOCK ORMSRR;
         ENDIF;

         // check for vendpaby - create if not found
         IF NOT (FRTLIN = *ON OR
                 OIPART = CDSALES OR OIPART = PPSALES OR
                 %SUBST(OIPART:1:2) = 'ES' OR
                 OIPART = ESPROCS OR OISTAT = 'X' OR
                 OIPART = CSRUSH);
           // dont add/update paby if ecomm and tpkey(trading partner) <> blanks
           // 06/20/13 per Wayne
           // (only do update if ordering from non-EDI vendor)
           IF OHCODE <> 'E' OR (OHCODE = 'E' AND OHTPKEY = *BLANKS);
             UPDVY();
             RETURN;
           ENDIF;

           // update vendpaco for drop ship po's
           IF OHDC='DS';
             IF OHCODE = 'E';
               // if Ecomm - set ORG to EAG
               ORG='EAG';
             ENDIF;
             UPDVO();
             RETURN;
           ENDIF;
         ENDIF;

         // update PO, VY,CO,VP if qty and/or cost changes
         // For new records, nothing to compare
         IF (OIUNIT <> OISAV(SFLUPDR#).OIUNIT
             AND OISAV(SFLUPDR#).OIUNIT <> *ZEROS) OR
            (OIQTY <> OISAV(SFLUPDR#).OIQTY
             AND OISAV(SFLUPDR#).OIQTY <> *ZEROS);
         // automatically reopen line if backorder qty <> 0 and line was closed
           OIBQTY = OIQTY - OIRQTY;
           IF OISTAT = 'C' AND OIQTY > OIRQTY;
             OISTAT = ' ';
           ENDIF;
           // added 1/9/2013 frt line closed for inventory with CPU frt code
           IF FRTLIN = *ON AND OHFRT='CPU';
             OISTAT = 'C';
           ENDIF;
           // validate/update to wipship if changed qty
           IF OIQTY <> OISAV(SFLUPDR#).OIQTY;
             IF OHCODE = 'I';
               WSDC = OHDC;
             ELSE;
               WSDC = 'DS';
             ENDIF;
             // ..update wipship if no error (check moved to VerifyLine)
             CHAIN (OIWSNN:OIWSJOB:WSDC:OIWSYMD:OIWSLOC:OIWSTIMEP) WSFMT;
             IF %FOUND(WIPSHIP);
               WSUSER = USER;
               WSCYMD = %DEC(%CHAR(%DATE():*CYMD0):7:0);
               WSCHMS = %DEC(%CHAR(%TIME():*HMS0):7:0);
               WSQTY = OIQTY;
               UPDATE WSFMT;
             ENDIF;
           ENDIF;

           // update req if no error and changed cost or qty
           CHAIN (OIYY:OIREQ) OMFMT;
           IF %FOUND(ORMSRR);
             OMUSER = USER;
             OMCDAT = %DATE();
             OMQTY = OIQTY;
             OMEST$ = OIUNIT;
             UPDATE OMFMT;
           ENDIF;
           UNLOCK ORMSRR;

           // if Ecomm - set ORG to EAG
           IF OHCODE='E';
             ORG='EAG';
           ENDIF;

           IF OIUNIT <> OISAV(SFLUPDR#).OIUNIT;
             CHAIN (OHNN:OHCUID:OHLOC) CSFMT;
             IF OHCODE = 'I';
               CSNN = *BLANKS;
               CSID = 0;
               CSORAX = *BLANKS;
               //get Org_Xref costs
               CHAIN(N) (OIPART:ORG:*BLANKS:0:*BLANKS) VOFMT;
             ELSE;
               //get SPECIFIC costs
               CHAIN(N) (OIPART:ORG:CSNN:CSID:CSORAX) VOFMT;
             ENDIF;
           ENDIF;

           IF OHCODE <> ' ';
             IF OHCODE = 'I';
               //get Org_Xref costs
               CHAIN(N) (OIPART:ORG:*BLANKS:0:*BLANKS) VOFMT;
             ELSE;
               //get SPECIFIC costs
               CHAIN(N) (OIPART:ORG:CSNN:CSID:CSORAX) VOFMT;
             ENDIF;

           // dont add/update paco or vendpart if ecomm and tpkey(trading partner) <> blanks
             // 06/20/13 per Wayne
             IF OHCODE <> 'E' OR (OHCODE = 'E' AND OHTPKEY = *BLANKS);
               // if PO D/C <> Vendpart DC of Orgin, update PACO only
               // else update vendpart and mastspec
               IF VRORIG <> OHDC;
                 IF NOT (FRTLIN = *ON OR
                         OIPART = CDSALES OR OIPART = PPSALES OR
                         %SUBST(OIPART:1:2)='ES' OR
                         OIPART = ESPROCS OR OIPART = CSRUSH);
                   IF NOT %FOUND(VENDPACO) OR VODEL = ' ';
                     UPDVO();
                   ENDIF;
                 ENDIF;
               // spot to add vendpart, masterspec update
               ELSE;
                 // update vendpart and master spec
                 IF NOT (FRTLIN = *ON OR
                         OIPART = CDSALES OR OIPART = PPSALES OR
                         %SUBST(OIPART:1:2)='ES' OR
                         OIPART = ESPROCS OR OIPART = CSRUSH);
                   UPDVP();
                 ENDIF;
               ENDIF;
             ENDIF;
           ENDIF;
         ENDIF;

         IF POTOT > POMAX AND
           (POTOT > SVPOTOT * 1.1 OR
            (NEWLIN = *ON AND OISTAT <> 'X') OR
            OIDDAT = OISAV(SFLUPDR#).OIDDAT);
           OHSTAT = 'H';
           SaveRecord();
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SaveLine;
         DCL-DS OICUR     LIKEREC(OIFMT) INZ;

         // Allowance for LOCKMSG logic
         DOU NOT %ERROR();
           IF NEWLIN = *ON;
             // Snapshot doesn't exist, so fail through to NOT %FOUND
             CHAIN(E) (OIYY:OI#:OIPP:OILIN#:OIPART) OIFMT OICUR;
           ELSE;
             // Pull from snapshot since OILIN# and/or OIPART can change!
             CHAIN(E) (OIYY:OI#:OIPP:OISAV(SFLUPDR#).OILIN#:
                       OISAV(SFLUPDR#).OIPART) OIFMT OICUR;
           ENDIF;
           IF %ERROR();
             LOCKMSG(STATUSS);
           ENDIF;
         ENDDO;
         OICHDT = %DATE();
         OICHTM = %TIME();
         OICHBY = USER;
         OIX = 'X';

         OICUR = OIDS;
         IF NOT %FOUND(ORITML5);
           IF NEWLIN = *ON;
             WRITE OIFMT OICUR;
           ELSE;
             SAVERR = *ON;
           ENDIF;
         ELSE;
           UPDATE OIFMT OICUR;
         ENDIF;
         UNLOCK ORITML5;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC PostSaveLine;
         // Old record is still in ...SAV

         // Send to SAP if already interfaced
         SETLL SPHPO# SAPHFMT;
         IF %EQUAL(SAPPOHDCP);
           SAPPOINT(OHYY:OHPP:OH#);
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC SaveLineText;
         DCL-DS OITCUR    LIKEREC(OITFMT) INZ;
         // Allowance for LOCKMSG logic
         DOU NOT %ERROR();
           CHAIN(E) (OIYY:OIPP:OI#:OILIN#:OIPART:*BLANKS) OITFMT OITCUR;
           IF %ERROR();
             LOCKMSG(STATUSS);
           ENDIF;
         ENDDO;
         OITUSE = USER;
         OITDAT = %DATE();
         IF NOT %FOUND(ORITX);
           OITYY = OHYY;
           OITPP = OHPP;
           OIT# = OH#;
           OITLN# = OILIN#;
           OITPRT = OIPART;
           OITSUB = *BLANKS;
           OITCUR = OITDS;
           WRITE OITFMT OITCUR;
         ELSE;
           OITCUR = OITDS;
           UPDATE OITFMT OITCUR;
         ENDIF;
         UNLOCK ORITX;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC CheckModifiedRecord;
         DCL-DS OHCUR     LIKEREC(OHFMT) INZ;
         DCL-DS OTCUR     LIKEREC(OTFMT) INZ;

         CHAIN(N) (OHYY:OH#) OHFMT OHCUR;
         IF OHCUR <> OHSAV;
           SAVERR = *ON;
         ENDIF;
         CHAIN(N) (OHYY:OHPP:OH#:*BLANKS) OTFMT OTCUR;
         IF OTCUR <> OTSAV;
           SAVERR = *ON;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC CheckModifiedLines;
         DCL-DS OICUR     LIKEREC(OIFMT) INZ;
         DCL-DS OITCUR    LIKEREC(OITFMT) INZ;
         SETLL (OHYY:OH#) OIFMT;
         FOR SFLR# = 1 TO 999;
           CLEAR OICUR;
           READE(N) (OHYY:OH#) OIFMT OICUR;
           IF %EOF(ORITML5);
             // Record was removed (somehow)
             IF SFLR# < SFLSIZ;
               SAVERR = *ON;
             ENDIF;
             LEAVE;
           ENDIF;
           // Record was added
           IF SFLR# > SFLSIZ;
             SAVERR = *ON;
             LEAVE;
           ENDIF;
           // Record was changed
           IF OICUR <> OISAV(SFLR#);
             SAVERR = *ON;
             LEAVE;
           ENDIF;
           CLEAR OITCUR;
           CHAIN(N) (OIYY:OIPP:OI#:OISAV(SFLR#).OILIN#:OISAV(SFLR#).OIPART:
             *BLANKS) OITFMT OITCUR;
           IF OITCUR <> OITSAV(SFLR#);
             SAVERR = *ON;
           ENDIF;
         ENDFOR;

         // Restore subfile position
         IF SFLUPDR# > 0;
           CHAIN SFLUPDR# SFLFMT;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC NewRecord;
         IF @OH# > 0;
           // For new records - save old parameters in case of cancel
           @OHYYSAV = @OHYY;
           @OH#SAV  = @OH#;
           @RETSAV  = @RET;
           @SIZSAV  = @SIZ;
         ENDIF;
         // Clear current PO from buffer
         CLEAR OHDS;
         CLEAR OTDS;
         CLEAR OIDS;
         CLEAR OITDS;
         // Defaults (use ...SAV to carry over)
         IF OHSAV.OHPP <> *BLANKS;
           OHPP = OHSAV.OHPP;
         ELSE;
           OHPP = UINN;
         ENDIF;
         OHCODE = 'I';
         OHSTAT = ' ';
         OHMETH = 'N';
         OHREQ = USER;
         OHRDTE = %DATE();
         // Clear ...SAV for reload
         CLEAR OHSAV;
         CLEAR OTSAV;
         CLEAR OISAV;
         CLEAR OITSAV;

         // Clear key and paging controls
         @OHYY = *BLANKS;
         @OH# = 0;
         @RET = 0;
         @SIZ = 0;
         SFLSIZ = 0;
         HIDEMSG = *ON;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC NewLine;
         SFLUPDR# = SFLSIZ + 1;
         // Clear current PO Line from buffer
         CLEAR SFLFMT;
         CLEAR OIDS;
         CLEAR OITDS;
         // Add new record to ...SAV
         OISAV(SFLUPDR#) = OIDS;
         OITSAV(SFLUPDR#) = OITDS;
         // Defaults (use ...SAV on other records to carry over)

         // Create new subfile line
         // IMPORTANT do not increase SFLSIZ, breaks CheckModifiedLines()
         SFLR# = SFLUPDR#;
         WRITE SFLFMT;
         HIDEMSG = *ON;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       DCL-PROC ShowLineText;
         DCL-PI *N IND;
         END-PI;
         DCL-S OITPEND    IND;
         DCL-S X          INT(5);
         NOTEHDR = 'Notes for line ' + %CHAR(OILIN#) + ' ' + OIPART;
         IF OITXTDIS = *OFF;
           NOTECLRDIS = *OFF;
           NOTEDFTDIS = *OFF;
           IF OITXTUC = *BLANKS;
             // Set default values
             FOR X = 1 TO 8;
               OITXTU(X) = 'E';
             ENDFOR;
             IF OISKU <> *BLANKS;
               DefaultLineText();
             ENDIF;
             OITPEND = *ON;
           ENDIF;
         ELSE;
           NOTECLRDIS = *ON;
           NOTEDFTDIS = *ON;
           OITDS = OITSAV(SFLR#);
         ENDIF;
         DOU NOTEBACK = *ON;
           EXFMT NOTEFMT;
           IF OITFMTCHG <> *OFF;
             OITPEND = *ON;
           ENDIF;
           SELECT;
           WHEN NOTECLR = *ON;
             FOR X = 1 TO 8;
               OITXT(X) = *BLANKS;
             ENDFOR;
             OITPEND = *ON;
           WHEN NOTEDFT = *ON;
             DefaultLineText();
             OITPEND = *ON;
           ENDSL;
         ENDDO;
         SFLNOTE = *OFF;
         RETURN OITPEND;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       // Pull default notes from Vendor by Part
       DCL-PROC DefaultLineText;
         DCL-S X          INT(5);
         DCL-S Y          INT(5);
         SETLL (OISKU: OHDC: OHVEND) VYFMT;
         DOW 'X'='X';
           READE(N) (OISKU: OHDC: OHVEND) VYFMT;
           IF %EOF(VENDPABL6) OR VYDEL <> 'D';
             LEAVE;
           ENDIF;
         ENDDO;

         // Vendor OEM
         // IF VYDESC <> VQDESC, move to first available po text line
         IF NOT %EOF(VENDPABL6) AND VYDEL <> 'D';
           IF VYDESC <> *BLANKS AND VYDESC <> VQDESC;
             FOR X = 1 TO 8;
               IF OITXT(X)= VYDESC;
                 LEAVE;
               ELSE;
                 IF OITXT(X)=*BLANKS;
                   OITXT(X)=VYDESC;
                   LEAVE;
                 ENDIF;
               ENDIF;
             ENDFOR;
           ENDIF;
           // Vendor Description
           IF VYDESC2 <> *BLANKS;
             FOR X = 1 TO 8;
               IF OITXT(X)= VYDESC2;
                 LEAVE;
               ELSE;
                 IF OITXT(X)=*BLANKS;
                   OITXT(X)=VYDESC2;
                   LEAVE;
                 ENDIF;
               ENDIF;
             ENDFOR;
           ENDIF;
           // vendor Text  (VYXT~)
           FOR X = 1 TO 4;
             IF VYXT(X) <> *BLANKS;
               FOR Y = 1 TO 8;
                 SELECT;
                 WHEN OITXT(Y) = VYXT(X);
                   LEAVE;
                 WHEN OITXT(Y) = *BLANKS;
                   OITXT(Y) = VYXT(X);
                   LEAVE;
                 ENDSL;
               ENDFOR;
             ENDIF;
           ENDFOR;
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //

       // ------------------------------------------------------------------- //
       // Copy a Purchase Order to new PO#
       DCL-PROC CopyPO;
         DCL-S X          INT(5);
         DCL-S OH#P       PACKED(6); // For conversion into CXOHPP

         @OHYY = *BLANKS;
         @OH# = 0;
         @RET = 0;
         @SIZ = 0;
         NEWREC = *ON;
         // copy header
         //put copied PO on hold
         OHSTAT = 'H';
         OHREQ = USER;
         OHRDTE = %DATE();
         OHMETH = 'N';
         OHEDOC = *ZEROS;
         OHTPKEY = *BLANKS;
         OHOBGRP = *LOVAL;
         OHOBACT = *LOVAL;
         // disallow use of customer fields for inventory POs....affects EDI enabled vendors
         IF OHCODE = 'I' AND OHDC<>'MS';
           OHNN = *BLANKS;
           OHCUID = *ZEROS;
           OHLOC = *BLANKS;
         ENDIF;
         PreSaveRecord();  // Will set OHYY, OH#
         IF SAVERR = *ON;
           RETURN;
         ENDIF;
         SaveRecord();
         OHSAV = OHDS; // Update snapshot early for reuse

         //copy header text
         OHT# = OH#;
         OHTYY = OHYY;
         OHTCX = *BLANK;
         SaveHeaderText();
         OTSAV = OTDS; // Update snapshot early for reuse

         // copy po lines (calculate due date from "today")
         NEWLIN = *ON;
         FOR SFLUPDR# = 1 TO SFLSIZ;
           OIDS = OISAV(SFLUPDR#);
           IF OISTAT='X';
             ITER;
           ENDIF;
           OI# = OH#;
           OIYY = OHYY;
           OISTAT = ' ';
           OIRQTY = *ZEROS;
           CHAIN(N) OIPART VRFMT;
           IF NOT (OIPART = 'CSFREIGHTCHARGES' OR
                   OIPART = 'CSFREIGHTFSC' OR
                   OIPART = 'ESFREIGHT' OR
                   OIPART = 'CSTARIFF');
             OIDDAT = %DATE() + %DAYS(VRLEAD);
           ELSE;
             // For freight, find lowest due date
             OIDDAT = *HIVAL;
             FOR X = 1 TO SFLUPDR# - 1;
               IF OISAV(X).OIDDAT < OIDDAT;
                 OIDDAT = OISAV(X).OIDDAT;
               ENDIF;
             ENDFOR;
             IF OIDDAT = *HIVAL;
               OIDDAT = %DATE();
             ENDIF;
           ENDIF;
           OIDDATCHG = *ON;
           OIREQ = *ZEROS;
           CLEAR OIWSNN;
           CLEAR OIWSJOB;
           CLEAR OIWSYMD;
           CLEAR OIWSLOC;
           CLEAR OIWSTIME;
           SaveLine();
           OISAV(SFLUPDR#) = OIDS; // Update snapshot early for reuse

           // write lines text
           OITDS = OITSAV(SFLUPDR#);
           OIT# = OH#;
           OITYY = OHYY;
           OITCX = *BLANK;
           SaveLineText();
           OITSAV(SFLUPDR#) = OITDS; // Update snapshot early for reuse
         ENDFOR;
         NEWLIN = *OFF;
         SFLUPDR# = 0;
         // after copy, ensure OHPP set properly
         OH#P = OH#;
         CXOHPP(OHYY:OHPP:OH#P:OHPP);

         // display "error" info message - copy successful
         CTLFMTERR = *ON;
         CTLFMTERM = 'Copy successful, New PO# is PR' + OHYY + %CHAR(OH#);
         NEWREC = *OFF;
       END-PROC;
       // ------------------------------------------------------------------- //
       // Print/send a Purchase Order
       DCL-PROC SendPO;
         DCL-S OH#P       PACKED(6); // For conversion into CXOHPP
         DCL-S @SEND      CHAR(1);
         DCL-S LEN        ZONED(2);
         DCL-S SVMETH     LIKE(OHMETH);

         IF OHSTAT <> 'H' OR (OHCODE <> 'E' OR (OHCODE = 'E' AND OHDC <> 'DS'));
           // email PDF
           UNLOCK ORHDRL4;
           // approval only no po transmit
           @SEND = ' ';
           IF OHSTAT = 'H' OR USRMODE = 4;
             // set default value based on existence of vendor email addr
             IF VNEMAL <> *BLANKS;
               @SEND = '3';
             ELSE;
               @SEND = '4';
             ENDIF;
             // determine what to do with PO (1 2 or 3)
             //      APPOAPVCL(@SEND: USER);
             ASKHEADER = 'Transmit PO';
             ASKTEXT = 'Select the desired PO Approval option:';
             ASKCHOICE = '1 Approve & Send PDF to Requestor,+
                          2 Approve & Send PDF to Yourself,+
                          3 Approve & Send PDF to Vendor/Yourself,+
                          4 Approve only,+
                          9 Exit';
             ASKVALUE = '1,2,3,4,9';
             ASKRETURN = @SEND; // Default
             EXFMT ASKFMT;
             @SEND = ASKRETURN;
           ENDIF;
           IF @SEND = '9';
             RETURN;
           ENDIF;
           IF @SEND <> '4';
             OH#P = OH#;
             APPOPRTCL(OHYY: OHPP: OH#P: OHDC: OHMETH: @SEND: OHSTAT);
             IF @SEND = '9';
               RETURN;
             ENDIF;
           ELSE;
             IF OHMETH <> 'Y';
               SVMETH = 'A';
             ENDIF;
           ENDIF;
         ENDIF;
         //if held from ORLOAD,
         IF OHSTAT = 'H' OR SVMETH = 'A';
           //release to EDI
           OHSTAT = *BLANKS;
           IF SVMETH = 'A' AND @SEND = '3';
             OHMETH = SVMETH;
           ENDIF;
           SaveRecord();
         ENDIF;

         IF OHSTAT=' ' AND
           (OHCODE='I' OR (OHCODE='D' AND OHNN='94' AND
           OHCUID=3915 AND OHLOC='ATL')); //Storopack
           @WH = *BLANKS;
           // loop thru PO lines
           FOR SFLUPDR# = 1 TO SFLSIZ;
             OIDS = OISAV(SFLUPDR#);
             IF OHSAV.OHMETH = 'N' AND
               (OHVEND=7805 OR OHVEND=7751 OR OHVEND=7750);
               // for LTM vendor,
               IF @WH = *BLANKS;
                 // .. prompt to allow user to pick supply site
                 WHSETCL(@WH);
               ENDIF;
               IF @WH<>'X' AND OISTAT=' ' AND
                 (OHCODE='I' AND OIWSJOB=0 OR OHCODE='D');
                 CRTWIP();
                 IF SAVERR = *ON;
                   RETURN;
                 ENDIF;
                 // set appropriate completion message
                 IF WIPREC = *ON;
                   CTLFMTERR = *ON;
                   CTLFMTERM = 'Jobs successfully added to WIP';
                 ELSE;
                   CTLFMTERR = *ON;
                   CTLFMTERM = 'No jobs created: alredy in WIP';
                 ENDIF;
               ENDIF;
             ENDIF;
           ENDFOR;
           SFLUPDR# = 0;
         ENDIF;

         IF OHSTAT = ' ' AND OHCODE <> 'E' AND OHCODE <> 'D' AND OHDC <> 'MS';
           // NOTE: exclude ecom, since requires unique ship to id for HRMS and would exceed 7,569 l
           //       quickly
           //       also exclude drop ship...same issue
           //       also exclude MS...same issue
           // check for need to send internal PO via EDI
           L0LAKT = 'INT';
           L0LAK = %TRIM(%EDITW(OHVEND:'0      '));
           CHAIN (L0LAKT:L0LAK) INTEDI;
           IF NOT %FOUND(LOCXREFLF) OR L0DEL = 'D';
             RETURN;
           ENDIF;
           // warn if attempt to retransmit EDI
           IF OHTPKEY <> *BLANKS;
             CTLFMTERR = *ON;
             CTLFMTERM = 'EDI: contact vendor with changes';
             RETURN;
           ENDIF;
           // initial transmit requested, determine EDI trading partner key
           L0LAKT = 'TPK';
           CHAIN (L0LOC:L0LAKT) L0FMT;
           IF NOT %FOUND(LOCXREF);
             CLEAR L0LAK;
           ENDIF;
           // if BVP trading partner, check for part# exceeding 20-char limit
           IF %SUBST(L0LAK:1:4) = 'BVP ';
             OH#P = OH#;
             OILEN(OHYY:OHPP:OH#P:LEN);
             // ..when found, skip EDI and process manually
             IF LEN > 20;
               CTLFMTERR = *ON;
               CTLFMTERM = 'EDI, but item length forces manual PO.';
               RETURN;
             ENDIF;
           ENDIF;
           OHTPKEY = 'RTL2' + %SUBST(L0LAK:1:4);
           SaveRecord();
           CTLFMTERR = *ON;
           CTLFMTERM = 'EDI: PO flagged for transmission.';
           RETURN;
         ENDIF;
       END-PROC;
       //----------------------------------------------------------------
       // subr to create jobs for LTM
       DCL-PROC PO2WIP;
         IF VFYERR = *ON;
           RETURN;
         ENDIF;
         // .. prompt to allow user to pick supply site
         WHSETCL(@WH);
         IF @WH = 'X';
           RETURN;
         ENDIF;
         WIPREC = *OFF;
         // loop thru PO lines
         FOR SFLUPDR# = 1 TO SFLSIZ;
           OIDS = OISAV(SFLUPDR#);
           SELECT;
           WHEN (OIPART = CSFRGHT OR OIPART = ESFRGHT OR
           OIPART = CSTARIF OR OIPART = CSFRFSC) OR
             OIWSJOB <> 0 OR OISTAT <> ' ';
           // ....skip freight, lines that have already driven jobs, inactive lines
             ITER;
           ENDSL;
           CRTWIP();
           IF CTLFMTERR = *ON;
             RETURN;
           ENDIF;
         ENDFOR;
         SFLUPDR# = 0;
         // set appropriate completion message
         IF WIPREC = *ON;
           CTLFMTERR = *ON;
           CTLFMTERM = 'Jobs successfully added to WIP';
         ELSE;
           CTLFMTERR = *ON;
           CTLFMTERM = 'No jobs created: alredy in WIP';
         ENDIF;
       END-PROC;
       // ------------------------------------------------------------------- //
       DCL-PROC CRTWIP;
         // get related info
         CHAIN(N) OIPART VRFMT;
         CHAIN(N) (VRNN:VRID:VRIT) MQFMT;

         CLEAR WPFMT;
         CLEAR WSFMT;
         // Prime values for WipJob
         WPDEL = 'D';
         WPRORN = 'R';
         WPADD = 'A';
         WPX = 'X';
         WPUSER = USER;
         WPENTD = %DEC(%CHAR(%DATE():*CYMD0):7:0);
         WPCYMD = WPENTD;
         WPCHMS = %DEC(%CHAR(%TIME():*HMS0):7:0);
         // ....get SUPPLY  whs   info
         SELECT;
         WHEN OHVEND = 7805;                                                  //LTM
           WPNN = '95';
         WHEN OHVEND = 7751;                                                  //DCM
           WPNN = '97';
         WHEN OHVEND = 7750;                                                  //PEM
           WPNN = '98';
         WHEN OHVEND = 7799;                                                  //EAG
           WPNN = '94';
         OTHER;
         ENDSL;
         // ....get ship to whs   info
         CHAIN(N) OHPP COAFMT;
         WPID = LCACCT;
         WPIT = VRIT;
         WPNNOV = VRNN;
         WPIDOV = VRID;
         WPITOV = VRIT;
         WPORDQ = OIQTY;
         WPUSEX = MQUSEX;
         WPOLDD = %DEC(%CHAR(OIDDAT:*CYMD0):7:0);
         WPNEWD = WPOLDD;

         // Prime values for WipShip
         WSSCH2 = 'F';
         WSX = 'X';
         WSUSER = USER;
         WSCYMD = WPCYMD;
         WSCHMS = WPCHMS;
         WSNN = WPNN;
         WSWH = @WH;
         WSLOCV = ORG; //from BusA of PO
         IF OHCODE = 'D';
           WSLOC = OHLOC; //CS/IA must align
         ELSE;
           // for inventory POs, will need the ORG for ship-to site
           CHAIN(N) OHDC COAFMT;
           WSLOC = ORG;
         ENDIF;
         // validate target location exists
         CHAIN (WPNN:WPID:WSLOC) CSFMT;
         IF NOT %FOUND(CUSTSHPP) OR CSDEL='D';
           CTLFMTERR = *ON;
           CTLFMTERM = 'No jobs created: bad BusA?';
           RETURN;
         ENDIF;
         WSYMD = WPOLDD;
         WSID = WPID;
         WSIT = WPIT;
         WSQTY = OIQTY;
         WSPRIQ = OIQTY;
         WSPO = PONUM;
         WSLIN# = %TRIM(%EDITC(OILIN#:'3'));
         WSQTY = OIQTY;
         SELECT;
         WHEN OIUNIT < 100 AND OIUNIT > %DEC(OIUNIT:12:2); // Do we have value at .00XXX
           WSPCOD = 1;
           WSP1 = OIUNIT * 1000;
         OTHER;
           WSPCOD = 5;
           WSP1 = OIUNIT * 1;
         ENDSL;
         WSQ1 = OIQTY;

         CHAIN WPNN COAFMT;
         DOW 1 = 1;
           IF COAJOB + 1 < 99999;
             COAJOB += 1;
           ELSE;
             COAJOB = 100;
           ENDIF;
           SETLL (WPNN:COAJOB) WPFMT;
           IF NOT %EQUAL(WIPJOBS);
             SETLL (WPNN:COAJOB) WPHIST;
           ENDIF;
           IF %EQUAL(WIPJOBS) OR %EQUAL(WIPJOBH);
             ITER;
           ENDIF;
           LEAVE;
         ENDDO;
         UPDATE COAFMT;

         WPJOB = COAJOB;
         WSJOB = WPJOB;
         WRITE WSFMT;
         WRITE WPFMT;
         WIPREC = *ON;
         // prime placeholder record for owner
         IF WSWH <> WSNN;
           WSWH = WSNN;
           WSDEL = 'D';
           WSX = 'O';
           WRITE WSFMT;
         ENDIF;
         // update OI w/new values of WSkey
         IF OHCODE = 'I';
           OIWSNN = WSNN;
           OIWSJOB = WSJOB;
           OIWSYMD = WSYMD;
           OIWSLOC = WSLOC;
           OIWSTIME = WSTIME;
         ENDIF;
         SaveLine();
       END-PROC;
       //----------------------------------------------------------------
       DCL-PROC UPDVY;
         DCL-S X          INT(5);
         DCL-S Y          INT(5);
         DCL-S ACI        PACKED(5:4);
         DCL-S @VYDEL     CHAR(1);

         // Determine if a different vendor is primary for this DC/Itm
         %OCCUR(VYDS) = 2;
         RESET @VYDEL;
         SETLL (OISKU:OHDC) VYFMT;
         DOW 'X' = 'X';
           READE (OISKU:OHDC) VYFMT VYDS;
           IF %EOF(VENDPABL6) OR (VYDEL = ' ' AND OHVEND <> VYVEND);
             LEAVE;
           ENDIF;
         ENDDO;
         IF NOT %EOF(VENDPABL6);
           // ..found, prompt to Allow user to mark new vendor as primary or alternate
           //    PABYALTCL(@VYDEL:OHVEND);
           ASKHEADER = 'Replace primary vendor?';
           ASKTEXT = 'A different vendor is already primary for this Part. +
                      Do you want to replace the primary vendor with this one?';
           ASKCHOICE = '_ Set the new vendor as PRIMARY,+
                        A Use the new vendor as ALTERNATE,+
                        X Exit without making changes';
           ASKVALUE = ',A,X';
           ASKRETURN = 'A'; // Default
           EXFMT ASKFMT;
           @VYDEL = ASKRETURN;
           // ..if setting new vendor as primary, mark old primary as alternate
           IF @VYDEL = ' ';
             VYDEL = 'A';
             VYUSER = 'PR' + OHYY + %TRIM(%EDITW(OI#:'0      '));
             VYCHGD = %DATE();
             UPDATE VYFMT VYDS;
           ENDIF;
         ENDIF;
         // write vendor/item data to vendpaby if not already there
         %OCCUR(VYDS) = 1;
         CHAIN (OISKU:OHDC:OHVEND) VYFMT VYDS;
         IF NOT %FOUND(VENDPABL6) OR VYCCOD = *ZEROS;
           IF NOT %FOUND(VENDPABL6);
             RESET VYDS;
             VYPART = OIPART;
             VYVEND = OHVEND;
             VYSKU = OISKU;
             VYUOM = OIUOM;
           ENDIF;
           VYDC = OHDC;
           CHAIN(N) (OIPART:ORG:*BLANKS:0:*BLANKS) VOFMT;
           IF NOT %FOUND(VENDPACO) OR VODEL='D';
             // prime paby cost from vendpart
             CHAIN(N) OIPART VRFMT;
             VYQ   = QTY;
             VYC   = CST;
             VYF   = VRF;
             VYA   = VRA;
             VYCCOD = VRCCOD;
             VYEXPD = VREXPD;
             VYOVHD = VROVHD;
             VYSRCL = VRSRCL;
             VYSRCU = VRSRCU;
           ELSE;
             // prime paby cost from vendpaco
             VYQ   = VOQ;
             VYC   = VOC;
             VYF   = VOF;
             VYA   = VOA;
             VYCCOD = VOCCOD;
             VYEXPD = VOEXPD;
             VYOVHD = VOOVHD;
             VYSRCL = VOSRCL;
             VYSRCU = VOSRCU;
           ENDIF;
         ENDIF;
         Y = 1;
         //replace vendor by part cost with PO cost
         SELECT;
         WHEN VYCCOD = 1 OR VYCCOD = 4 OR VYCCOD = 5;
           FOR Y = 1 TO 5;
             SELECT;
             WHEN OIQTY = VYQ(Y);
               LEAVE;
             WHEN VYQ(Y) = *ZEROS OR OIQTY < VYQ(Y);
               IF Y > 1;
                 Y -= 1;
               ENDIF;
               LEAVE;
             ENDSL;
           ENDFOR;
           // if didn't find qualifying cost, use last bucket
           IF Y = 6;
             Y -= 1;
           ENDIF;
           IF VYCCOD = 1 OR VYCCOD = 4;
             EVAL(H) VYC(Y) = OIUNIT * 1000;
           ELSE;
             VYC(Y) = OIUNIT;
           ENDIF;
         WHEN VYCCOD = 6;
           EVAL(H) VYC(Y) = OIUNIT * 1000;
         WHEN VYCCOD = 7;
           VYC(Y) = OIUNIT;
         WHEN VYCCOD = 10 AND OIUNIT <> 0;
           VYC(Y) = OIUNIT;
         WHEN VYCCOD = 10 AND OIUNIT = 0;
           OIUNITERR = *ON;
           OIUNITERM = 'Cost code 10, Must supply a cost > 0';
           OIUNITERF = ERFERROR;
           SAVERR = *ON;
           RETURN;
         ENDSL;
         // update vendpaby applied cost
         ACI = 1;
         IF VREFFD > %DATE() AND VRPCT > 0;
           ACI = 1 + VRPCT;
         ENDIF;
         //                  ... Translate costs per base unit to Applied costs
         VYA = 0;
         FOR X = 1 TO 5;
           IF X > 1 AND VYQ(X) = 0;
             LEAVE;
           ENDIF;
           //                  ... Ovhd% + Advance Cost Increase + Unit based Ovhd
           EVAL(H) VYA(X) = VYC(X) * ACI * (VYOVHD + 1) + VYSRCU;
           //                  ... Lot based Ovhd
           IF VYSRCL > 0 OR VYF(X) > 0;
             SELECT;
             WHEN VYCCOD = 10;
               VYA(X) = VYA(X) + (VYSRCL+VYF(X));
             WHEN VYCCOD = 1 OR VYCCOD = 6;
               EVAL(HR) VYA(X) = VYA(X) + (VYSRCL + VYF(X)) / VYQ(X) * 1000;
             OTHER;
               EVAL(H) VYA(X) = VYA(X) + (VYSRCL+VYF(X)) / VYQ(X);
             ENDSL;
           ENDIF;
         ENDFOR;
         VYUSER = 'PR' + OHYY + %TRIM(%EDITW(OI#:'0      '));
         VYCHGD = %DATE();
         VYDEL = @VYDEL;
         IF NOT %FOUND(VENDPABL6);
           WRITE VYFMT VYDS;
         ELSE;
           UPDATE VYFMT VYDS;
         ENDIF;
         UNLOCK VENDPABL6;
       END-PROC;
       // ------------------------------------------------------------------- //
       DCL-PROC UPDVO;
         DCL-S X          INT(5);
         DCL-S Y          INT(5);
         DCL-S ACI        PACKED(5:4);

         // lock for update
         IF OHCODE = 'I';
           VONN   = *BLANKS;
           VOID   = 0;
           VOORAX = *BLANKS;
           CSNN   = *BLANKS;
           CSID   = 0;
           CSORAX = *BLANKS;
         ELSE;
           CHAIN (OHNN:OHCUID:OHLOC) CSFMT;
           VONN   = CSNN;
           VOID   = CSID;
           VOORAX = CSORAX;
         ENDIF;
         IF OHCODE = 'I';
           //get Org_Xref costs
           CHAIN (OIPART:ORG:*BLANKS:0:*BLANKS) VOFMT VODS;
         ELSE;
           //get SPECIFIC costs
           CHAIN (OIPART:ORG:CSNN:CSID:CSORAX) VOFMT VODS;
         ENDIF;
         //... prime new record if missing
         IF NOT %FOUND(VENDPACO);
           CHAIN(N) OIPART VRFMT;
           VOPART = VRPART;
           VOOO   = ORG;
           IF OHCODE='I';
             VONN   = *BLANKS;
             VOID   = 0;
             VOORAX = *BLANKS;
             VOLOC  = *BLANKS;
           ELSE;
             VONN   = CSNN;
             VOID   = CSID;
             VOORAX = CSORAX;
             VOLOC  = CSLOC;
           ENDIF;
           IF NOT(VRCCOD=6 OR VRCCOD=7);
             VOCCOD = VRCCOD;
           ELSE;
             VOCCOD = 5;
           ENDIF;
           VOQ    = QTY;
           VOC    = CST;
           VOA    = VRA;
           VOF    = VRF;
           VOOVHD = VROVHD;
           VOSRCL = VRSRCL;
           VOSRCU = VRSRCU;
         ENDIF;
         // .. always update these values
         VODEL  =' ';
         IF OIDDAT = *LOVAL;
           VOEXPD = %DEC(%CHAR(%DATE() + %DAYS(7):*CYMD0):7:0);
         ELSE;
           VOEXPD = %DEC(%CHAR(OIDDAT + %DAYS(7):*CYMD0):7:0);
         ENDIF;
         Y = 1;
         //replace proper override cost with PO cost
         SELECT;
         WHEN VOCCOD = 1 OR VOCCOD = 4 OR VOCCOD = 5;
           FOR Y = 1 TO 5;
             SELECT;
             WHEN OIQTY = QTY(Y);
               LEAVE;
             WHEN QTY(Y) = *ZEROS OR OIQTY < QTY(Y);
               IF Y > 1;
                 Y -= 1;
               ENDIF;
               LEAVE;
             ENDSL;
           ENDFOR;
           // if didn't find qualifying cost, use last bucket
           IF Y = 6;
             Y -= 1;
           ENDIF;

           IF VOCCOD=1 OR VOCCOD=4;
             EVAL(H) VOC(Y) = OIUNIT * 1000;
           ELSE;
             VOC(Y) = OIUNIT;
           ENDIF;
         WHEN VOCCOD = 6;
           EVAL(H) VOC(Y) = OIUNIT * 1000;
         WHEN VOCCOD = 7;
           VOC(Y) = OIUNIT;
         WHEN VOCCOD = 10 AND OMEST$ <> 0;
           VOC(Y) = OIUNIT;
         WHEN VOCCOD = 10 AND OIUNIT = 0;
           OIUNITERR = *ON;
           OIUNITERM = 'Cost code 10, Must supply a cost > 0';
           OIUNITERF = ERFERROR;
           SAVERR = *ON;
           RETURN;
         ENDSL;
         ACI = 1;
         IF VREFFD > %DATE() AND VRPCT > 0;
           ACI = 1 + VRPCT;
         ENDIF;
         // added new calcs for applied costs
         //                  ... Translate costs per base unit to Applied costs
         VOA = 0;
         // added 7/31/2013 dont add frt cost into "DS" paco record
         IF OHDC = 'DS';
           VOF = *ZEROS;
         ENDIF;
         FOR X = 1 TO 5;
           IF X > 1 AND VOQ(X) = 0;
             LEAVE;
           ENDIF;
           //                  ... Ovhd% + Advance Cost Increase + Unit based Ovhd
           EVAL(H) VOA(X) = VOC(X) * ACI * (VOOVHD + 1) + VOSRCU;
           //                  ... Lot based Ovhd
           IF VOSRCL > 0 OR VOF(X) > 0;
             SELECT;
             WHEN VOCCOD = 10;
               VOA(X) = VOA(X) + (VOSRCL + VOF(X));
             WHEN VOCCOD=1 OR VOCCOD=6;
               EVAL(HR) VOA(X) = VOA(X) + (VOSRCL + VOF(X)) / VOQ(X) * 1000;
             OTHER;
               EVAL(H) VOA(X) = VOA(X) + (VOSRCL + VOF(X)) / VOQ(X);
             ENDSL;
           ENDIF;
         ENDFOR;
         VOX    = 'X';
         VOUSER = 'PR' + OHYY + %TRIM(%EDITW(OH#:'0      '));
         VOCYMD = %DEC(%CHAR(%DATE():*CYMD0):7:0);
         IF %FOUND(VENDPACO);
           UPDATE VOFMT VODS;
         ELSE;
           MONITOR;
             WRITE VOFMT VODS;
           ON-ERROR;
             OIUNITERR = *ON;
             OIUNITERM = 'Cost Override exists for non-DRP org???';
             OIUNITERF = ERFERROR;
             SAVERR = *ON;
             RETURN;
           ENDMON;
         ENDIF;
         UNLOCK VENDPACO;
       END-PROC;
       // ------------------------------------------------------------------- //
       DCL-PROC UPDVP;
         DCL-S X          INT(5);
         DCL-S Y          INT(5);
         DCL-S ACI        PACKED(5:4);

         //get Org_Xref costs
         CHAIN OIPART VRFMT VRDS;
         //replace proper vendpart cost with PO cost
         SELECT;
         WHEN VRCCOD = 1 OR VRCCOD = 4 OR VRCCOD = 5;
           FOR Y = 1 TO 5;
             SELECT;
             WHEN OIQTY = QTY(Y);
               LEAVE;
             WHEN QTY(Y) = *ZEROS OR OIQTY < QTY(Y);
               IF Y > 1;
                 Y -= 1;
               ENDIF;
               LEAVE;
             ENDSL;
           ENDFOR;
           // if didn't find qualifying cost, use last bucket
           IF Y = 6;
             Y -= 1;
           ENDIF;
           IF VRCCOD = 1 OR VRCCOD = 4;
             EVAL(H) CST(Y) = OIUNIT * 1000;
           ELSE;
             CST(Y) = OIUNIT;
           ENDIF;
         WHEN VRCCOD = 6;
           EVAL(H) CST(Y) = OIUNIT * 1000;
         WHEN VRCCOD = 7;
           CST(Y) = OIUNIT;
         WHEN VRCCOD = 10 AND OIUNIT <> 0;
           CST(Y) = OIUNIT;
         WHEN VRCCOD = 10 AND OIUNIT = 0;
           OIUNITERR = *ON;
           OIUNITERM = 'Cost code 10, Must supply a cost > 0';
           OIUNITERF = ERFERROR;
           SAVERR = *ON;
           RETURN;
         ENDSL;
         ACI = 1;
         IF VREFFD > %DATE() AND VRPCT > 0;
           ACI = 1 + VRPCT;
         ENDIF;
         // added new calcs for applied costs
         // Translate costs per base unit to Applied costs
         VRA = 0;
         FOR X = 1 TO 5;
           IF X > 1 AND QTY(X) = 0;
             LEAVE;
           ENDIF;
           // Ovhd% + Advance Cost Increase + Unit based Ovhd
           EVAL(H) VRA(X) = CST(X) * ACI * (VROVHD + 1) + VRSRCU;
           // Lot based Ovhd
           IF VRSRCL > 0 OR VRF(X) > 0;
             SELECT;
             WHEN VRCCOD = 10;
               VRA(X) = VRA(X) + (VYSRCL + VRF(X));
             WHEN VRCCOD=1 OR VRCCOD=6;
               EVAL(HR) VRA(X) = VRA(X) + (VRSRCL + VRF(X)) / QTY(X) * 1000;
             OTHER;
               EVAL(H) VRA(X) = VRA(X) + (VRSRCL + VRF(X)) / QTY(X);
             ENDSL;
           ENDIF;
           // update mastspec cost with first applied cost
           IF X = 1;
             MQUPD();
           ENDIF;
         ENDFOR;
         VRUSER = 'PR' + OHYY + %TRIM(%EDITW(OI#:'0      '));
         VRCYMD = %DEC(%CHAR(%DATE():*CYMD0):7:0);
         IF %FOUND(VENDPART);
           UPDATE VRFMT VRDS;
         ENDIF;
         UNLOCK VENDPART;
       END-PROC;
       //----------------------------------------------------------------
       DCL-PROC MQUPD;
         DCL-S COST       PACKED(15:5);

         CHAIN (VRNN:VRID:VRIT) MQFMT;
         //                  ..... qualify Part COST with mq-qualifiers
         COST = VRA(1);
         IF MQMLT > 1;
           EVAL(H) COST /= MQMLT;
         ENDIF;
         IF MQ#PCS > 1;
           COST *= MQ#PCS;
         ENDIF;
         IF MQ#OUT > 1;
           EVAL(H) COST /= MQ#OUT;
         ENDIF;
         IF MQUSEX = 1;
           COST *= MQUSEX;
         ENDIF;
         EVAL(H) MQCOST = COST * 1;
         SELECT;
         WHEN VRCCOD = 1 OR VRCCOD = 6;
           MQCCOD = 6;
         WHEN VRCCOD = 5 OR VRCCOD = 7;
           IF COST >= 1;
             MQCCOD = 7;
           ELSE;
             MQCOST = COST * 1000;
             MQCCOD = 6;
           ENDIF;
         OTHER;
           MQCCOD = VRCCOD;
         ENDSL;
         MQX = 'X';
         MQUSER =' PR' + OHYY + %TRIM(%EDITW(OH#:'0      '));
         MQSYMD = %DEC(%CHAR(%DATE():*CYMD0):7:0);
         IF %FOUND(MASTSPEP);
           UPDATE MQFMT;
         ENDIF;
         UNLOCK MASTSPEP;
       END-PROC;
