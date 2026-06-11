     h**********************************************************************
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
      **********************************************************************
      * Created: 7/20/2020 C:079 Project: WC2749
      **********************************************************************
      * Revisions:
      *
      **********************************************************************
     Fwmevml1   IF   E             disk
     Fwtgtl     if   e           k Disk
     Fsmupl     if   e           k Disk
      * Entry
     d*
     d* Prototypes
     d*
     dGetVPayDetail    pr                  ExtPgm('WTEPAYVMT2')
     d                                9  0
     d                            32767
     d                                1
     dGetVPayDetail    pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1
     d*
     d* Data Structures
     d*
     d psds          esds                  extname(szq1p)
     d*
     d* Standalone fields
     d*
     d xxDate8         s              8  0
     d xxDate          s               d
     d scDate          s             10a
     d xxFormat        s             25a
     d xxPtyp          s             30a
     d xxAppl          s             30a
     d xxCo#           s                   Like(UPco#)
     d*
     d* Copybooks
     d*
      /copy sPrAtrium
      /copy sPrDate

      /free
       //==================================================================
       // Main program to load detail box
       //==================================================================
       outJSON  = ' ';

       Chain (Q1user) pup01;
       If %Found(smupl);
          xxCo# = Upco#;
       Else;
          xxCo# = 0;
       Endif;

       Chain(n) inRRN pevm01;
          If %Found(WMEVML1);
             // Vendor name
             BuildJSON(outJSON:'OutDtlEpayVendor':EVMvend);

             // Vendor format
             Chain (xxCo#:'EPFM':EVMfmt) pgt01;
             If %Found(WTGTL);
                xxFormat = %Trim(EVMfmt) + ' - ' + %Trim(GTdesc);
             Else;
                xxFormat = *Blanks;
             Endif;
             BuildJSON(outJSON:'OutDtlEpayVendorFormat':xxFormat);

             // Vendor payment type
             Chain (xxCo#:'EPPT':EVMpTyp) pgt01;
             If %Found(WTGTL);
                xxPtyp = %Trim(EVMpTyp) + ' - ' + %Trim(GTdesc);
             Else;
                xxPtyp = *blanks;
             Endif;
             BuildJSON(outJSON:'OutDtlEpayVendorPaymentType':xxPtyp);

             // Vendor application code
             Chain (xxCo#:'BTYP':EVMappl) pgt01;
             If %Found(WTGTL);
                xxAppl = %Trim(EVMappl) + ' - ' + %Trim(GTdesc);
             Else;
                xxAppl = *blanks;
             Endif;
             BuildJSON(outJSON:'OutDtlEpayVendorApplicationCode':xxAppl);

             If EVMefDt <> 0;
                xxDate8 = DtMdYY(EVMefDt);
                xxDate = CvtToDate(xxDate8);
                scdate = AlphaUSA(xxdate);
             Else;
                scDate = *blanks;
             Endif;
             BuildJSON(outJSON:'OutDtlEPayEffectiveDate':scdate);

             If EVMexDt <> 0;
                xxDate8 = DtMdYY(EVMexDt);
                xxDate = CvtToDate(xxDate8);
                scdate = AlphaUSA(xxdate);
             Else;
                scDate = *blanks;
             Endif;
             BuildJSON(outJSON:'OutDtlEPayEfDt':scDate);

             BuildJSON(outJSON:'OutDtlEpayVendorInboundFilesFolderPath':
                       EVMiFldFil);
             BuildJSON(outJSON:'OutDtlEpayVendorOutboundPaymentFilesFolderPath':
                       EVMoFldFil);
             BuildJSON(outJSON:'OutDtlEpayVendorOutboundAttachmentsFolderPath':
                        EVMoFldAtt);

             // Vendor specific fields
             BuildJSON(outJSON:'OutDtlEpayOrigin':EVMorigin);
             BuildJSON(outJSON:'OutDtlEPayTPACode':EVMtpaCode);
          Endif;

       RtnCode = ' ';
       Return;
      /end-free

