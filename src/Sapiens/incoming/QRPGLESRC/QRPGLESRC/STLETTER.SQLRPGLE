     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
     H*====================================================================
     H/TITLE STLETTER - Program for Scriptura related tasks.
     H*====================================================================
     H*  Description:
     H*
     H*  This program can be called to perform Scriptura related tasks.
     H*
     H*  Subroutine DARTS - Generate DARTS remark for document pased in.
     H*
     H*  Subroutine DATA - Generate work file data for document passed in.
     H*
     H*  Subroutine USER - Return user who submitted document request.
     H*
     H*  Subroutine CHKXML - Generate message/spool file if no XML file.
     H*
     H*  Subroutine PRTERROR - Print an error message.
     H*
     H*  Subroutine XMLREQUEST - Request an XML file name and write record.
     H*
     H*  Subroutine ARCHIVE - Archive the XML document.
     H*
     H*  Subroutine TEMPLATE - Return the template document ID.
     H*
     H*  Subroutine DIRECTORY - Return the directory for XML file.
     H*
     H*  Subroutine SCRIPTURA - Return if Scriptura output be generated.
     H*
     H*  Subroutine LANGUAGE - Return language code based on language passed in.
     H*
     H*  Subroutine WORKFILE - Return if a workfile should be built.
     H*
     H*  Subroutine TEMPLRQST - Request an additional template record.
     H*
     H*  Subroutine TIMEOUT - Request # of seconds for timeout to java/scriptura.
     H*
     H*  Subroutine SIGNATURE - Copy signature to temporary folder and return info.
     H*
     H*  Created:  02/17/15  C:65  Project: SY251
     H*====================================================================
     H*  Revisions:
     H*
     H* 05/07/25 C:291 Project: WC2494
     H*                Pass Description from XPTYPE to ISEWMD to show
     H*                on attached documents in iDarts.
     H*
     H* 01/30/25 C:65  Project: SY446 (Design and Compose Revamp)
     H*                Only use logo URL for D&C LiveDoc requests.
     H*
     H* 01/09/25 C:65  Support: 117717 (Prescient)
     H*                Change the merging of PDFs for insured and agency copy
     H*                to happen with direct calls to the java class using its
     H*                various methods.  The maximum number of documents merged
     H*                at one time will be controlled by field STSCP/SC1107 if
     H*                populated, otherwise 100.
     H*
     H* 01/06/25 C:65  Support: 118236 (CoveRisk)
     H*                Only perform the threshold checks and merging of PDFs
     H*                when emailing has been enabled for the job.
     H*
     H* 12/09/24 C:65  Support: 118144 (CoveRisk)
     H*                Escape quotation marks in email addresses for SNDEMAIL.
     H*
     H* 11/27/24 C:342 Support: 117926 (Prescient)
     H*                Change Commission Statements comment in iDarts
     H*
     H* 11/11/24 C:300 Support: 117835
     H*                Check Status of 'Commission Statements' template
     H*                'Agent Commission.scd' from STSP1L file
     H*
     H* 09/25/24 C:65  Project: SY446 (Design and Compose Revamp)
     H*                When design and compose is enabled pull the logo URL
     H*                instead of the logo path for interactive requests.
     H*
     H* 08/21/24 C:65  Support: 114429 (UBIC)
     H*                Change JAR location for PDF Merge to same directory
     H*                Scriptura Compsuite JAR is located.
     H*
     H* 05/20/24 C:333 Support: 114757 (FMIC)
     H*                Renewal COI Print Job Issue
     H*
     H* 04/23/24 C:062 Project: WC2670(115433)
     H*                -Don't include excluded items in agent and insured   when
     H*                 copies. This includes the report.
     H*                -Get premium notices included in report.             when
     H*                -List all documents not just the first
     H*
     H* 04/12/24 C:291 Project: PWC2670
     H*                Add condition to execute BuildAgentFtpEmailList only when
     H*                Agent Email addresses (XPagtMailA) exist.
     H*
     H* 02/23/24 C:65  Project: PHW#114068 (Sunz)
     H*                Change the timeout calculation to be based on the size of
     H*                the template XML files in addition to the number of them.
     H*                Some templates process a huge number of pages.
     H*
     H* 02/20/24 C:291 Project: WC2670
     H*                Change desc on the iDarts remark for the Bills
     H*
     H* 01/16/24 C:340 Project: 153WAD
     H*                Add processing to copy document to User Folder.
     H*
     H* 08/03/23 C:340 Project: WC2670
     H*                Add addional attachments.
     H*
     H* 09/11/23 C:344 Project: WC2907
     H*                Add From Email Address
     H*
     H* 08/15/23 C:65  Project: WC2908
     H*                Add more details to the error report - Flow file name,
     H*                Java or Scriptura error, etc.
     H*
     H* 08/04/23 C:65  Project: PHW113411 (Sunz)
     H*                Increase the fractional seconds in the timestamp portion
     H*                of the flow file name to help prevent duplicates when group
     H*                job queue functionality is allowing multiple jobs to run
     H*                concurrently.  Add user name to flow file name as well.
     H*
     H* 05/24/23 C:062 Project: WC2838
     H*                Copy documents that failed to ftp.
     H*
     H* 05/03/23 C:344 Project: WC2838
     H*                When a Claim LiveDoc is processed then use the
     H*                populated liveDoc ID instead of the Template ID.
     H*                Restore removal of REPLACETOKEN parm runInfo,
     H*                docCat, docSub and docLang. Created subroutine
     H*                CVTLANGABB to covert ISO language abbreviation to the
     H*                language fullname.
     H*
     H* 03/30/23 C:062 Project: WC2838
     H*                Add report
     H*
     H* 02/13/23 C:062 Project: WC2838
     H*                Add new path statement parameter.
     H*
     H* 02/06/23 C:344 Project: WC2875
     H*                Increase RRN Type field from 25 to 100 char.
     H*
     H* 01/16/23 C:62  Project: WC2838
     H*                Make additonal changes.
     H*
     H* 01/13/23 C:65  Project: PHW105625 (CoveRisk/Scriptura)
     H*                Add logging to email subroutine to record email details.
     H*
     H* 11/11/22 C:344 Project: PHW103313
     H*                Change desc on the iDarts remark for the certificates
     H*                of insurance.
     H*
     H* 09/20/22 C:62  Project: WC2838
     H*                Contact information changes.
     H*
     H* 06/07/22 C:33  Support: 100993
     H*                Keep Q#PNAM defined by category
     H*
     H* 05/20/22 C:333 Support: 72454
     H*                Mid Term Endorsement / Additional Named Insured
     H*                - DARTS Remark should read Mid-term Endorsement
     H*                  not the form number.
     H*
     H* 05/06/22 C:65  Project: WC2278  #101948
     H*                Swap any semi-colon marks in email lists to commas so
     H*                the emails can be properly separated out.
     H*
     H* 02/03/22 C:65  Project: WC2278 #97345
     H*                Change the original coding for sending emails to use
     H*                a newer command/api instead - SNDEMAIL.  As command
     H*                can accept no from email address, do not prevent email
     H*                when there is no user email address.
     H*
     H* 09/12/21 C:33  Project: WC2278 #98058
     H*               -Use unique key for retrieving doc info
     H*
     H* 08/31/21 C:33  Project: WC2278 #97253
     H*               -Use the same data queue as SNDEMAIL.
     H*               -Parse multiple email recipients.
     H*
     H* 04/22/21 C:65  Project: WC2278
     H*                If the LDA has %key% in position 251 then retrieve the
     H*                key value from position 261 and use it to retrieve the
     H*                LDA values from an xmlprep record.  This record will
     H*                store all of the LDA values including expanded email.
     H*                Additionally do not parse the email list into separate
     H*                emails - send all as one request/email.  If XPMAIL field
     H*                has %mass% then call api to determine/populate the email
     H*                addresses for the request.
     H*                Move the call to email subroutine so its within the loop
     H*                over xmlprep so emails work for mass processes.
     H*
     H* 12/15/20 C:65  Project: SY371
     H*                Allow iDarts records to be created without an attachment
     H*                in cases where there is a remark code but resolve output
     H*                to iDarts was not checked.
     H*
     H* 07/09/20 C:65  Project: WC2073
     H*                Add an Agency Services category for Commissions, since
     H*                the output is resolved to Agent Master iDarts.
     H*
     H* 03/13/20 C:65  Project: SY371/Phw#87921 (Amwins)
     H*                Prevent creation of XMLPREP or XMLTEMP records when
     H*                called during WCPOLS extract.  No output is needed.
     H*
     H* 01/24/20 C:65  Project: SY371
     H*                Check SCDSC8 in STSCP if an alternate compsuite output
     H*                folder is to be used.  Place output there if it exists.
     H*
     H* 07/12/19 C:65  Project: SY371
     H*                Populate temporary signature folder fields in xmlprep
     H*                always, as template level signatures require them for flow.
     H*
     H*  05/08/19 C:300 Support: 78170 (LUBA)
     H*                 Attach a document to an existing remark in iDarts ---
     H*                 By populating Seq# of 1st iDARTS document in XPDRSEQ# (XMLPREP)
     H*                 we can attach another document to an existing iDarts record by
     H*                 using a different API DA100R (ISEWMD)
     H*
     H* 11/01/18 C:267 Project: 150WAM PHW 50562
     H*                Additional revisions for darts remarks for
     H*                Division ADD
     H*                Delete WXDA1P workfile for division ADD after processing
     H*
     H* 06/04/18 C:267 Project: 150WAM PHW 50562
     H*                For endorsement of adding a division, print more
     H*                darts details
     H*
     H* 05/08/18 C:65  Project: SY395
     H*                Recognize interactive forms.
     H*
     H* 04/09/18 C:65  Project: SY371
     H*                Print more details from error message on report.
     H*
     H* 02/08/18 C:65  Project: 148WAT
     H*                Recompile for bundle field.
     H*
     H* 11/16/17 C:65  Project: 102WHK
     H*                Call processing program for department letters with
     H*                different schema type specified.
     H*
     H* 10/26/17 C:65  Project: SY371/150WBW
     H*                Clean up both the secondary process level signature
     H*                and the two form specific signature files.  Add
     H*                processing to populate process level 2nd signature.
     H*
     H* 10/24/17 C:65  Project: SY371/150WBW
     H*                Do not populate process level signatures into XMLTEMP -
     H*                duplicates effort on copying signatures.  Add a new
     H*                call to the program to allow a passed in signature to
     H*                be copied to a temporary file so signatures can be
     H*                updated into XMLPREP/TEMP records externally.  Note:
     H*                Update TEMPIFS and TEMPWIN fields externally too.
     H*
     H* 08/31/17 C:65  Project: SY371
     H*                Remove eff date from read prior equal statement on
     H*                replacement template file.
     H*
     H* 05/26/17 C:65  Project: SY371
     H*                If SC1103 is populated in Scriptura module file then
     H*                use the value for # of seconds to start the timeout.
     H*                Update default minimum timeout to be two minutes.
     H*
     H* 05/04/17 C:65  Project: SY371
     H*                Do not return flow file when the incoming xml file
     H*                is renamed to allow subsequent call via S4401DART
     H*                to continue replacing (and cleaning) xmlFile.
     H*                Reset file name to prevent it from having bad data
     H*                in it from S4401DART call as well.
     H*
     H* 04/14/17 C:65  Project: SY371
     H*                With the flow file name change, the Update XML File
     H*                Name subroutine needs to replace the .PDF file name
     H*                received with the flow XML file name.
     H*
     H* 03/14/17 C:65  Project: SY371
     H*                Add in a retry on checking when a document exists on
     H*                the IFS in case there is a delay on it being written.
     H*
     H* 03/02/17 C:65  Project: SY371
     H*                Copy the iDarts document to the user folder when
     H*                indicated.
     H*
     H* 03/01/17 C:65  Project: SY371
     H*                Update the user folder to have authority given to
     H*                the scriptura profiles.
     H*
     H* 02/16/17 C:65  Project: SY371
     H*                Prefix the temporary signature folder path with
     H*                data center path when needed.
     H*
     H* 01/03/17 C:65  Project: SY371
     H*                Update the printing code to print mass documents or
     H*                single run documents.
     H*
     H* 12/22/16 C:65  Project: SY371
     H*                Populate new print values into file.
     H*
     H* 12/06/16 C:65  Project: SY371
     H*                Check new setup files for logos and signatures.
     H*
     H* 11/29/16 C:65  Project: SY371
     H*                Rename flow xml to start with string of 'Flow_XML'.
     H*                Add processing for flexible timeout by counting # of templates.
     H*
     H* 11/16/16 C:65  Project: SY371
     H*                Update XPDATA to Yes for master records.
     H*
     H* 10/07/16 C:65  Project: SY371
     H*                Update positions on LDA to prevent overlap with other
     H*                processes which utilize it.
     H*
     H*  10/04/16 C:65  PROJECT: SY371
     H*                 Fix a looping problem on archiving.
     H*
     H*  09/21/16 C:65  PROJECT: SY371
     H*                 Fix a looping problem in Darts subroutine.
     H*                 Add an order field to allow templates to be sequenced.
     H*
     H*  09/15/16 C:65  PROJECT: SY371
     H*                 Populate QA and Test group flags.
     H*
     H*  09/09/16 C:65  PROJECT: SY371
     H*                 Archive the template xml file. Add new subroutine to loop
     H*                 over the xmlprep and xmltemp files to archive all xml files.
     H*                 Add effective date as a parameter to the program.  This field
     H*                 is used to find the template from the new template information/
     H*                 replacement template file.  If no date is passed in then the
     H*                 current system date is used instead.
     H*
     H*  08/26/16 C:65  PROJECT: SY371
     H*                 Add field to xml prep file to indicate additional templates.
     H*
     H*  08/11/16 C:65  PROJECT: SY371
     H*                 Access print/email/darts info from local data
     H*                 area populated by the output control window.
     H*
     H*  08/10/16 C:65  Project: SY371
     H*                 Add a check if the output document exists prior to writing
     H*                 the Darts remark.
     H*
     H*  08/04/16 C:65  Project: SY371
     H*                 Add template file for additional templates needed for
     H*                 a document - templates will also be stored in their own
     H*                 XML file.  These can be requested through TEMPLRQST subroutine.
     H*                 The template request subroutine accepts the output file name
     H*                 in the QQXML parameter and returns the template XML file name
     H*                 back.  The QQReturn parameter will hold the type of process.
     H*                 Remove directory field from XMLPREP - it was unused.
     H*                 Populate XPLINK on the master XMLPREP record so it can
     H*                 be identified as a mass process record and return XPFile in
     H*                 QQXML so individual records in a mass request can be accessed.
     H*
     H*  07/28/16 C:65  Project: SY371
     H*                 Add subroutine at beginning of Darts section to generate an
     H*                 email for the request if indicated - this will generate an
     H*                 email on single requests or on the master document of a mass
     H*                 request.
     H*
     H*  06/29/16 C:65  Project: SY371
     H*                 Populate temporary image folder fields into XML for
     H*                 signature and copy the signature there.
     H*
     H*  06/23/16 C:65  Project: SY371
     H*                 Populate user name/email/staff code fields in XMLPREP.
     H*
     H*  06/17/16 C:136 Project: SY371
     H*                 Don't call SNXTDOCID if a prefix is not passed in.
     H*                 The prefix is needed when generating a DARTS remark
     H*                 only.
     H*
     H*  06/16/16 C:65  Project: SY371
     H*                 Add WORKFILE mode for returning if a work file
     H*                 should be built for a phase 2 process.  A 'Y'
     H*                 value is passed back in QQXML parameter if either
     H*                 Scriptura or 3rd party XML is needed.
     H*
     H*  05/31/16 C:65  Project: SY371
     H*                 Add functionality for directing output to a user
     H*                 output folder.  If process is flagged to direct
     H*                 output to user folder then populate into the output
     H*                 folder field if no darts, otherwise place in new field.
     H*                 Also add functionality for linked records for mass
     H*                 processes.  The "master record" will contain the XML
     H*                 file name while the "sub records" will contain the
     H*                 policy/claim info and the XML file name will be stored
     H*                 in a new link field - to link them to master record.
     H*                 Move the RunDataPgm subroutine to be within the new
     H*                 loop in Darts subroutine so it runs for every request.
     H*
     H*  04/26/16 C:65  Project: SY251
     H*                 Add archiving to Darts call for interactives since
     H*                 STLETTER is directly submitted for them for DARTS.
     H*
      *  04/21/16 C:302 Project: 25WUK
      *                 Update to change the destination folder of the PDF
      *                 document to export folder indicated in the module file if
      *                 it is populated.
     H*
     H*  04/11/16 C:65  Project: SY371
     H*                 Build Darts comment for phase 2 processes to use the
     H*                 type of process field for the description.
     H*
     H*  04/08/16 C:65  Project: SY251
     H*                 Blank out call parameter for user if no record found.
     H*
     H*  04/06/16 C:65  Project: SY371
     H*                 Populate default information for phase 2 processes
     H*                 from the phase 2 setup file to the XML prep record.
     H*
     H*  04/04/16 C:65  Project: SY251/SY371
     H*                 Add new subroutine to convert the language passed in
     H*                 to a language code.
     H*
     H*  04/01/16 C:65  Project: SY251/SY371
     H*                 Add extra data program call to happen on Darts mode
     H*                 for additional functionality needed.
     H*
     H*  02/25/16 C:65  Project: SY251
     H*                 Update comments on the Darts records.  Replace first
     H*                 (not all changes documented)
     H*
     H*  02/09/16 C:65  Project: SY371
     H*                 Add client ID value to XML prep record.
     H*
     H*  01/06/16 C:65  Project: SY251
     H*                 Populate quote field in Darts record key and use
     H*                 interactive doc ID for comment when appropriate.
     H*
     H*  11/24/15 C:65  Project: SY251
     H*                 Remove unneeded folder path field for data center.
     H*
     H*  11/24/15 C:65  Project: SY371
     H*                 Check DATA and DARTS flags prior to calling/writing
     H*                 the workfile and DARTS programs.  Add subroutine
     H*                 ARCHIVE to separate out the archiving from Darts.
     H*                 Add subroutines TEMPLATE and DIRECTORY for returning
     H*                 those values and SCRIPTURA for generating output.
     H*
     H*  10/08/15 C:65  Project: SY371
     H*                 Add subroutine XMLREQUEST which will generate a new
     H*                 XML file name and write a record to XMLPREP to hold
     H*                 it.
     H*
     H*====================================================================
     FSTCTL1    if   e           k disk
     FSMCOL     if   e           k disk
     FSMUPL     IF   E           K DISK
     FSTSCP     UF   E             DISK    rename(psc01:psc0p)
     FXMLPREL1  IF A E           K DISK
     FXMLPREL2  IF   E           K DISK    rename(xp100:xp102)
     FXMLTEML1  IF A E           K DISK    rename(xt100:xt101)
     FXMLTEML   IF   E           K DISK    rename(xt100:xt100)
     FSTSP1L    IF   E           K DISK    rename(psp011:psp010)
     FSTSP2L    IF   E           K DISK    rename(psp012:psp012)
     FSTSP3L    IF   E           K DISK    rename(psp013:psp013)
     FXMLPREL3  UF   E           K DISK    rename(xp100:xp103)
     FXMLPREL4  IF   E           K DISK    rename(xp100:xp104)
     F                                     prefix(x4:2)
     FWDELP     IF   E             DISK    rename(pel01:pel0p) usropn           wcms
     FWMCMP     IF   E             DISK    rename(pcm01:pcm0p) usropn           wcms
     FFMCMP     IF   E             DISK    rename(pcm01:mcm0p) usropn           manc
     F                                     prefix(FM:2)
     FUMPCP     IF   E             DISK    rename(ppc01:mpc0p) usropn           manc
     FWMAGP     IF   E             DISK    rename(pag01:pag0p) usropn           wcms
     FWMEML     IF   E           K DISK                                         wcms
      *  Rate Code
     FWMRSL     IF   E           K DISK                                         wcms
     FWDELL     IF   E           K DISK    usropn                               wcms
     F                                     infds(dbfds1)
     FWMCML     IF   E           K DISK    rename(pcm01:pcm00) usropn           wcms
     F                                     infds(dbfds2)
     FWMAGL     IF   E           K DISK    rename(pag01:pag00) usropn           wcms
     F                                     infds(dbfds3)
     F*** Begin Add *** 05/24/23 *******************************************
     FWMAAL     IF   E           K DISK    rename(paa01:paa00)                  wcms
     F*** End   Add *** 05/24/23 *******************************************
     FFMCML1    IF   E           K DISK    rename(pcm01:mcm01) usropn           manc
     F                                     prefix(FM:2)
     F                                     infds(dbfds4)
     FUMPCL11   IF   E           K DISK    rename(ppc01:mpc11) usropn           manc
     F                                     infds(dbfds5)
      *  Division remark codes
     FWMDXL     IF   E           K DISK                                         wcms
      *  Endorsement file
     FWMENL3    IF   E           K DISK                                         wcms
      *  Workfile - New division add
     FWXDA1L1   IF   E           K DISK                                         wcms
     f                                     infds(da1fds)
     FWXDA1P    UF A E             DISK    rename(pda01:pda0p)
     FSTDLL     IF   E           K DISK    rename(pdl01:pdl00)
     FSTDLL2    IF   E           K DISK    rename(pdl01:pdl02)
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     FSTP1L     IF   E           K DISK    rename(pp101:pp100)
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)
     FSTTSL     IF   E           K DISK    rename(pts01:pts00)
     FSMKTL1    IF   E           K DISK
     f*** Begin Add *** 09/20/22 ***********************************
     FWMRPL     IF   E           K DISK    Rename(prp01:prp00)
     f*** End   Add *** 09/20/22 ***********************************
     f*** Begin Add *** 11/11/22 *** PHW103313 *********************
     FWXCHP1    IF   E             DISK    Rename(pch01:pch02) usropn
     f*** End   Add *** 11/11/22 *** PHW103313 *********************
     f*** Begin Add *** 01/13/23 ***********************************
     FSXEMP     O    E             DISK    Rename(pem01:sem0p)
     f*** End   Add *** 01/13/23 ***********************************
     F*
     f*** Beg Add *** 08/03/23 *** WC2670 **************************
     FXMLPREL5  IF   E           K DISK    Rename(XP100:XP105)
     F                                     PreFix(x5:2)
     f*** End Add *** 08/03/23 *** WC2670 **************************
     f*** Beg Add *** 11/27/24 *************************************
     Fwmlsl     if   e           k disk    Rename(pls01:pls00)
     f*** End Add *** 11/27/24 *************************************
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) usropn
     F*** Begin Add *** 03/30/23 *******************************************
     FQSYSPRT7  O    F  132        PRINTER OFLIND(*INOA) usropn
     F*** End   Add *** 03/30/23 *******************************************
     D*
     D TIT             S             60    DIM(2) CTDATA PERRCD(1)
     D PSDS          ESDS                  extname(SZQ1P)
     D*
     d da1fds          ds
     d  binda1rrn            397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D                 DS
     D  QCKEYc                 1     40
     D  QCCO#c                 1      3  0
     D  QCFNDc                 4      6  0
     D  QCFYRc                 7      9  0
     D  QCCASEc               10     16  0
     D                 DS
     D  QCKEYa                 1     40
     D  QCFein                 1      9  0
     D  QCMod                 10     12  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DSDT2                  1      8  0
     D  DSMM2                  1      2
     D  DSDD2                  3      4
     D  DSYY2                  5      8
     D*
     D Q#KTYP          s              2
     d*** Begin Add *** 09/20/22 *******************************************
     D crlf            c                   '<br/>'

     D agtInfoRRN      s              9  0

     d beg             s             11s 0
     d end             s             11s 0
     d len             s             11s 0
     d pos             s             11s 0
     d str             s             11s 0

     D chrAccInfo      s            500a   Inz
     D chrAgtInfoDocID...
     D                 s            100a   Inz
     D chrAgtInfoPath...
     D                 s            256a   Inz
     d chrCmdType      s             12a   Inz
     d chrDocList      s                   SqlType(clob:1000000)
     d chrDocList2     s          16000    Inz
     d chrDocListErr   s          16000    Inz
     d chrDocMail      s          10000a   Inz
     d chrDocMailAgt   s           1000a   Inz
     D chrEMName       s             27a   Inz
     D chrExtractedDocID...
     D                 s            256a   Inz
     d chrFeinMod      s             12a
     D chrFNData       s                   Like(SPfnData) Inz
     D chrFNPrefix     s                   Like(SPfnPrefix) Inz
     d chrFromDir      s            250a
     d chrFromDirFtp   s            250a
     d chrFromDocID    s            100a
     d chrHasMail      s              1a   Inz
     D chrInsInfoDocID...
     D                 s            100a   Inz
     D chrInsInfoPath...
     D                 s            256a   Inz
     d chrMail         s              1a
     d chrOutQ         s             10a
     d chrPassword     s             12a
     d chrPath         s            100a
     d chrReportPath   s           5000a
     d chrResults      s            122a
     d chrRtncode      s              1a
     d chrThreshold    s              1a
     d chrToDir        s            250a
     d chrToSystem     s            100a
     d chrUserID       s             12a
     d chrXmlString    s            200a
     D chrFlowXml      s             60a

     d decDocCount     s             11s 0
     d decDocIndex     s             10s 0
     d decDocSize      s             10s 0
     d documentFtpD    s               n
     d*** begin add ***  01/09/25  **************************************
     d numDocs         s             11s 0 Inz(0)
     d maxDocs         s             11s 0 Inz(100)

     D javaString      s               O   CLASS(*JAVA:'java.lang.String')

     D pdfMergerObj    s               O   Class(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility')

     D NewString       PR              O   EXTPROC(*JAVA:
     D                                     'java.lang.String':
     D                                     *CONSTRUCTOR)
     D                                     CLASS(*JAVA:'java.lang.String')
     D  charParm                   1000A   CONST Varying

     D PDFMCreateObj   PR              O   EXTPROC(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility':
     D                                     *CONSTRUCTOR)
     D                                     Class(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility')

     D PDFMAddSource   PR                  EXTPROC(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility':
     D                                     'addSource')
     D   fileName                      O   CLASS(*JAVA:'java.lang.String')

     D PDFMMergeDocs   PR                  EXTPROC(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility':
     D                                     'mergeDocuments')

     D PDFMSetDestin   PR                  EXTPROC(*JAVA:
     D                                     'org.apache.pdfbox.multipdf.PDF-
     D                                     MergerUtility':
     D                                     'setDestinationFileName')
     D   fileName                      O   CLASS(*JAVA:'java.lang.String')

     d*** end add *****  01/09/25  **************************************

     d emailType       s              1a   Inz

     d ftpCommand      s           1000a
     d ftpCommand2     s           1000a
     d ftpFailed       s               n

     d hldXmlLink      s                   Like(XPlink) Inz
     d insuredDocList  s               a   Len(10000000) varying
     d intFein         s                   Like(AGfein)
     d intLnkSize      s             10i 0
     d intMod          s                   Like(AGmod)
     d logType         s              1a   Inz
     d mergeType       s              1a   Inz

     d isFirst         s               n   Inz
     d isFirstDoc      s               n   Inz
     d isFirstDocErr   s               n   Inz
     d foundSlash      s               n   Inz
     d errorFound      s               n   Inz
     d*** begin add ***  08/15/23  *****************************************
     d errorType       s             10    Inz
     d*** end add *****  08/15/23  *****************************************
     d mergedDocExists...
     d                 s               n   Inz
     d printReport     s               n   Inz
     d*** Begin Add *** 02/13/23 *******************************************
     d Q#Env           s              1    Inz
     d Q#uncn          s            255    Inz
     d*** End   Add *** 02/13/23 *******************************************
     d*** Begin Add *** 04/23/24 *******************************************
     d chrDocTitle     s             15
     d*** End   Add *** 04/23/24 *******************************************

     d needsUnderscore...
     d                 s               n
     d*** Begin Add *** 01/16/23 *******************************************
     d isAgencyThreshold...
     d                 s               n   Inz(*Off)
     d*** End   Add *** 01/16/23 *******************************************

     d outAgentName    s             40    Inz
     d outWMAHPrrn     s              9  0 Inz
     d outWMAAPrrn     s              9  0 Inz
     d outWMAGPrrn     s              9  0 Inz

     d sqlAgtInfo      s           2500a   Inz

     d*** Beg Add *** 01/16/24 *** 153WAD **********************************
     d w@Length        s              3  0
     d*** End Add *** 01/16/24 *** 153WAD **********************************
     D strAgencyEmail...
     D                 s              1    inz(' ')
     D*** Begin Add *** 01/16/23 *******************************************
     D strAgencyEmailA...
     D                 s           1000    inz(' ') Varying
     D strEmailFtp     s           6000    inz(' ')
     D*** End   Add *** 01/16/23 *******************************************
     d strAgtInfo      s             60a   Inz
     D strInsuredEmail...
     D                 s              1    inz(' ')
     D*** Begin Add *** 01/16/23 *******************************************
     D strInsuredEmailA...
     D                 s           1000    Inz(' ') Varying
     D*** End   Add *** 01/16/23 *******************************************
     d strPath         s            350a
     d strToPath       s            350a

     D*** begin add ***  08/21/24  *****************************************
     DstrLibEnv        s             10    inz(' ')

     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10
     D*** end add *****  08/21/24  *****************************************

     D RtvLnkSize      pr                  extPgm('RTVLNKSIZ')
     d   inPath                     100a
     d   outSize                     10i 0
     d   outRtnCode                   1a

     d* Retrieve Secured Data
     d StRtvDta        pr                  extPgm('STRTVDTA')
     d  inCompany#                    3  0
     d  inType                        1
     d  inTypeKey                    50
     d  inNumber1                     1
     d  outData1                    250
     d  inNumber2                     1
     d  outData2                    250

     d  inCompany#     s              3  0
     d  inType         s              1    Inz('A')
     d  inTypeKey      s             50
     d  inNumber1      s              1    Inz('1')
     d  inData1        s            250
     d  inNumber2      s              1    Inz('2')
     d  inData2        s            250
     d  outData1       s            250
     d  outData2       s            250

     d*** End   Add *** 09/20/22 *******************************************
     d x               s              5  0
     d limit           s              5  0
     d sleep           pr            10i 0 extproc('sleep')
     d seconds                       10u 0 value
     d effDate         s              7  0 inz(0)
     d xxOrder         s              5  0
     d xxRRN           s              9  0
     D*** Begin Add *** 03/30/23 *******************************************
     d xxRRNA          S              9  0
     D*** End   Add *** 03/30/23 *******************************************
     D xmlFile         s                   like(XPXML)
     D xmlFileTemp     s                   like(XPXML)
     d outFile         s             60    inz(*blanks)
     D xxRMCD          s                   like(XPRMCD)
     D xxLang          s                   like(DLLang)
     d xxDesc          s            100
     d*** Begin Chg *** 09/20/22 *******************************************
     d*xmlString       s             25
     d xmlString       s             36
     d*** End   Chg *** 09/20/22 *******************************************
     d myStamp         s             17
     d myStampInt      s             17  0
     d*** begin add ***  08/04/23  *****************************************
     d myStampFull     s             20
     d myStampIntFull  s             20  0
     d*** end add *****  08/04/23  *****************************************
     d xmlCheck        s              1    inz(*off)
     d fldValue        s            100    inz('/SCR_DTM/')
     D xxPre           s              2
     D xxRtcd          s              1
     D xxSub#          s              5
     D xxPath          s             80
     D xxPflr          s             80
     D xxDcid          s             12
     D xxRoot          s             10
     D clientID        s              3    inz('000')
     d timeoutVal      s              5  0
     D*
     D xxXmlFile       s             61
     D xxXmlFold       s            101
     D xmlUNCN         s            256
     D message         s            120
     D message2        s            120
     D message3        s            120
     D error           s               n
     D success         s               n
     D*
     D keyCo#          s                   like(XPCO#)
     D keyFnd          s                   like(XPFND)
     D keyEmp#         s                   like(XPEMP#)
     D keyDiv          s                   like(XPDIV)
     D keyFyr          s                   like(XPFYR)
     D keyCase         s                   like(XPCASE)
     D keySub#         s                   like(XPSUB#)
     D keyCert         s                   like(XPCERT)
     D keyExt          s                   like(XPEXT)
     D keyFein         s                   like(XPFEIN)
     D keyMod          s                   like(XPMOD)
      *** Begin Add *** 11/11/22 *** PHW103313 **************************
     D @i              s              5i 0 inz
     D @qcLen          s              5i 0 inz
     D @nmLen          s              5i 0 inz
     D @tlLen          s              5i 0 inz
      *** End   Add *** 11/11/22 *** PHW103313 **************************
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxHold                         1    overlay(svlda:189)
     D xxSave                         1    overlay(svlda:190)
     D xxDart                         1    overlay(svlda:251)
     d xxDartRMCD                     5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxEmailTo                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     D*** Begin Chg *** 09/20/22 *******************************************
     d*xxBody                       350    overlay(svlda:673)
     d xxBody                       200    overlay(svlda:673)
     D*** End   Chg *** 09/20/22 *******************************************
     d*** Begin Chg *** 05/01/23 *******************************************
     d*xxLDATag*                     10    overlay(svlda:251)
     d*xxLDAKey*                     10    overlay(svlda:261)
     d xxLDATag                      10    overlay(svlda:269)
     d xxLDAKey                      10    overlay(svlda:279)
     d*** End   Chg *** 05/01/23 *******************************************
     D*** Begin Add *** 09/20/22 *******************************************
     d xxPrintI                       1    overlay(svlda:873)                   Insured Copy Print
     d xxPrinterI                    10    overlay(svlda:874)                   Insured Printer Key
     d xxTrayI                       50    overlay(svlda:884)                   Insured Printer Tray
     d xxPrintA                       1    overlay(svlda:934)                   Agent Copy Print
     d xxPrinterA                    10    overlay(svlda:935)                   Agent Printer Key
     d xxTrayA                       50    overlay(svlda:945)                   Agent Printer Tray
     D*** End   Add *** 09/20/22 *******************************************

     DChkDlo           pr                  extPgm('CHKIFSOBJ')
     D docName                      256    const
     D error                           n

     DS4401C2          pr                  extPgm('S4401C2')
     D xxXmlFile_                    61    const
     D xxXmlFold_                   101    const

     d SNextDocID      pr                  ExtPgm('SNXTDCID')
     d   $#Fold_                     80
     d   $#Dcid_                     12
     d   QQPre_                       2    const
     d   QQCo#_                       3    const
     d   QQFnd_                       3    const
     d   QQRtcd_                      1    const
     d   QQSub_                       5    const
     d   QQPFlr_                     80

     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   $#Root_                     10

     d SA030R          pr                  ExtPgm('SA030R')
     d   $#SUFX_                      3
     d   $#FLRP_                     80    options(*nopass)

     d PrtStmF         pr                  ExtPgm('PRTSTMFR')
     D   outStream                  256A   varying const
     D   outOutq                     10A   const
     D   outFormType                 10A   const
     D   outHold                      4A   const
     D   outSave                      4A   const
     D   outCopies                    3P 0 const
     D
     D strStream       s            256A   varying
     D strHold         s              4A
     D strSave         s              4A

     D strPgm          s             10

     d ExtraDataPgm    pr                  ExtPgm(strPgm)
     d   $#xmlf_                     60
     D langReturn      s            250
     D inLanguage      s             60
     D processType     s            100
     D templateName    s             60
     D templatePath    s            250
     D xxCat           s              5
     D xxTest          s              1
     D lo              c                   const('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D up              c                   const('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D userFolderBase  c                   const('/CompSuite Output/')
     D userFolder      s            250
     D altFolder       s            256
     d*** Begin Chg *** 09/20/22 *******************************************
     d*strCommand      s            255
     d strCommand      s           1000
     d*** End   Chg *** 09/20/22 *******************************************
     d suffix          s              3
     d massReq         s              1    inz(' ')
     d xmlLink         s             60    inz(' ')
     d xmlFile2        s             60    inz(' ')
     d xxProcess       s              1    inz(' ')

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     D xxSig           s            100
     D xxFolder        s            250
     D xxSig2          s            100
     D xxFolder2       s            250
     D svSig           s            100
     D svFolder        s            250
     D index           s              5  0
     D notAvail        s              1    inz(' ')
     D*** Begin Add *** 03/30/23 *******************************************
     D companyHead     s             60    Inz
     D chrType         s             50    Inz
     D chrJobName      s             10    Inz
     D chrJobUser      s             10    Inz
     D chrJob#         s              6    Inz
     D chrMassPath     s             60
     D chrInsuredPath  s             60
     D chrAllPrinter   s             10
     D chrInsPrinter   s             10
     d chrAgtPrinter   s             10

     d decGroup#       s                   Like(EMfnd)
     d decFein         s                   Like(EMfein)
     d decMod          s                   Like(EMmod)
     d decEmp#         s                   Like(EMemp#)
     d decDiv          s                   Like(EMdiv)
     d decYear         s                   Like(EMfyr)
     d decCase         s                   Like(EMcase)

     d chrName         s             30a
     d chrInsMail      s              3a
     d chrAgtMail      s             10a
     d chrAgtName      s             60a

     d chrDocPath      s            112a
     d chrInsEmail     s            112a
     d chrAgtEmail     s            112a
     d chrAgtFtp       s            112a
     d chrAgtPath      s            112a
     d chrFtpFailPath  s            256a

     d decInsDocE      s              5s 0
     d decInsDocC      s              5s 0
     d decAgtDocE      s              5s 0
     d decAgtDocC      s              5s 0
     d decAgtTotalFtp  s              5s 0
     d decDocTotals    s              5s 0

     d getPrepRcd      s               n   Inz

     d errorOnMkDir    s               n   Inz
     d isFtpInfo       s               n   Inz

     D*** End   Add *** 03/30/23 *******************************************
     D inRC            s              1    inz(' ')
     d index1          s              5  0
     D strMessage      s            200    inz(' ')
     D strList         s           5000    inz(' ')
     D strEmail        s           6000    inz(' ')
      *** Beg Chg *** 02/06/2023 *** WC2875 ***************************
     D***strRRNType      s             25    inz(' ')
     D strRRNType      s            100    inz
      *** End Chg *** 02/06/2023 *** WC2875 ***************************
     D*** Begin Add *** 03/30/23 *******************************************
     d strRRNTypeA     S            100    inz('Agency')
     D*** End   Add *** 03/30/23 *******************************************
     D strType         s            100    inz(' ')
     D strSubject      s           1000    inz(' ')
     D strBody         s          16000    inz(' ')
     D strLib          s             10    inz(' ')
     D strQueue        s             10    inz(' ')
     D strData         s          64512    inz(' ')
     D strTimeStamp    s             19    inz(' ')
     D strTime         s              6    inz(' ')
     D strDate         s              8    inz(' ')
     D strCmd          s           6000    inz(' ')
     D strDocName      s            100    inz(' ')
     D*** Begin Add *** 03/30/23 *******************************************
     D strSql          s           1000    inz(' ')

     ddsSXEMP        e ds                  extname(SXEMP) qualified inz
     ddsXMLPREP      e ds                  extname(XMLPREP) inz
     D*** End   Add *** 03/30/23 *******************************************
     D
     d QSndDtaq        pr                  extpgm('QSNDDTAQ')
     d   XXQNM                       10    const
     d   XXLIB                       10    const
     d   XXFLDL                       5  0 const
     d   XXDATA                   64512    const

     DCheckObjInLIBL   pr                  extPgm('SRCHKOBJ')
     D outObject                     10    const
     D outObjectType                  7    const
     D inError                        1

     DCreateDataQ      pr                  extPgm('SCCRTDTQ')
     D outDataQ                      10    const
     D outDataQLib                   10    const
     D outDataQLen                    5  0 const
     D outDataQText                  20    const
     D inReturn                       1

     D/copy sPrGenErr
      /Copy sPrRunCmd

     D/copy sPrDate
     D/copy sPrEnv
     D/copy wPrGroup
     D/copy sPrEmail
     D*** Begin Add *** 09/20/22 **************************************
     D/copy sPrSql
     D*** End   Add *** 09/20/22 **************************************
     D*** Begin Add *** 01/16/23 **************************************
     D/copy wPrAgent
     D*** End   Add *** 01/16/23 **************************************
     D/copy sPrString

     C*
     C* Key list for XML Prep File
     C*
     C     keyXML        klist
     C                   kfld                    xmlFile
     C*
     C     ELkey         KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#
     C                   KFLD                    keyDiv
     C                   KFLD                    keyFyr

     C     EMKy1         KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#
     C                   KFLD                    keyDiv

     C     EMKy2         KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#

     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD

     C     EMKy3         KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#
     C                   KFLD                    pdnwdv

     C     ENkey         KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#
     C                   KFLD                    keyDiv
     C                   KFLD                    keyFyr
     C                   KFLD                    q1jobn

     C     DAkey1        KLIST
     C                   KFLD                    keyCo#
     C                   KFLD                    keyFnd
     C                   KFLD                    keyEmp#
     C                   KFLD                    keyDiv
     C                   KFLD                    keyFyr
     C                   KFLD                    q1jobn
     C*
     C     key1XML       klist
     C                   kfld                    xmlFile2
     C                   kfld                    xmlLink
     C*
     C*   KEY LIST FOR THE REMARK TYPE TABLE FILE
     C*
     C     keySTCTL1     KLIST
     C                   KFLD                    xxRMCD
     C*
     C*   Document Setup File Key Lists
     C*
     C     keySTDLL      Klist
     C                   Kfld                    XPCo#
     C                   Kfld                    XPCat
     C                   Kfld                    XPSubC
     C                   Kfld                    XPDcid
     C                   Kfld                    xxLang
     C     keySTDLL2     Klist
     C                   Kfld                    XPCo#
     C                   Kfld                    XPCat
     C                   Kfld                    XPSubC
     C                   Kfld                    XPInid
     C                   Kfld                    xxLang
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD1       PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Entry list to program.
     C*
     C     *entry        plist
     C                   parm                    QQCall           10
     C                   parm                    QQXML            60
     C                   parm                    QQReturn        250
     C                   parm                    QQMode            1
     C                   parm                    QQRrn             9
     C                   parm                    QQPrefix          2
     C                   parm                    QQString         10
     C                   parm                    QQCo#             3
     C                   parm                    QQFnd             3
     C                   parm                    QQMass            1
     C                   parm                    QQLink           60
     C                   parm                    QQEfdt            7 0
     C*
     C                   If        Q1prms >= 12
     C                   Eval      effDate = QQEfdt
     C                   Endif
     C*
     C                   If        effDate = 0
     C                   Eval      effDate = Dateto7(SystemDate())
     C                   Endif
     C*
     C                   If        Q1prms >= 10
     C                   Eval      massReq = QQMass
     C                   Endif
     C*
     C                   If        Q1prms >= 11
     C                   Eval      xmlLink = QQLink
     C                   Endif
     C                   If        QQCall = 'XMLREQUEST' or
     C                             QQCall = 'TEMPLRQST'
     C*
     C                   Movel(P)  'W7417R'      XXPGM
     C*
     C                   Call      'SRCHKPGM'                           99
     C                   Parm                    XXPGM            10
     C                   Parm                    XXERR             1
     C     XXERR         Ifeq      '1'
     C                   Eval      *inLR = *on
     C                   Return
     C                   Endif
     C*
     C                   Endif
     C*
     C                   Call      'RTVCLTID'
     C                   Parm                    clientID
     C                   Clear                   message
     C                   Clear                   message2
     C                   Clear                   message3
     C*** begin add ***  08/15/23  *****************************************
     C                   Eval      errorType = 'Scriptura'
     C                   If        QQCall = 'JAVAERROR'
     C                   Eval      errorType = 'Java'
     C                   Eval      QQCall = 'PRTERROR'
     C                   Endif
     C*** end add *****  08/15/23  *****************************************
     C*
     C* Read the Scriptura module file.
     C*
     C                   Read(n)   psc0p
     C*
     C* Create email data queue if needed.
     C*
     C                   Exsr      EmailQueue
     C*
     C                   If        QQCall = 'LANGUAGE'
     C                   Eval      inLanguage = %trim(QQXML)
     C                   Exsr      LANGUAGE
     C                   Eval      QQReturn = %trim(langReturn)
     C                   Return
     C                   Endif
     C                   If        QQCall = 'WORKFILE'
     C                   Eval      processType = %trim(QQReturn)
     C                   Exsr      WORKFILE
     C                   Return
     C                   Endif
     C                   If        QQCall = 'SIGNATURE'
     C                   Eval      myStamp =%subst(%char(%timestamp:*iso0):1:17)
     C                   Eval      xxSig = %trim(QQXML)
     C                   Eval      xxFolder = %trim(QQReturn)
     C                   Exsr      MoveSig
     C                   Eval      QQXML = %trim(XPSig)
     C                   Eval      QQReturn = %trim(XPTempIFS)
     C                   Return
     C                   Endif
     C*
     C* Request a new XML file name and return.
     C*
     C                   If        QQCall = 'XMLREQUEST'
     C                   Move      QQRrn         xxRrn
     C                   Move      QQCo#         keyCo#
     C                   Move      QQFnd         keyFnd
     C                   Exsr      XMLREQUEST
     C                   Movel(p)  xmlFile       QQXML
     C                   Eval      *inLR = *on
     C                   Return
     C                   Endif
     C*
     C* Request an additional template record and return.
     C*
     C                   If        QQCall = 'TEMPLRQST'
     C                   Move      QQRrn         xxRrn
     C                   Eval      outFile = %trim(QQXML)
     C                   Exsr      TEMPLRQST
     C                   Movel(p)  xmlFile       QQXML
     C                   Eval      *inLR = *on
     C                   Return
     C                   Endif
     C*
     C                   Movel(p)  QQXML         xmlFile
     C*
     C                   Exsr      UpdXmlName
     C*
     C     keyXML        Chain     xp100
     C*
     C                   If        not %found
     C                   If        QQCall = 'USER'
     C                   Clear                   QQCall
     C                   Endif
     C                   Eval      *inLR = *on
     C                   Return
     C                   Endif
     C*** begin add ***  08/21/24  ***********************************
       GetObjLib('ATNAVP':'*FILE':strLibEnv);
       If strLibEnv = ' ';
          strLibEnv = 'pfdvp';
       Endif;
     C*** end add *****  08/21/24  ***********************************
     C*
     C                   Move      XPCo#         keyCo#
     C                   Move      XPSub#        keySub#
     C                   Move      XPFnd         keyFnd
     C                   Move      XPEmp#        keyEmp#
     C                   Move      XPDiv         keyDiv
     C                   Move      XPFyr         keyFyr
     C                   Move      XPCase        keyCase
     C                   Move      XPCert        keyCert
     C                   Move      XPExt         keyExt
     C                   Move      XPFein        keyFein
     C                   Move      XPMod         keyMod
     C*
     C                   Select
     C                   When      QQCall = 'DARTS'
     C                   Exsr      DARTS
     C                   If        XPIntr <> ' '
     C                   Exsr      Archive
     C                   Endif
     C                   When      QQCall = 'DATA'
     C                   Exsr      DATA
     C                   When      QQCall = 'USER'
     C                   Exsr      USER
     C                   When      QQCall = 'CHKXMLM' or QQCall = 'CHKXMLO'
     C                   Exsr      CHKXML
     C                   When      QQCall = 'PRTERROR'
     C                   Exsr      PRTERROR
     C                   When      QQCall = 'ARCHIVE'
     C                   Exsr      ARCHIVE2
     C                   When      QQCall = 'TEMPLATE'
     C                   Exsr      TEMPLATE
     C                   When      QQCall = 'DIRECTORY'
     C                   Exsr      DIRECTORY
     C                   When      QQCall = 'SCRIPTURA'
     C                   Exsr      SCRIPTURA
     C                   When      QQCall = 'TIMEOUT'
     C                   Exsr      TIMEOUT
     C                   Endsl
     C*
     C                   Eval      *inLR = *on
     C*
     C                   Return
     C**************************************************************************************
     C*
     C* ARCHIVE - Archive the XML document.
     C*
     C**************************************************************************************
     C     ARCHIVE       Begsr
      /free

       If XPSig <> ' ';
          Monitor;
             clear strCommand;
             strCommand = 'DEL OBJLNK(''' +  %trim(XPTempIFS) +
              %trim(XPSig) + ''')';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
       Endif;
       If XPSig2 <> ' ';
          xxSig = XPSig2;
          xxFolder = XPTempIFS;
          Exsr DeleteSig;
       Endif;
      /end-free
     C                   If        XPXML <> ' '
     C*
     C* Call S4401C2 to archive the XML document.
     C*
     C                   Eval      xxXmlFile = %SUBST(XPXml:1:60) + '$'
     C                   Eval      xxXmlFold = %SUBST(XPXmlF:1:100) + '$'
     C*
     C                   CALL      'S4401C2'
     C                   PARM                    xxXmlFile
     C                   PARM                    xxXmlFold
     C                   Endif
     C*
     C                   If        XPXMLT <> ' '
     C                   Eval      xxXmlFile = %SUBST(XPXmlT:1:60) + '$'
     C                   Eval      xxXmlFold = %SUBST(XPXmlF:1:100) + '$'
     C*
     C                   CALL      'S4401C2'
     C                   PARM                    xxXmlFile
     C                   PARM                    xxXmlFold
     C*
     C                   Endif
     C*
     C                   Endsr                                                  ARCHIVE Subr
     C**************************************************************************************
     C*
     C* ARCHIVE2 - Loop over the XMLPREP file to archive all xml files.
     C*
     C**************************************************************************************
     C     ARCHIVE2      Begsr
     C*
      /free

       Exsr Archive;

       If XPAddT = 'Y';
          Setll XPFile xt100;
          Reade(n) XPFile xt100;
          Dow not %eof;
             If XTFrmSig <> ' ';
                xxSig = XTFrmSig;
                xxFolder = XTTempIFS;
                Exsr DeleteSig;
             Endif;
             If XTFrmSig2 <> ' ';
                xxSig = XTFrmSig2;
                xxFolder = XTTempIFS;
                Exsr DeleteSig;
             Endif;
             xxXmlFile = %SUBST(XTXmlT:1:60) + '$';
             xxXmlFold = %SUBST(XTXmlF:1:100) + '$';
             S4401C2(xxXmlFile:xxXmlFold);
             Reade(n) XPFile xt100;
          Enddo;
       Endif;

       If XPLink <> ' ';
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade(n) key1XML xp100;

          Dow not %eof;
             Exsr Archive;

             If XPAddT = 'Y';
                Setll XPFile xt100;
                Reade(n) XPFile xt100;
                Dow not %eof;
                   If XTFrmSig <> ' ';
                      xxSig = XTFrmSig;
                      xxFolder = XTTempIFS;
                      Exsr DeleteSig;
                   Endif;
                   If XTFrmSig2 <> ' ';
                      xxSig = XTFrmSig2;
                      xxFolder = XTTempIFS;
                      Exsr DeleteSig;
                   Endif;
                   xxXmlFile = %SUBST(XTXmlT:1:60) + '$';
                   xxXmlFold = %SUBST(XTXmlF:1:100) + '$';
                   S4401C2(xxXmlFile:xxXmlFold);
                   Reade(n) XPFile xt100;
                Enddo;
             Endif;

             Reade(n) key1XML xp100;
          Enddo;

       Endif;

      /end-free
     C*
     C                   Endsr                                                  ARCHIVE2 Subr
     C**************************************************************************************
     C*
     C* DELETESIG - Remove the temporary signature object.
     C*
     C**************************************************************************************
     C     DELETESIG     Begsr
      /free
          Monitor;
             clear strCommand;
             strCommand = 'DEL OBJLNK(''' +  %trim(xxFolder) +
              %trim(xxSig) + ''')';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
      /end-free
     C                   Endsr                                                  DELETESIG Subr
     C**************************************************************************************
     C*
     C* SCRIPTURA - Return flag to generate Scriptura output.
     C*
     C**************************************************************************************
     C     SCRIPTURA     Begsr
     C*
     C                   Clear                   QQReturn
     C                   Eval      QQReturn = XPScrp
     C*
     C                   Endsr                                                  SCRIPTURA Subr
     C**************************************************************************************
     C*
     C* TEMPLATE - Return template document ID.
     C*
     C**************************************************************************************
     C     TEMPLATE      Begsr
     C*
     C                   Clear                   QQReturn
     C                   Eval      QQReturn = XPDcid
     C*
     C                   Endsr                                                  TEMPLATE Subr
     C**************************************************************************************
     C*
     C* DIRECTORY - Return directory for XML document.
     C*
     C**************************************************************************************
     C     DIRECTORY     Begsr
     C*
     C                   Clear                   QQReturn
     C                   Eval      QQReturn = XPXmlF
     C*
     C                   Endsr                                                  DIRECTORY Subr
     C**************************************************************************************
     C*
     C* DARTS remark creation for document.
     C*
     C**************************************************************************************
     C     DARTS         Begsr
       //*** Begin Add *** 09/20/22 ****************************************
     C                   TIME                    DSDTTM
       // Check Threshold
       ExSr Threshold;
       //*** End   Add *** 09/20/22 ****************************************
     C*
     C* Print the mass output document if it exists.
     C*
     C                   If        XPLink <> ' ' and XPOutQ <> ' '
     C*
     C                   Eval      xmlUNCN = %trim(XPDir) + %trim(XPFile)
     C*
     C                   Z-add     11            limit
     C                   Exsr      CheckDoc
     C*
     C                   If        success
     C                   Exsr      Print
     C                   Endif                                                  If success
     C*
     C                   Endif                                                  If XPLink <> ' '
     C*
      /free
       xxProcess = ' ';

       If XPLink = ' ';
          xxProcess = 'Y';
       Else;
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade(n) key1XML xp100;
          If not %eof;
             xxProcess = 'Y';
          Endif;
       Endif;

       Dow xxProcess = 'Y';

          If XPLink <> ' ';
             keyCo#  = XPCo#;
             keySub# = XPSub#;
             keyFnd  = XPFnd;
             keyEmp# = XPEmp#;
             keyDiv  = XPDiv;
             keyFyr  = XPFyr;
             keyCase = XPCase;
             keyCert = XPCert;
             keyExt  = XPExt;
             keyFein = XPFein;
             keyMod  = XPMod;
          Endif;

      /end-free
     C                   Eval      xmlUNCN = %trim(XPDir) + %trim(XPFile)
     C*
     C                   Z-add     11            limit
     C                   Exsr      CheckDoc
     C*
     C* Write the Darts remark if the output document exists.
     C*
     C                   If        success
     C                             or XPRMCD <> ' '
     C                   If        (XPDart = 'Y' and success) or
     C                             (XPRMCD <> ' ' and XPDart <> 'Y')
     C                   TIME                    DSDTTM
     C     XPUser        Chain     PUP01                              60
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANK        QCKEY
     C*
     C                   Select
     C*
     C                   When      XPCat = 'PS'
     C                   MOVE      keyCO#        QCCO#
     C                   MOVE      keyFND        QCFND
     C                   MOVE      keyEMP#       QCEMP#
     C                   MOVE      keyDIV        QCDIV
     C                   Move      XPQte         QCQte
     C*
     C                   When      XPCat = 'CS'
     C                   MOVE      keyCO#        QCCO#c
     C                   MOVE      keyFND        QCFNDc
     C                   MOVE      keyFYR        QCFYRc
     C                   MOVE      keyCASE       QCCASEc
     C                   MOVE      qcKeyc        qcKey
     C*
     C                   When      XPCat = 'AS'
     C                   MOVE      keyFein       QCFein
     C                   MOVE      keyMod        QCMod
     C                   MOVE      qcKeya        qcKey
     C*
     C                   When      XPCat = 'MC' or XPCat = 'PC'
     C                   MOVE      keyCO#        #1CO#
     C                   MOVE      keySUB#       #1SUB#
     C                   MOVE      keyFND        #1FND
     C                   MOVE      keyFYR        #1FYR
     C                   MOVE      keyCASE       #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   Move      ##Key         qcKey
     C                   Endsl
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C*
     C                   MOVE      *BLANKS       Q#COMM
     C
      /free
          Select;
          When SCCode01 = XPLang;
             xxLang = SCLang01;
          When SCCode02 = XPLang;
             xxLang = SCLang02;
          When SCCode03 = XPLang;
             xxLang = SCLang03;
          When SCCode04 = XPLang;
             xxLang = SCLang04;
          When SCCode05 = XPLang;
             xxLang = SCLang05;
          When SCCode06 = XPLang;
             xxLang = SCLang06;
          When SCCode07 = XPLang;
             xxLang = SCLang07;
          When SCCode08 = XPLang;
             xxLang = SCLang08;
          When SCCode09 = XPLang;
             xxLang = SCLang09;
          When SCCode10 = XPLang;
             xxLang = SCLang10;
          Endsl;

          //* Default language to english if none are selected.
          If xxLang = ' ';
             xxLang = 'English';
          Endif;

          If XPDcid <> ' ';
             Chain(n) keySTDLL pdl00;
          Else;
             Chain(n) keySTDLL2 pdl02;
          Endif;

          xxDesc = %trim(DLDesc);

          //* Default description if none available.
          If xxDesc = ' ';
             If XPType <> ' ';
                xxTest = %xlate(lo:up:%subst(XPType:1:1));
                If xxTest = 'A' or xxTest = 'E' or xxTest = 'I'
                 or xxTest = 'O' or xxTest = 'U';
                   xxDesc = 'An ' + %trim(XPType);
                Else;
                   //***  xxDesc = 'A ' + %trim(XPType);
                   if %trim(XPType) ='WC890600';
                      xxDesc = 'A Mid-Term Endorsement';
                   else;
                      xxDesc = 'A ' + %trim(XPType);
                   endif;
                Endif;
             Else;
                xxDesc = %trim(TIT(1));
             Endif;
          Endif;

      /end-free
     C*
     C                   Select
     C
     C                   When      XPRType = 'C'
     C                   Movel(p)  'Claimant:'   Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   When      XPRType = 'S'
     C                   Movel(p)  'Subscriber:' Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   When      XPRType = 'P'
     C                   Movel(p)  'Provider:'   Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   When      XPRType = 'G'
     C                   Movel(p)  'Group:'      Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   When      XPRType = 'E'
     C                   Movel(p)  'Employer:'   Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   When      XPRType = 'O'
     C                   Movel(p)  'Other:'      Q#Comm
     C     Q#COMM        CAT       xxDesc:1      Q#COMM
     C
     C                   Other
     C                   MOVEL(P)  xxDesc        Q#COMM          256
     C                   Endsl
     C*
     C     Q#COMM        CAT       TIT(2):1      Q#COMM
     C*
     C     Q#COMM        CAT       UPUSNM:1      Q#COMM
     C     Q#COMM        CAT       'on':1        Q#COMM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         DSDT2
     C*
     C     Q#COMM        CAT       DSMM2:1       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSDD2:0       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSYY2:0       Q#COMM
     C                   If        XPCase = 0
     C     Q#COMM        CAT       'for':1       Q#COMM
     C     Q#COMM        CAT       'policy':1    Q#COMM
     C     Q#COMM        CAT       'year':1      Q#COMM
     C*
     C     keyFyr        IFGT      99
     C                   MOVE      keyFyr        XBFYR             3
     C     Q#COMM        CAT       XBFYR:1       Q#COMM
     C                   ELSE                                                   FYR > 99
     C                   MOVE      keyFyr        XAFYR             2
     C     Q#COMM        CAT       XAFYR:1       Q#COMM
     C                   ENDIF                                                  FYR > 99
     C                   Endif
     C*
     C     Q#COMM        CAT       '.':0         Q#COMM
     C*
     C* Remark Code Description for the Addition of Division Endorsement
     C*  - include NEW division#, name, rating state in the remark code
      /free

       //Check remarks code for Division Add
       chain (keyCo#:keyFnd) pdx01;                      //one rcd per CO#/FND
       If %found(WMDXL);
          If XPRmcd = DXDVAD;                            //typically 'PSDA'
             //lookup workfile to get correct division# being added
             Chain DAKey1 pda01;
             If %found(WXDA1L1);
                If PDCO# = keyCo# and PDFND = keyFnd and
                   PDEMP# = keyEmp# and PDDIV = keyDiv and
                   PDFYR = keyFyr;
                   //lookup employer name for this NEW division being added
                   chain EMKy3 PEM01;
                   If %found (WMEML);
                      Q#COMM = %trim(Q#COMM) + '  DBA: ' + %trim(EMName) +
                          ', DIV: ' + %editc(EMDIV:'X');
                      //** Get Rating State
                      keyDiv = EMDIV;
                      Open  WDELL;
                      Chain ELKEY pel01;
                      If %found (WDELL);
                         Chain RSky PRS01;
                         If %found (WMRSL);
                            Q#COMM = %trim(Q#COMM) + ', ' + RSJRST + '.';
                         EndIf;
                      EndIf;
                      Close WDELL;
                   EndIf;
                  //*** Delete workfile WXDA1P
                  chain binda1rrn pda0p;
                  If %found(WXDA1P);
                     delete pda0p;
                  EndIf;
                EndIf;
             EndIf;
          EndIf;
       EndIf;
      /end-free

       //*** Beg Add *** 11/27/24 **********************************
       If XPtype = 'Commission Statements';
          Q#COMM = 'Agent Commission Statement - Group';
          If XPfnd <> 0;
             Q#COMM = %trim(Q#COMM) + ' ' +  %editc(XPfnd:'X') +
                      ' Statement Date';
          Endif;
          // Get Agency Number from Agent Assignment File (wmaal)
          Chain (XPco#:XPfein:XPmod:XPfnd) paa00;
          If %found(wmaal);
             If AAagt# <> 0;
                chain (XPco#:XPfnd:AAagt#) pls00;
                If %found(wmlsl);
                   Monitor;
                    Q#COMM= %trim(Q#COMM) + ' ' + %editc(dtMDYY(LSfrdt):'Y') +
                            ' thru ' + %editc(dtMDYY(LStodt):'Y');
                   On-Error;
                   Endmon;
                Endif;
             Endif;
          Endif;
       Endif;
       //*** End Add *** 11/27/24 **********************************
       // Begin Add *** 11/11/22 *** PHW103313 *********************
       If XPType = 'Certificate of Insurance';

          // If there are ADDITIONAL TEMPLATES
          If XPAddT = 'Y';
             Q#COMM = 'Certificates of Insurance for Policy Year';
          Else;
             Q#COMM = 'Certificate of Insurance for Policy Year';
          EndIf;

          If keyFyr > 99;
             Q#COMM = %trim(Q#COMM) + ' ' + XBFYR;
          Else;
             Q#COMM = %trim(Q#COMM) + ' ' + XAFYR;
          EndIf;

          // Traverse the Cert Holders CX/RE Letter Scriptura Workfile
          // until the XML Filename is the same as the Template XML
          // Filename.
          Open WXCHP1;
          Dou %eof(WXCHP1);
             Read pch02;
             If %eof(WXCHP1);
                Leave;
             Endif;
             If CHXml = XPXmlt;
                Q#COMM = %trim(Q#COMM) + ': ' + %editc(CHCer#:'X') +
                '(' + %trim(CHCnam) + ')';
                Leave;
             Endif;
          Enddo;
          Close WXCHP1;

          // Read XML Template Prep until
          If XPAddT = 'Y';
             Setll XPFile xt100;
             Reade XPFile xt100;
             Dow not %eof(XMLTEML);

                // Traverse Cert Holders CX/RE Letter Scriptura
                // Work File until XML Filename is the same as the
                // XML from the XML Template Prep file.
                xxRRN = 1;
                Open WXCHP1;
                Dou %eof(WXCHP1);
                   Read pch02;
                   If %eof(WXCHP1);
                      Leave;
                   Endif;

                   If CHXml = XTXmlt;

                      clear @qcLen;
                      clear @nmLen;
                      clear @tlLen;

                      // Check Q#COMM field for occupied bytes.
                      For @i = %size(Q#COMM) downto 1;
                         If %subst(Q#COMM:@i:1) <> ' ';
                            @qcLen = @i;
                            Leave;
                         Endif;
                      Endfor;

                      // Check Cert Name field for occupied bytes.
                      For @i = %size(CHCnam) downto 1;
                         If %subst(CHCnam:@i:1) <> ' ';
                            @nmLen = @i;
                            Leave;
                         Endif;
                      Endfor;

                      // Add bytes occupied by Q#COMM, Cert#, Cert Name,
                      // and 4 bytes ( space, comma and 2 parenthesis).
                      @tlLen = @qcLen + (2 * %size(CHCer#) - 1) +
                               @nmLen + 4;
                      If @tlLen < %size(Q#COMM);
                         Q#COMM = %trim(Q#COMM) + ', ' +
                         %editc(CHCer#:'X') + '(' + %trim(CHCnam) + ')';
                      Else;
                         Leave;
                      Endif;
                   Endif;

                Enddo; // Dou WXCHP1
                Close WXCHP1;

                Reade XPFile xt100;
             Enddo;
          Endif;
       EndIf;
       // End   Add *** 11/11/22 *** PHW103313 *********************
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  *blanks       Q#DCID           12
     C                   MOVEL(P)  *blanks       Q#FOLD           80
     C*
     C                   Clear                   Q#PNAM           10
     C                   Select
     C                   When      XPCat = 'CS'
     C                   MOVEL(P)  'W0070R'      Q#PNAM
     C                   When      XPCat = 'PS'
     C                   MOVEL(P)  'W0050R'      Q#PNAM
     C                   When      XPCat = 'MC'
     C                   MOVEL(P)  'W0070R'      Q#PNAM
     C                   When      XPCat = 'PC'
     C                   MOVEL(P)  'U0060R'      Q#PNAM
     C                   When      XPCat = 'AS'
     C                   MOVEL(P)  'W0040R'      Q#PNAM
     C                   Endsl
     C*
     C                   MOVE      *ZERO         Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  XPRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     SRDTO         Q#TRDT            7
     C                   MOVEL     SRDTO         Q#IDT             7
     C                   MOVE      DSTIME        Q#TRTM            6
     C                   MOVE      *BLANK        Q#STAT            1
     C                   MOVEL(P)  Q1USER        Q#IUSR           10
     C                   MOVE      *BLANKS       Q#DCDT            7
     C                   MOVE      *BLANK        Q#FL03            1
     C                   MOVE      *BLANK        Q#FL04            1
     C                   MOVE      *BLANK        Q#FL05            1
     C                   MOVE      *BLANK        Q#VCFN            1
     C                   MOVE      *BLANKS       Q#DPGM           10
     C                   MOVE      *BLANKS       Q#DKEY           40
     C*
     C                   MOVEL(P)  XPRMCD        xxRMCD
     C     keySTCTL1     CHAIN     PCT01                              60
     C                   If        *IN60 = *Off and CTDEPG <> *Blanks
     C                   MOVEL     CTDEPG        Q#DPGM
     C                   MOVEL     Q#KEY         Q#DKEY
     C                   Endif
     C*
     C                   Move      *blanks       Q#Path           80
     C                   Eval      Q#Path = %trim(XPDir) + %trim(XPFile)
     C*** Begin Add *** 02/13/23 *******************************************
     C                   Eval      Q#uncn = %trim(XPDir) + %trim(XPFile)
     C*** End   Add *** 02/13/23 *******************************************
     C                   If        XPDart <> 'Y'
     C                   Clear                   Q#Path
     C*** Begin Add *** 02/13/23 *******************************************
     C                   Clear                   Q#uncn
     C*** End   Add *** 02/13/23 *******************************************
     C                   Endif
     C                   Move      *blank        Q#RPYN            1
      //*** Beg Add ***** 02/20/24 **********************************************
        If XPIDesc <> *Blank;
           Q#COMM = XPIDesc;
        Endif;
      //*** End Add ***** 02/20/24 **********************************************
     C*
      * Attach document to same iDARTS remark --
      * By populating Seq# of 1st iDARTS document in XPDRSEQ# (XMLPREP)
      * we can attach another document to an existing iDarts record by using
      * a different API DA701R and DA100R (ISEWMD)
     C                   If        XPDRSEQ# <> 0
     C                             and XPDart = 'Y'
     C                   MOVE      XPDRSEQ#      Q#SEQ#
     c
      * Call program to write additional darts. (in SMARP file)
     c                   Call      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#COMM
     C                   PARM                    Q#PNAM
     C                   PARM      '000'         Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM      '00001'       Q#ASQE            5
      *
      * Call program to additional attach darts document. (in SMMDP file)
      *
     C                   MOVEL     XPFILE        Q#OCID
     C                   MOVEL     XPDIR         Q#FOLD
     C*** Begin Del *** 02/13/23 *******************************************
     C**********         MOVEL     Q#PATH        Q#UNCN
     C*** End   Del *** 02/13/23 *******************************************
      *
     c                   Call      'ISEWMD'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#OCID           40
     C                   PARM                    Q#FOLD
     C                   PARM      'O'           Q#OTYP            1
      *** Beg Chg ***** 05/07/25 *********************************
     C***                PARM      *blanks       Q#ODSC           40
     C                   PARM      XPTYPE        Q#ODSC           40
      *** End Chg ***** 05/07/25 *********************************
     C                   PARM                    Q#RMCD
     C                   PARM      'AS400   '    Q#SYSN           20
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#USER
     C                   PARM                    QQRTCD            1
     C                   PARM      *blanks       QQEDSC           80
     C                   PARM      *blanks       Q#APTH           80
     C                   PARM                    Q#RPYN
     C                   PARM                    Q#UNCN          255
     c*
     c                   else
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   PARM                    Q#TRDT
     C                   PARM                    Q#TRTM
     C                   PARM                    Q#STAT
     C                   PARM                    Q#IUSR
     C                   PARM                    Q#DCDT
     C                   PARM                    Q#FL03
     C                   PARM                    Q#FL04
     C                   PARM                    Q#FL05
     C                   PARM                    Q#IDT
     C                   PARM                    Q#VCFN
     C                   PARM                    Q#DPGM
     C                   PARM                    Q#DKEY
     C                   PARM                    Q#PATH
     C                   PARM                    Q#RPYN
     C*** Begin Add *** 02/13/23 **************************************
     C                   PARM      ' '           Q#Env             1            42  Pass as blank
     C*** Begin Add *** 02/13/23 **************************************
     C                   PARM                    Q#uncn          255            43
     C*** End   Add *** 02/13/23 **************************************
     C*
     c                   Endif
     C                   Endif                                                  If XPDart = 'Y'
     C* Email out document(s) from request.
     C*** Begin Add *** 03/30/23 ****************************************
     C                   If        success
     C*** End   Add *** 03/30/23 ****************************************
     C                   Exsr      Email
     C*** Begin Add *** 03/30/23 ****************************************
     C                   EndIf
     C*** End   Add *** 03/30/23 ****************************************
     C* Print documents for individual runs.
     C                   If        XPOutQ <> ' ' and XPLink = ' '
     C                             and success
     C                   Exsr      Print
     C                   Endif                                                  If XPOutQ <> ' '
     C*
     C* Copy the iDarts document to user folder if requested.
     C*
       //*** Beg Chg *** 01/16/24 *** 153WAD ******************
     C**                 If        XPOutFld <> ' ' and XPOutFld <> XPDir
     C**                 Exsr      CopyDoc
               If (XPOutFld <> ' ' and XPOutFld <> XPDir) Or
                  (XPMail = ' ' and XPMailAdd = ' ' and
                   XPCOPYE = 'Y' and xxEmail = 'Y' );
                  If XPOutFld = ' ';
                     Sa030r(suffix);
                     If suffix <> ' ';
                        userFolder = '/PDS/' + suffix + %trim(userFolderBase) +
                         %trim(Q1User) + '/';
                     Else;
                        userFolder = %trim(userFolderBase) +
                                     %trim(Q1User) + '/';
                     Endif;
                     XPOutFld = %trim(userFolder);
                  EndIf;
                  Exsr CopyDoc;
       //*** End Chg *** 01/16/24 *** 153WAD ******************
     C                   Endif
     C                   Endif                                                  If success
     C*
     C* Run additional data program.
     C*
     C                   Exsr      RunDataPgm
      /free

          If XPLink = ' ';
             xxProcess = ' ';
          Else;
             xxProcess = ' ';
             Reade(n) key1XML xp100;
             If not %eof;
                xxProcess = 'Y';
             Endif;
          Endif;

       Enddo;

       //*** Begin Add *** 03/30/23 ****************************************
       // Generate Report
       If printReport;
          ExSr GenerateReport;
       EndIf;
       //*** End   Add *** 03/30/23 ****************************************

      /end-free
     C*
     C                   Endsr                                                  DARTS Subr
     C**************************************************************************************
     C*
     C* DATA work file creation for document request.
     C*
     C**************************************************************************************
     C     DATA          Begsr
     C*
     C                   If        XPData = 'Y'
     C                   Select
     C                   When      XPCat = 'PS' or XPCat = 'CS'
     C                   Call      'WXMLDATA'
     C                   Parm                    xmlFile
     C                   When      XPCat = 'MC' or XPCat = 'PC'
     C                   Call      'FXMLDATA'
     C                   Parm                    xmlFile
     C                   Endsl
     C                   Endif                                                  If XPData = 'Y'
      /free
         //* Run data program for different phase 1 schema request.

         If XPSubC <> ' ' and XPType <> ' ';
            Chain(n) (XPCat:XPType) pts00;
            If %found and TSPPgm <> ' ';
                Monitor;
                   strPgm = TSPPgm;
                   ExtraDataPgm(XPXML);
                On-error;
                Endmon;
            Endif;
         Endif;

      /end-free
     C*
     C                   Endsr                                                  DATA Subr
       //*** Begin Add *** 09/20/22 ****************************************
       //===================================================================
       // CopyDocToFtpFailDirectory - copy document to fail directory
       //===================================================================
       BegSR CopyDocToFtpFailDirectory;

          // Access Scriptura Settings File
          SetLL 1 psc0p;
          Read(n) psc0p;

          // Access Agency Assignment
          Chain (AGco#:AGfein:AGMod:XPfnd) paa00;

          // Create Job folder
          chrCmdType   = ' ';
          If SCurl1 <> ' ';
             chrFtpFailPath = %Trim(SCurl1) + %Trim(Q1job) + '_' +
                %EditC(Q1jobn:'X') + '_' + %EditC(XPfnd:'X') + '_' +
                %EditC(AAagt#:'X');

             errorOnMkdir = *Off;
             strCmd  = 'MKDIR DIR(''' + %Trim(chrFtpFailPath) + ''')';
             Monitor;
                QCMDEXC(strCmd:%len(%Trim(strCmd)));
             On-Error;
                If Q1extP <> 'CPF' Or Q1exNm <> 'A0A0';
                   chrCmdType   = 'MKDIR';
                   errorOnMkdir = *On;
                Else;
                EndIf;
             EndMon;

             // Copy document to ftp failure directory
             If Not errorOnMkDir;
                strCmd = 'CPY OBJ(''' + %Trim(chrExtractedDocID) +
                         ''') TODIR(''' + %Trim(chrFtpFailPath) + ''')';
                Monitor;
                   QCMDEXC(strCmd:%len(%Trim(strCmd)));
                On-Error;
                   chrCmdType = 'CPY';
                EndMon;

                // Build Body of email
                If isFirstDocErr;
                   isFirstDocErr = *Off;
                   chrDocListErr = %Trim(chrFtpFailPath) + '/' +
                                 %Trim(chrFromDocID);
                Else;
                   chrDocListErr = %Trim(chrDocListErr) + crlf +
                                   %Trim(chrFtpFailPath) + '/' +
                                   %Trim(chrFromDocID);
                EndIf;

                // Mark documents that failed to ftp
                chrDocListErr = %Trim(chrDocListErr) + ' - ' +
                                %Trim(chrDocMailAgt);
             EndIf;
          Else;
             chrCmdType = 'No Directory';
          EndIf;

       EndSR;

       //===================================================================
       // Send Email when can't ftp document
       //===================================================================
       BegSR EmailFtpStatus;

          // Initialize
          strEmailFtp = SCurl5;

          // Did ftp fail?
          chrDocList2   = %scanrpl('''':'''''':chrDocList2);
          chrDocListErr = %scanrpl('''':'''''':chrDocListErr);

          //* Get Agency Info to replace tokens
          Chain (XPCO#:intFein:intMod) pag00;
          If %found (WMAGL);
             xxRRNA      = dbrrn3;
             strRRNTypeA = 'Agency';
             Chain (AGco#:AGfein:AGmod:XPfnd) paa00;
             strAgtInfo  = %Trim(AGName) + ' (Agency # ' +
                           %EditC(AAagt#:'X') + ')';
          Endif;

          If ftpFailed;
             SetLL 1 psc0p;
             Read(n) psc0p;
             If Not %Eof(stscp);
                If SCurl5 <> ' ';
                   strEmailFtp = SCurl5;
                   strEmailFtp = %scanrpl(',':' ':strEmailFtp);
                EndIf;
             EndIf;
             strSubject = 'Failed to ftp all document';
             strBody = 'Failed to ftp. ' +
                       crlf + crlf +
                       %Trim(strAgtInfo) +
                       crlf + crlf +
                       %Trim(chrDocListErr) +
                       crlf + crlf +
                       'Tried to send to system ' +
                       %Trim(chrToSystem) + ' directory ' +
                       %Trim(chrToDir);
          Else;
             strEmailFtp = chrDocMailAgt;
             strEmailFtp = %scanrpl(',':' ':strEmailFtp);

             If SPemlSubF = ' ';
                strSubject = 'Document ftp success';
             Else;
                //* Replace tokens with data.
                strSubject = ReplaceTokensEmail(SPemlSubF:dsRunInfo:
                             xxRRNA:strRRNTypeA);
             EndIf;

             If SPemlBodyF = ' ';
                strBody = 'Document ftp success';
             Else;
                //* Replace tokens with data.
                strBody = ReplaceTokensEmail(SPemlBodyF:dsRunInfo:
                             xxRRNA:strRRNTypeA);
             EndIf;

             strBody = %Trim(strBody) + crlf + crlf +
                       %Trim(chrDocList2) +
                       crlf + crlf + 'Sent to system ' +
                       %Trim(chrToSystem) + ' directory ' +
                       %Trim(chrToDir);
          EndIf;

          //* Build the SNDEMAIL command to send the email(s).
          If XPUsrMail <> ' ';
             strCmd = 'SNDEMAIL FROM(''' + %Trim(XPUsrMail) + ''') ';
          Else;
             strCmd = 'SNDEMAIL FROM(*NONE) ';
          Endif;

          strCmd = %Trim(strCmd) +
             ' TO(' + %Trim(strEmailFtp) + ') ' +
             'SUBJECT(''' + %Trim(strSubject) + ''') ' +
             'BODY(''' + %Trim(strBody) + ''') ' +
             'SUFFIX(' + suffix + ') ' +
             'RETRY(' + %char(2) +') ';

           //* Execute the SNDEMAIL command to send the email(s).
           If strEmailFtp <> ' ';
              Monitor;
                 RunCmd(strCmd:%Len(%Trim(strCmd)));
              On-Error;
              EndMon;
           EndIf;

            //* Record email details in log file.
            logType     = 'E';
            emailType   = 'F';
            getPrepRcd  = *Off;
            Exsr ProcessLog;

       EndSR;

       //*** Begin Add *** 03/30/23 ****************************************
       //===================================================================
       // Generate Report and CSV
       //===================================================================
       BegSR GenerateReport;

          // Read thru Log File
          isFirst = *On;
          strSql  = 'Select * from sxemp where EMjob# = ' +
                    %EditC(Q1jobn:'X') + ' And EMjobUser = ''' +
                    %Trim(Q1cUsr) + ''' And EMjobName = ''' +
                    %Trim(Q1job) + ''' And EMresults <> ''' +
                    'Ftp status has been emailed.'' ' +
                    'Order by EMco#, EMfnd, EMfein, ' +
                    'EMmod, EMemp#, EMdiv, EMfyr, EMcase';

          Chain (UPco#) pco01;
          companyHead    = %Editc(COco#:'X') + '-' + %Trim(COname);
          chrType        = %Subst(XPtype:1:50);
          chrJobName     = Q1job;
          chrJobUser     = Q1cUsr;
          chrJob#        = %EditC(Q1jobN:'X');

          If xxPrinter = ' ' Or xxprint <> 'Y';
             chrAllPrinter = 'none';
          Else;
             chrAllPrinter  = xxPrinter;
          EndIf;

          If xxPrinterI = ' ';
             chrInsPrinter = 'none';
          Else;
             chrInsPrinter  = xxPrinterI;
          EndIf;

          If xxPrinterA = ' ';
             chrAgtPrinter = 'none';
          Else;
             chrAgtPrinter  = xxPrinterA;
          EndIf;

          strCmd = 'OVRPRTF FILE(QSYSPRT7) PAGESIZE(66 132) ' +
                   //*** Beg Chg *** 05/20/24 ***************//
                   //***'LPI(6) CPI(10) OVRFLW(60)';
                   'LPI(6) CPI(10) OVRFLW(60) OUTQ(QPRINT) HOLD(*YES)';
                   //*** End Chg *** 05/20/24 ***************//
          Monitor;
             RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-error;
          Endmon;

          Open qsysprt7;

          Except PageBrk;
          Except Head1;

          Exec Sql Prepare sqlSXEMP From :strSql;
          Exec Sql Declare cursorSXEMP Cursor For sqlSXEMP;
          Exec Sql Open cursorSXEMP;

          chrResults      = ' ';
          chrInsMail      = 'no';
          chrAgtMail      = 'no';
          chrInsEmail     = 'none';
          chrAgtEmail     = 'none';
          chrAgtPath      = 'none';
          chrAgtFtp       = 'none';
          decGroup#       = 0;
          decFein         = 0;
          decMod          = 0;
          decEmp#         = 0;
          decDiv          = 0;
          decYear         = 0;
          decCase         = 0;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorSXEMP into :dsSXEMP;
             If IsValidFetch(sqlStt);

                If dsSXEMP.EMemp# = 0;
                   Iter;
                EndIf;

                If *INoa = *On;
                   *INoa           = *Off;
                   Except PageBrk;
                EndIf;

                If decGroup# <> 0;
                   If decGroup# <> dsSXEMP.EMfnd  Or
                      decEmp#   <> dsSXEMP.EMemp# Or
                      decDiv    <> dsSXEMP.EMdiv  Or
                      decYear   <> dsSXEMP.EMfyr  Or
                      decCase   <> dsSXEMP.EMcase Or
                      decFein   <> dsSXEMP.EMfein Or
                      decMod    <> dsSXEMP.EMmod;
                      Except Detail1;
                      //*** Begin Add *** 04/23/24 *************************
                      PrintDocPath(decGroup#:decEmp#:decDiv:decYear);
                      Except Detail3;
                      //*** End   Add *** 04/23/24 *************************
                      chrResults  = ' ';
                      chrInsMail  = 'no';
                      chrAgtMail  = 'no';
                      chrInsEmail = 'none';
                      chrAgtEmail = 'none';
                      chrAgtPath  = 'none';
                      chrAgtFtp   = 'none';
                   EndIf;
                EndIf;

                decGroup#   = dsSXEMP.EMfnd;
                decFein     = dsSXEMP.EMfein;
                decMod      = dsSXEMP.EMmod;
                decEmp#     = dsSXEMP.EMemp#;
                decDiv      = dsSXEMP.EMdiv;
                decYear     = dsSXEMP.EMfyr;
                decCase     = dsSXEMP.EMcase;

                Chain (dsSXEMP.EMco#:dsSXEMP.EMfnd:dsSXEMP.EMemp#:
                       dsSXEMP.EMdiv) pem01;
                chrName     = %Subst(EMname:1:30);

                If chrResults = ' ';
                   chrResults  = dsSXEMP.EMresults;
                Else;
                   chrResults  = %Trim(chrResults) + ', ' +
                                 %Trim(dsSXEMP.EMresults);
                EndIf;

                If dsSXEMP.EMinsEmail <> ' ';
                   chrInsEmail = dsSXEMP.EMinsEmail;
                EndIf;

                If dsSXEMP.EMagtEmail <> ' ';
                   chrAgtEmail = dsSXEMP.EMagtEmail;
                EndIf;

                Select;
                When %Subst(dsSXEMP.EMcmd:1:8) = 'SNDEMAIL';
                   If dsSXEMP.EMinsEmail <> ' ' And
                      dsSXEMP.EMinsEmail <> 'none';
                      chrInsMail  = 'yes';
                   EndIf;

                   If dsSXEMP.EMagtEmail <> ' ' And
                      dsSXEMP.EMagtEmail <> 'none' And
                      chrAgtMail         <> 'ftp' And
                      chrAgtMail         <> 'ftp error';
                      chrAgtMail = 'yes';
                   EndIf;
                When %Subst(dsSXEMP.EMcmd:1:8) = 'SNDFTPD';
                   If dsSXEMP.EMmsgID = ' ';
                      chrAgtMail = 'ftp';
                   Else;
                      chrAgtMail = 'ftp error';
                   EndIf;
                EndSL;

                If %Subst(dsSXEMP.EMcmd:1:7) = 'SNDFTPD';
                   str = %Scan('TOSYSTEM(''':dsSXEMP.EMCMD:1) + 10;
                   end = %Scan('''':dsSXEMP.EMcmd:str);
                   chrAgtFtp = %Subst(dsSXEMP.EMcmd:str:end-str);
                EndIf;

                If dsSXEMP.EMfein <> 0;
                   Open wmagl;
                   Chain (dsSXEMP.EMco#:dsSXEMP.EMfein:dsSXEMP.EMmod) pag00;
                   chrAgtName  = %Trim(AGname) +
                                ' (' + %EditC(dsSXEMP.EMfein:'X') +
                                '/'+ %EditC(dsSXEMP.EMmod:'X') + ')';
                   Close wmagl;
                Else;
                   chrAgtName  = ' ';
                EndIf;

                //*** Begin Del *** 04/23/24 *******************************
                //chrDocPath = %Trim(dsSXEMP.EMdir) + %Trim(dsSXEMP.EMfile);
                //***chrDocPath = %ScanRpl('"':'':chrDocPath);
                //*** chrDocPath = %ScanRpl('''''':'''':chrDocPath);
                //*** End   Del *** 04/23/24 *******************************

                If %Subst(dsSXEMP.EMcmd:1:7) = 'SNDFTPD';
                   str = %Scan('TOSYSTEM(''':dsSXEMP.EMCMD:1) + 10;
                   end = %Scan('''':dsSXEMP.EMcmd:str);
                   chrAgtFtp = %Subst(dsSXEMP.EMcmd:str:end-str);
                EndIf;

                If %Subst(dsSXEMP.EMcmd:1:7) = 'PRTSTMF';
                   If %Scan('AgentCopy':dsSXEMP.EMfromUNCN) <> 0;
                      chrAgtPath = dsSXEMP.EMfromUNCN;
                   EndIf;
                EndIf;
             Else;
                 Leave;
             Endif;
          Enddo;

          If *INoa = *On;
             *INoa           = *Off;
             Except PageBrk;
          EndIf;
          Except Detail1;
          //*** Begin Add *** 04/23/24 *************************************
          PrintDocPath(decGroup#:decEmp#:decDiv:decYear);
          Except Detail3;
          //*** End   Add *** 04/23/24 *************************************

          If *INoa = *On;
             *INoa = *Off;
             Except PageBrk;
          EndIf;

          Except Total1;
          Close qsysprt7;

          // Convert report to PDF and place in user folder
          chrReportPath = %Trim(chrFromDir) + 'LogReport_' +
                          %EditC(Q1jobN:'X') + '_' +
                          %EditC(DateTo8(systemDate()):'X') + '.PDF';

          strCmd = 'SPLAMATIC/SPLCONVERT File(QSYSPRT7) ToObj(''' +
                   %Trim(chrReportPath) + ''') Job(' + %EditC(Q1jobN:'X') +
                   '/' + %Trim(Q1cUsr) + '/' + %Trim(Q1job) +
                   ') Transform(*PDF811)';
          Monitor;
             RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-error;
          Endmon;

          // Create current jobs data set
          strCmd = 'CPYF FromFile(sxeml1) ToFile(qtemp/sxempjob) ' +
                   'MbrOpt(*replace) CrtFile(*yes) ' +
                   'IncRel((*IF EMjob# *EQ ' + %EditC(Q1jobN:'X') + ') ' +
                   '(*AND EMjobName *EQ ''' + %Trim(Q1job) + ''') '   +
                   '(*AND EMjobUser *EQ ''' + %Trim(Q1cUsr) + ''')) ' +
                   'FmtOpt(*map *drop)';
          Monitor;
             RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-error;
          Endmon;

          // Create CSV File
          strCmd = 'CPYTOIMPF FromFile(qtemp/sxempjob) ToStmF(''' +
                   %Trim(chrFromDir) + '/sxemp' + %Editc(Q1jobN:'X') +
                   '-' + %Trim(Q1cUsr) + '-' + %Trim(Q1job) + '.csv'') ' +
                   'MbrOpt(*replace) StmfCCSID(*PCASCII) RcdDlm(*CRLF) ' +
                   'RmvBlank(*TRAILING) AddColNam(*SQL)';
          Monitor;
             RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-error;
          Endmon;

       EndSR;
       //*** End   Add *** 03/30/23 ****************************************

       //***********************************************************************************
       // PrintDocument - Print Document
       //***********************************************************************************
       Begsr PrintDocument;

          strStream      = strPath;

          strCmd = 'PRTSTMF STMF(''' + %Trim(strStream) +
                   ''') OUTQ(' + chrOutQ +')';

          //* Execute the PRTSTMF command to print merged document
          Monitor;
             RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-error;
             message = 'Print failure:  OutQ:  ' + %trim(chrOutQ) + ' Form Ty' +
              'pe: ' + %trim(XPForm) + ' Hold: ' + %trim(strHold) + ' Save: ' +
              %trim(strSave) + '  Please see Job Log.';
             // Print Error
             errorFound = *On;
          Endmon;

          logType = 'P';
          getPrepRcd = *Off;
          Exsr ProcessLog;

       EndSr;
       //***********************************************************************************
       // Threshold - Check threshold factors
       //***********************************************************************************
       Begsr Threshold;

          //*** Begin Add *** 01/16/23 ************************************
          // Initialize totals
          decInsDocE      = 0;
          decInsDocC      = 0;
          decAgtDocE      = 0;
          decAgtDocC      = 0;
          decAgtTotalFtp  = 0;
          decDocTotals    = 0;
          printReport     = *On;

          // Access the LDA
          In *Lock *dtaara;
          Out *dtaara;

          // Leave this routine if XPlink = blank
          //*** begin chg ***  01/06/25  *********************************
          //*****If XPlink = ' ';
          If XPlink = ' ' or xxEmail <> 'Y';
          //*** end chg *****  01/06/25  *********************************
             printReport     = *Off;
             LeaveSR;
          Endif;

          Chain(n) (XPtype) psp00;
          //*** End   Add *** 01/16/23 ************************************
          //*** begin add ***  01/09/25  *********************************
          //* Designate where to retrieve Java class file for document merging.
          strCommand =
           'ADDENVVAR CLASSPATH ''/www/' + %trim(strLibEnv) + '/scriptura/' +
           'pdfbox-app-2.0.26.jar''';
          Monitor;
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          EndMon;

          //* Set maximum number of docs to merge at once, default is 100.
          If SC1107 <> 0;
             maxDocs = SC1107;
          Endif;
          //*** end add *****  01/09/25  *********************************

          //================================================================
          // Part 1 - Build insured and agency document list and set metrics
          //================================================================

          // Create and clear Agent Information Work file
          CreateAgtInfoTable();

          // Clear work fields
          Clear insuredDocList;

          // Get the from directory and mass file
          chrFromDir  = XPdir;
          chrMassPath = %Trim(XPdir) + %Trim(XPfile);
          chrFlowXml  = XPxml;

          // Read thru documents
          xmlFile2 = ' ';
          xmlLink = XPLink;
          hldXmlLink = XPLink;
          Setll key1XML xp100;
          Chain(n) key1XML xp100;
          If %Found(xmlprel1);
             DoU %Eof(xmlprel1);

                // Don't include smushed documents
                //*** Begin Chg *** 04/23/24 *******************************
                //*** If XPemp# = 0 And XPfein = 0;
                If (XPemp# = 0 And XPfein = 0) Or XPskip = 'Y';
                //*** End   Chg *** 04/23/24 *******************************
                   ReadE(n) key1XML xp100;
                   iter;
                EndIf;

                // Get the docment size
                chrPath    = %Trim(XPdir) + %Trim(XPfile);
                intLnkSize = 0;
                chrRtnCode = ' ';
                RtvLnkSize(chrPath:intLnkSize:chrRtnCode);

                // Build document list
                If XPinsMail = ' ' Or XPinsMailA = ' ';
                   If insuredDocList = ' ';
                      insuredDocList = chrPath;
                   Else;
                      insuredDocList = %Trim(insuredDocList) + ',' +
                        %Trim(chrPath);
                   EndIf;
                EndIf;

                decDocTotals += 1;

                // Check to see if fein/mod being reported
                chrFeinMod  = %EditC(XPfein:'X') + %EditC(XPmod:'X');
                agtInfoRRN  = 0;
                decDocCount = 0;
                decDocSize  = 0;
                chrDocList_data  = ' ';
                chrDocList_len   = 1;
                chrDocMail       = ' ';

                Exec Sql
                  Select RRN(AgentInfoTbl), docCount, docSize,
                         docList, docMail, hasMail, docMailAgt
                  Into :agtInfoRRN, :decDocCount, :decDocSize,
                       :chrDocList, :chrDocMail, :chrHasMail,
                       :chrDocMailAgt
                  From qtemp/AgentInfoTbl
                  Where feinMod = :chrFeinMod;

                If agtInfoRRN = 0;
                   chrFeinMod      = chrFeinMod;
                   chrThreshold    = 'N';
                   decDocCount    += 1;
                   decDocSize     += intLnkSize;
                   chrDocList_data = chrPath;
                   chrDocList_len  = %Len(%Trim(chrDocList_data));
                   chrDocMailAgt   = ' ';

                   If XPagtMail = ' ';
                      chrDocMail   = 'N';
                   Else;
                      chrDocMail   = XPagtMail;
                   EndIf;

                   If XPagtMailA = ' ' And XPinsMailA = ' ' And
                      XPmailAdd  = ' ';
                      chrHasMail       = 'N';
                   Else;
                      chrHasMail       = 'Y';
                   EndIf;

                   //*** Beg Add ***** 04/12/24 *************************************
                   If XPagtMailA <> *Blanks;
                   //*** End Add ***** 04/12/24 *************************************
                   chrDocMailAgt =
                      BuildAgentFtpEmailList(XPagtMailA:chrDocMailAgt);
                   //*** Beg Add ***** 04/12/24 *************************************
                   Endif;
                   //*** End Add ***** 04/12/24 *************************************

                   // Create record
                   Exec Sql
                      Insert into qtemp/AgentInfoTbl
                      Values(:chrFeinMod, :decDocCount, :decDocSize, ' ',
                             :chrDocList, :chrDocMail, :chrHasMail,
                             :chrDocMailAgt);
                Else;
                   decDocCount += 1;
                   decDocSize  += intLnkSize;

                   If chrDocList_data = ' ';
                      chrDocList_data = %Trim(chrPath);
                      chrDocList_len  = %Len(%Trim(chrDocList_data));
                      If XPagtMail = ' ';
                         chrDocMail   = 'N';
                      Else;
                         chrDocMail   = XPagtMail;
                      EndIf;
                   Else;
                      chrDocList_data =
                        %Trim(chrDocList_data) + ',' + %Trim(chrPath);
                      chrDocList_len  = %Len(%Trim(chrDocList_data));
                      If XPagtMail = ' ';
                         chrDocMail   = %Trim(chrDocMail) + 'N';
                      Else;
                         chrDocMail   = %Trim(chrDocMail) + XPagtMail;
                      EndIf;
                   EndIf;

                   //*** Beg Add ***** 04/12/24 *************************************
                   If XPagtMailA <> *Blanks;
                   //*** End Add ***** 04/12/24 *************************************
                   chrDocMailAgt =
                      BuildAgentFtpEmailList(XPagtMailA:chrDocMailAgt);
                   //*** Beg Add ***** 04/12/24 *************************************
                   Endif;
                   //*** End Add ***** 04/12/24 *************************************

                   // Update count, size and list
                   Exec Sql
                      Update qtemp/AgentInfoTbl
                      Set docCount = :decDocCount, docSize = :decDocSize,
                          docList = :chrDocList, docMail = :chrDocMail,
                          docMailAgt = :chrDocMailAgt
                      Where feinMod = :chrFeinMod;
                EndIf;
                ReadE(n) key1XML xp100;
             EndDo;
          Endif;

          XPlink = hldXmlLink;

          //================================================================
          // Part 2 - Process Insured Document List
          //================================================================

          // Create Insured Document Name
          chrInsInfoDocID = 'InsuredCopy_' + %EditC(Q1jobn:'X') + '_' +
             %Char(%TimeStamp()) + '.PDF';
          chrInsInfoDocID = %ScanRpl(',':'':chrInsInfoDocID);
          chrInsInfoDocID = %ScanRpl('''':'':chrInsInfoDocID);
          chrInsInfoPath  = %Trim(chrFromDir) +
                            %Trim(chrInsInfoDocID);
          isFirst         = *On;

          // Let's merge insured documents
          chrInsuredPath = 'none';
          If insuredDocList <> ' ';

             // Extract Documents
             mergedDocExists = *Off;
             //*** begin add ***  01/09/25  *********************************
             numDocs = 0;
             //*** end add *****  01/09/25  *********************************
             str = 1;
             DoU pos = 0;
                pos = %Scan(',':insuredDocList:str);
                If pos = 0;
                   // Merge Here
                   chrExtractedDocID =
                      %Subst(insuredDocList:str);
             //*** begin chg ***  01/09/25  *********************************
             //****MergeDocuments(chrInsInfoPath:chrExtractedDocID);
                   numDocs = maxDocs + 1;
                   MergeDocuments(chrInsInfoPath:chrExtractedDocID:numDocs);
             //*** end chg *****  01/09/25  *********************************
                   mergedDocExists = *On;
                   mergeType       = 'I';
                   logType         = 'M';
                   getPrepRcd      = *On;
                   Exsr ProcessLog;
                   decInsDocC += 1;
                   Leave;
                Else;
                   end = pos;
                   len = end - str;
                   chrExtractedDocID =
                      %Subst(insuredDocList:str:len);
                   str = end + 1;
             //*** begin chg ***  01/09/25  *********************************
             //****MergeDocuments(chrInsInfoPath:chrExtractedDocID);
                   MergeDocuments(chrInsInfoPath:chrExtractedDocID:numDocs);
             //*** end chg *****  01/09/25  *********************************
                   mergedDocExists = *On;
                   mergeType       = 'I';
                   logType         = 'M';
                   getPrepRcd      = *On;
                   Exsr ProcessLog;
                   decInsDocC += 1;
                EndIf;
             EndDo;

             // Did a document get merged
             If mergedDocExists;
                chrInsuredPath = %Subst(chrInsInfoPath:1:60);
             EndIf;

             // Print here
             If xxPrintI = 'Y' And mergedDocExists;
                strPath        = chrInsInfoPath;
                chrOutQ        = xxPrinterI;
                ExSr PrintDocument;
             EndIf;

          EndIf;

          //================================================================
          // Part 3 - Process Agency Document List
          //================================================================

          // Access the Scriptura Setup
          Setll 1 psc0p;
          Read(n) psc0p;

          // Build Cursor to read thru AgentInfoTbl
          sqlAgtInfo = 'Select feinMod, docCount, docSize, docList, ' +
                       'docMail, hasMail, docMailAgt ' +
                       'From qtemp/AgentInfoTbl';
          Exec Sql close cursorAgtInfo;
          Exec Sql Prepare sqlAgtInfo From :sqlAgtInfo;
          Exec Sql Declare cursorAgtInfo Cursor For sqlAgtInfo;
          Exec Sql Open cursorAgtInfo;

          // Open Agency File
          If Not %Open(wmagl);
             Open wmagl;
          EndIf;

          DoU not IsValidFetch(sqlstt);
             chrDocList_data  = ' ';
             chrDocList_len   = 1;
             Exec SQL Fetch Next From cursorAgtInfo
                Into :chrFeinMod, :decDocCount, :decDocSize,
                     :chrDocList, :chrDocMail, :chrHasMail, :chrDocMailAgt;
             If IsValidFetch(sqlstt);
                chrThreshold = 'N';
                isFtpInfo = *Off;
                intFein   = %Dec(%Subst(chrFeinMod:1:9):9:0);
                intMod    = %Dec(%Subst(chrFeinMod:10:3):3:0);
                Chain (XPco#:intFein:intMod) pag00;
                If %Found(wmagl);
                   // Check File size threshold
                   If AGetSize = 0;
                      If decDocSize > SC1104*1000000 And SC1104 <> 0;
                         chrThreshold = 'Y';
                      EndIf;
                   Else;
                      If decDocSize > AGetSize*1000000;
                         chrThreshold = 'Y';
                      EndIf;
                   Endif;
                   // Check File count threshold
                   If AGetCnt = 0;
                      If decDocCount > SC1105 And SC1105 <> 0;
                         chrThreshold = 'Y';
                      EndIf;
                   Else;
                      If decDocCount > AGetCnt;
                         chrThreshold = 'Y';
                      EndIf;
                   Endif;
                   If AGeFtp <> ' ';
                      isFtpInfo = *On;
                   EndIf;
                EndIf;

                // Update the threshold
                Exec Sql
                   Update qtemp/AgentInfoTbl
                   Set threshold = :chrThreshold
                   Where feinMod = :chrFeinMod;


                // Now Merge Documents
                If chrDocList_data <> ' ';
                   decDocIndex = 1;
                   // Create Agent Document Name
                   chrAgtInfoDocID = 'AgentCopy_' + %Trim(AGname) + '_' +
                    %EditC(Q1jobn:'X') + '_' + %Char(%TimeStamp()) + '.PDF';
                   chrAgtInfoDocID = %ScanRpl(',':'':chrAgtInfoDocID);
                   chrAgtInfoDocID = %ScanRpl('''':'':chrAgtInfoDocID);
                   chrAgtInfoPath  = %Trim(chrFromDir) +
                                     %Trim(chrAgtInfoDocID);

                   isFirst         = *On;
                   isFirstDoc      = *On;
                   isFirstDocErr   = *On;
                   ftpFailed       = *Off;
                   documentFtpD    = *Off;
                   mergedDocExists = *Off;

                   // Extract Documents
                   str = 1;
                //*** begin add ***  01/09/25  *********************************
                   numDocs = 0;
                //*** end add *****  01/09/25  *********************************
                   DoU pos = 0;
                      pos = %Scan(',':chrDocList_data:str);
                      If pos = 0;
                         // Merge Here
                         chrExtractedDocID = %Subst(chrDocList_data:str);
                         chrExtractedDocID =
                            %ScanRpl(',':'':chrExtractedDocID);
                         chrExtractedDocID =
                            %ScanRpl('''':'':chrExtractedDocID);
                         chrMail      = %Subst(chrDocMail:decDocIndex:1);
                         decDocIndex += 1;

                         // FTP document if threshold exceeded
                         Select;
                         When chrThreshold = 'Y' And chrMail = 'Y' And
                              isFtpInfo = *On;
                            FtpDocuments(chrExtractedDocID);
                            logType    = 'F';
                            getPrepRcd = *On;
                            Exsr ProcessLog;
                            decAgtTotalFtp += 1;
                            // If ftp failed then copy was attempted
                            If ftpFailed;
                               // Copy the document to ftp failure directory
                               ExSr CopyDocToFtpFailDirectory;
                               getPrepRcd = *Off;
                               logType    = 'C';
                               Exsr ProcessLog;
                            EndIf;
                         // Merge Documents
                         When chrMail = 'N' Or chrHasMail = 'N' Or
                              (chrThreshold = 'Y' And chrMail = 'Y' And
                               isFtpInfo = *Off);
                         //*** begin chg ***  01/09/25  *********************************
                         //*****   MergeDocuments(chrAgtInfoPath:chrExtractedDocID);
                            numDocs = maxDocs + 1;
                            MergeDocuments(chrAgtInfoPath:chrExtractedDocID:
                             numDocs);
                         //*** end chg *****  01/09/25  *********************************
                            mergedDocExists = *On;
                            mergeType       = 'A';
                            logType         = 'M';
                            getPrepRcd      = *On;
                            Exsr ProcessLog;
                            decAgtDocC += 1;
                         Other;
                            decAgtDocE += 1;
                         EndSl;

                         Leave;
                      Else;
                         // Find end of file name
                         end = pos;
                         len = end - str;
                         chrExtractedDocID =
                            %Subst(chrDocList_data:str:len);
                         str = end + 1;
                         chrMail      = %Subst(chrDocMail:decDocIndex:1);
                         decDocIndex += 1;

                         // FTP document if threshold exceeded
                         Select;
                         When chrThreshold = 'Y' And chrMail = 'Y' And
                              isFtpInfo = *On;
                            FtpDocuments(chrExtractedDocID);
                            logType    = 'F';
                            getPrepRcd = *On;
                            Exsr ProcessLog;
                            decAgtTotalFtp += 1;
                            // If ftp failed then copy was attempted
                            If ftpFailed;
                               // Copy the document to ftp failure directory
                               ExSr CopyDocToFtpFailDirectory;
                               getPrepRcd = *Off;
                               logType    = 'C';
                               Exsr ProcessLog;
                            EndIf;
                         // Merge Documents
                         When chrMail = 'N' Or chrHasMail = 'N' Or
                              (chrThreshold = 'Y' And chrMail = 'Y' And
                               isFtpInfo = *Off);
                         //*** begin chg ***  01/09/25  *********************************
                         //*****   mergeDocuments(chrAgtInfoPath:chrExtractedDocID);
                            MergeDocuments(chrAgtInfoPath:chrExtractedDocID:
                             numDocs);
                         //*** end chg *****  01/09/25  *********************************
                            mergedDocExists = *On;
                            mergeType       = 'A';
                            logType         = 'M';
                            getPrepRcd      = *On;
                            Exsr ProcessLog;
                            decAgtDocC += 1;
                         Other;
                            decAgtDocE += 1;
                         EndSl;
                      EndIf;
                   EndDo;

                   // Send FTP success/failure Email
                   If documentFtpD;
                      ExSr EmailFtpStatus;
                   EndIf;

                   // Print Here
                   If xxPrintA = 'Y' And  mergedDocExists;
                      strPath = chrAgtInfoPath;
                      chrOutQ = xxPrinterA;
                      ExSr PrintDocument;
                   EndIf;
                EndIf;
             EndIf;
          EndDo;

          // Close Agency file
          If %Open(wmagl);
             Close wmagl;
          EndIf;

          // Reaccess the master record
          Chain keyXML xp100;

       EndSr;
       //*** End   Add *** 09/20/22 ****************************************
     C**************************************************************************************
     C*
     C* USER return the user that submitted the document request in QQCall.
     C*
     C**************************************************************************************
     C     USER          Begsr
     C*
     C                   Eval      QQCall = XPUser
     C*
     C                   Endsr                                                  USER Subr
     C**************************************************************************************
     C*
     C* UpdXmlName - change the file name to have a .XML extension
     C*
     C**************************************************************************************
     C     UpdXmlName    Begsr
     C*
      /free
           If %scan('.PDF':xmlFile) <> 0;
              xmlFile = %subst(xmlFile:1:%scan('.PDF':xmlFile)) + 'PDF';
              Chain(n) (xmlFile) xp103;
              If %found;
                 xmlFile = XPXml;
              Endif;
           Endif;
      /end-free
     C*
     C                   Endsr                                                  UpdXmlName Subr
     C**************************************************************************************
     C*
     C* CHKXML - Check existance of XML file to generate an error message or error report.
     C*
     C**************************************************************************************
     C     CHKXML        Begsr
     C*
     C                   Eval      xmlUNCN = %trim(XPXmlF) + %trim(XPXml)
     C*
     C                   Z-add     3             limit
     C                   Exsr      CheckDoc
     C*
     C* Return a message or generate a spool file if an error was returned.
     C*
     C                   Clear                   QQReturn
     C*
     C                   If        not success
     C*
     C                   Select
     C*
     C                   When      QQCall = 'CHKXMLM'
     C                   Eval      QQReturn = 'The java program did not gener' +
     C                             'ate an XML file - Scriptura cannot be ' +
     C                             'launched.'
     C*
     C                   When      QQCall = 'CHKXMLO'
     C                   Eval      message = 'Processing did not generate an ' +
     C                             'XML file.  Scriptura will not be called.'
     C                   TIME                    DSDTTM
     C                   Open      Qsysprt
     C                   Except    Head
     C                   Close     Qsysprt
     C                   Eval      QQReturn = 'Error'
     C*
     C                   Endsl
     C*
     C                   Endif
     C*
     C                   Endsr                                                  CHKXML Subr
     C**************************************************************************************
     C*
     C* CheckDoc - Check existance of document file, with retries after 15 seconds in case
     C*            the document has not arrived on IFS yet.
     C*
     C**************************************************************************************
     C     CheckDoc      Begsr
     C*
     C                   Z-add     1             x
     C*
     C                   Dow       x < limit
     C*
     C                   Callp     ChkDlo(xmlUNCN:success)
     C*
     C                   If        success
     C                   Leavesr
     C                   Else
     C                   Callp     Sleep(15)
     C                   Eval      x = x + 1
     C                   Endif
     C*
     C                   Enddo
     C*
     C                   Endsr                                                  CheckDoc Subr
     C**************************************************************************************
     C*
     C* PRTERROR - Print an error message.
     C*
     C**************************************************************************************
     C     PRTERROR      Begsr
     C*
     C                   Clear                   message
     C                   Clear                   message2
     C                   Clear                   message3
     C                   Eval      message = %trim(%subst(QQReturn:1:120))
     C                   Eval      message2 = %trim(%subst(QQReturn:121:120))
     C                   Eval      message3 = %trim(%subst(QQReturn:241:10))
     C                   TIME                    DSDTTM
     C                   Open      Qsysprt
     C                   Except    Head
     C                   Except    Head2
     C                   Close     Qsysprt
     C*
     C                   Endsr                                                  PRTERROR Subr
     C**************************************************************************************
     C*
     C* XMLREQUEST - Request an XML file name and write record to XMLPREP to hold it.
     C*
     C**************************************************************************************
     C     XMLREQUEST    Begsr
      /free

          If SCXMLF <> ' ';
             fldValue = %trim(SCXMLF);
          Endif;

          //* Call subroutine to build the prefix string for file.
          Exsr BuildXMLStr;

          myStamp = %subst(%char(%timestamp:*iso0):1:17);
          //*** begin add ***  08/04/23  *************************************
          myStampFull = %subst(%char(%timestamp():*iso0):1:20);
          //*** end add *****  08/04/23  *************************************
          xmlCheck = *off;

          DoU xmlCheck = *on;

          //*** begin add ***  08/04/23  *************************************
          //*****   xmlFile = 'Flow_XML' + '_' + myStamp + '.XML';
             xmlFile = 'Flow_XML' + '_' + myStampFull + '_' + %trim(Q1User) +
              '.XML';
          //*** begin add ***  08/04/23  *************************************
             outFile = %trim(xmlString) + '_' + myStamp + '.PDF';

             Chain(n) (xmlFile) xp100;
             //* Also make sure xml file name is not used for a template xml file.
             If not %found;
                Chain(n) (xmlFile) xp102;
             Endif;
             If not %found;
                Chain(n) (xmlFile) xt101;
             Endif;

             //*  Check if xml file name already exists and increment time stamp.
             If %found;
                myStampInt = %int(myStamp);
                myStampInt = myStampInt + 1;
                myStamp = %editc(myStampInt:'X');
          //*** begin add ***  08/04/23  *************************************
                myStampIntFull = %int(myStampFull);
                myStampIntFull = myStampIntFull + 1;
                myStampFull = %editc(myStampIntFull:'X');
          //*** end add *****  08/04/23  *************************************
             Else;
                xmlCheck = *on;
             Endif;

          Enddo;

          //* Build the template xml file name.

          xmlCheck = *off;
          myStampInt = %int(myStamp);
          myStampInt = myStampInt + 1;
          myStamp = %editc(myStampInt:'X');

          DoU xmlCheck = *on;

             If massReq = 'Y' and xmlLink = ' ';
             //* No XML template file name needed for mass process master record.
                xmlFileTemp = ' ';
                Leave;
             Endif;

             xmlFileTemp = %trim(xmlString) + '_' + myStamp + '.XML';

             Chain(n) (xmlFileTemp) xp100;
             If not %found;
                Chain(n) (xmlFileTemp) xp102;
             Endif;
             If not %found;
                Chain(n) (xmlFileTemp) xt101;
             Endif;

             //*  Check if xml file name already exists and increment time stamp.
             If %found;
                myStampInt = %int(myStamp);
                myStampInt = myStampInt + 1;
                myStamp = %editc(myStampInt:'X');
             Else;
                xmlCheck = *on;
             Endif;

          Enddo;

          Clear xp100;

          XPCo#  = keyCo#;
          XPSub# = keySub#;
          XPFnd  = keyFnd;
          XPEmp# = keyEmp#;
          XPDiv  = keyDiv;
          XPFyr  = keyFyr;
          XPCase = keyCase;
          XPCert = keyCert;
          XPExt  = keyExt;
        //*** begin add ***  07/09/20  ***************************************
          XPFein = keyFein;
          XPMod  = keyMod;
        //*** end add *****  07/09/20  ***************************************

          //*** Begin Add *** 01/16/23 ***********************************
          // Let's populate Agent fein/mod
          If xxCat = 'PS' Or xxCat = 'CS';
             outAgentName = ' ';
             outWMAHPrrn  = 0;
             outWMAAPrrn  = 0;
             outWMAGPrrn  = 0;
             GetEmpAgt(XPco#:XPfnd:XPemp#:XPdiv:XPfyr:0:outAgentName:
                       outWMAHPrrn:outWMAAPrrn:outWMAGPrrn);

             If Not %Open(wmagp);
                Open wmagp;
             EndIf;
             Chain outWMAGPrrn pag0p;
             If %Found(wmagp);
                XPFein = AGFein;
                XPMod  = AGMod;
             Endif;
             If %Open(wmagp);
                Close wmagp;
             EndIf;
          EndIf;
          //*** End   Add *** 01/16/23 ***********************************

          XPCat  = xxCat;
          XPOrder = 1;

          XPXML  = xmlFile;
          If massReq = 'Y' and xmlLink = ' ';
             XPLink = xmlFile;
          Endif;
          //* Records linked to a mass record do not create their own XML file.
          If xmlLink <> ' ';
             XPXML  = ' ';
             XPLink = xmlLink;
          //* Return the template xml file name for individual records in a mass process.
             xmlFile = xmlFileTemp;
          Endif;

          XPXMLT = xmlFileTemp;

          //* Build the folder path for the returned Document folder.

          xxPre = QQPrefix;

          Clear xxPath;
          Clear xxPflr;
          Clear xxDcid;
          Clear xxRtcd;
          xxSub# = '00000';
          Clear xxRoot;

          If xxPre <> *blank;
             SNextDocID(xxPath:xxDcid:xxPre:%editc(keyCo#:'X'):
              %editc(keyFnd:'X'):xxRtcd:xxSub#:xxPflr);
          Endif;

          DocRoot(xxRoot);

          XPDir  = '/' + %trim(xxRoot) + '/' + %trim(xxPath) + '/';

          //* Build User Output folder location and create if needed.

          Sa030r(suffix);

          If suffix <> ' ';
             userFolder = '/PDS/' + suffix + %trim(userFolderBase) +
              %trim(Q1User) + '/';
          Else;
             userFolder = %trim(userFolderBase) + %trim(Q1User) + '/';
          Endif;
          //* If alternative output folder is specified, use if it exists.
          If SCDSC8 <> ' ';
             If suffix <> ' ';
                altFolder = '/PDS/' + suffix + %trim(SCDSC8);
             Else;
                altFolder = %trim(SCDSC8);
             Endif;

             ChkDlo(altFolder:success);
             If success;
                userFolder = %trim(altFolder) + %trim(Q1User) + '/';
             Endif;

          Endif;

          //* Attempt to create folder
          Monitor;
             clear strCommand;
             strCommand = 'MKDIR DIR(''' + %trim(userFolder) + ''') DTAAUT(' +
              '*EXCLUDE) OBJAUT(*NONE)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          //* Attempt to update scriptura profile permissions
          Monitor;
             clear strCommand;
             strCommand = 'CHGAUT OBJ(''' + %trim(userFolder) + ''') USER(' +
              'SCRIPTURA) DTAAUT(*RWX) OBJAUT(*ALL)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          //* Attempt to update scriptura profile permissions
          Monitor;
             clear strCommand;
             strCommand = 'CHGAUT OBJ(''' + %trim(userFolder) + ''') USER(' +
              'PDSSCRIP) DTAAUT(*RWX) OBJAUT(*ALL)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          XPXMLF = %trim(fldValue);
          XPFile = outFile;

          XPCltID = clientID;

          If QQReturn <> ' ';
             processType = %trim(QQReturn);
             Chain(n) (processType) psp00;
             If %found;
                XPType = %trim(processType);
                XPScrp = SPScrO;
                XP3rdX = SP3XML;

                XPForm = SPForm;
                XPdir2 = SPpathExt;
                If XPdir2 <> *blanks;
                   XPfile2 = XPfile;
                Endif;
                XPservice = SPservice;
                //*** Beg   Add *** 09/11/23 *** WC2907 ******************
                XPusrmail = SPfemail;
                //*** End   Add *** 09/11/23 *** WC2907 ******************
                //*** Beg Add *** 01/16/24 *** 153WAD ******************
                XPCopyE   = SPCopyE;
                //*** End Add *** 01/16/24 *** 153WAD ******************

                //* Logo
                If XPFnd <> 0;
                   Setgt (processType:XPFnd:effDate) psp012;
                   Readpe(n) (processType:XPFnd) psp012;
                   Dow not %eof and S2Stat <> 'A';
                      Readpe(n) (processType:XPFnd) psp012;
                   Enddo;
                   If not %eof and S2Logo <> ' ';
                      XPLogo = S2Logo;
                      XPLogoPth = S2Path;
                   //*** begin add ***  09/25/24  *****************************
                   //*** begin chg ***  01/30/25  *****************************
                   //******   If SCFlg6 = 'Y' and SPInter <> ' ';
                      If SCFlg6 = 'Y' and SPInter = 'Y';
                   //*** end chg *****  01/30/25  *****************************
                         XPLogoPth = S2URL;
                      Endif;
                   //*** end add *****  09/25/24  *****************************
                   Endif;
                Endif;

                If XPFnd = 0 or XPLogo = ' ';
                   Setgt (processType:0:effDate) psp012;
                   Readpe(n) (processType:0) psp012;
                   Dow not %eof and S2Stat <> 'A';
                      Readpe(n) (processType:0) psp012;
                   Enddo;
                   If not %eof and S2Logo <> ' ';
                      XPLogo = S2Logo;
                      XPLogoPth = S2Path;
                   //*** begin add ***  09/25/24  *****************************
                   //*** begin chg ***  01/30/25  *****************************
                   //******   If SCFlg6 = 'Y' and SPInter <> ' ';
                      If SCFlg6 = 'Y' and SPInter = 'Y';
                   //*** end chg *****  01/30/25  *****************************
                         XPLogoPth = S2URL;
                      Endif;
                   //*** end add *****  09/25/24  *****************************
                   Endif;
                Endif;

           //* Signature
                Clear xxSig;
                Clear xxFolder;
                Clear xxSig2;
                Clear xxFolder2;

                If XPFnd <> 0;
                   Setgt (processType:XPFnd:effDate) psp013;
                   Readpe(n) (processType:XPFnd) psp013;
                   Dow not %eof and S3Stat <> 'A';
                      Readpe(n) (processType:XPFnd) psp013;
                   Enddo;
                   If not %eof and S3Sig <> ' ';
                      xxSig = %trim(S3Sig);
                      xxFolder = %trim(S3Path);
                      xxSig2 = %trim(S3Sig2);
                      xxFolder2 = %trim(S3Path2);
                   Endif;
                Endif;

                If XPFnd = 0 or xxSig = ' ';
                   Setgt (processType:0:effDate) psp013;
                   Readpe(n) (processType:0) psp013;
                   Dow not %eof and S3Stat <> 'A';
                      Readpe(n) (processType:0) psp013;
                   Enddo;
                   If not %eof and S3Sig <> ' ';
                      xxSig = %trim(S3Sig);
                      xxFolder = %trim(S3Path);
                      xxSig2 = %trim(S3Sig2);
                      xxFolder2 = %trim(S3Path2);
                   Endif;
                Endif;
                XPIntr = SPInter;

                If SPSCltT <> ' ';
                   templateName = %trim(SPSCltT);
                   templatePath = %trim(SPPathC);
                   inLanguage = %trim(SPLangC);
                Else;
                   templateName = %trim(SPSDefT);
                   templatePath = %trim(SPPathD);
                   inLanguage = %trim(SPLangD);
                Endif;

          //*
          //* Retrieve template/signature from template info file.
          //*
                Setgt (processType:effDate) psp010;
                Readpe(n) (processType) psp010;

                //*** begin chg ***  11/11/24  *****************************
                //*** If not %eof;
                Dow not %eof;
                   If S1stat = 'A';
                //*** end   chg ***  11/11/24  *****************************
                   If S1SCltT <> ' ';
                      templateName = %trim(S1SCltT);
                      templatePath = %trim(S1PathC);
                      inLanguage = %trim(S1LangC);
                   Else;
                      If S1SDefT <> ' ';
                         templateName = %trim(S1SDefT);
                         templatePath = %trim(S1PathD);
                         inLanguage = %trim(S1LangD);
                      Endif;
                   Endif;


                //*** begin chg ***  11/11/24  *****************************
                //*** Endif;
                      Leave;
                   Endif;
                   Readpe(n) (processType) psp010;
                Enddo;
                //*** end chg ***  11/11/24  *****************************

          //*
          //* Copy the signature over if it has been specified.
          //*
          //* Secondary signature
                If xxSig2 <> ' ' and xxFolder2 <> ' ';
                   svSig = xxSig;
                   svFolder = xxFolder;
                   xxSig = xxSig2;
                   xxFolder = xxFolder2;
                   Exsr MoveSig;
                   XPSig2 = XPSig;
                   xxSig = svSig;
                   xxFolder = svFolder;
                Endif;
                If xxSig <> ' ' and xxFolder <> ' ';
                   Exsr MoveSig;
                Endif;

                If XPIntr = 'Y';
                   XPInid = %trim(templateName);
                Else;
                   XPDcid = %trim(templateName);
                   XPPath = %trim(templatePath);
                Endif;

                //* Convert language into code.
                Exsr Language;
                XPLang = %trim(langReturn);

                //* If no Darts folder has been requested then place in user folder.
                If QQPrefix = '  ';
                   XPDir = %trim(userFolder);
                Else;
                   If (massReq = 'Y' and SPOutM = 'Y') or
                    (massReq <> 'Y' and SPOutS = 'Y');
                      XPOutFld = %trim(userFolder);
                   Endif;
                Endif;
             Endif;

          Endif;

          If XPType = *Blanks and SCurl9 <> *Blanks;
             XPDir = SCurl9;
          Endif;

          If XPType = *blanks and SCSig <> ' ' and SCSigFold <> ' ';
             xxSig = %trim(SCSig);
             xxFolder = %trim(SCSigFold);
             Exsr MoveSig;
          Endif;

          Chain(n) Q1User pup01;
          If %found;
             XPUsrName = %trim(UPusnm);
             //*** Beg Add *** 09/11/23 *** WC2907 *********************
             //***XPUsrMail = %trim(UPMail);
             If XPUsrMail = ' ';
                XPUsrMail = %trim(UPMail);
             Endif;
             //*** End Add *** 09/11/23 *** WC2907 *********************
             XPUsrStaf = %trim(UPStaf);
          Endif;

          In *Lock *dtaara;
          Out *dtaara;
          If xxLDATag = '%key%';
             Chain (xxLDAKey) xp104;
             If %found;
                If XPXml <> ' ' and XPLink <> ' ';
                   XPDart = X4Dart;
                   XPRMCD = X4RMCD;
                   XPData = 'Y';
                Else;
                   XPDart = X4Dart;
                   XPRMCD = X4RMCD;
                Endif;

                If X4OutFld = 'Y';
                   XPOutFld = %trim(userFolder);
                Else;
                   XPOutFld = ' ';
                Endif;

                XPHold = X4Hold;
                XPSave = X4Save;
                If XPForm = ' ';
                   XPForm = '*STD';
                Endif;

                //* Set printer fields for printing.
                If X4FORM <> ' ' and X4RTYPE = 'Y';
                   Chain (X4FORM) pp100;
                   If %found;
                      XPPnam = %trim(P1Name);
                      If P1Type = 'N';
                         XPPunc = %trim(P1UNCN);
                      Else;
                         XPOutq = %trim(P1KEY);
                         XPOLib = %trim(P1OLIB);
                      Endif;
                      XPCopy = X4Copy;
                      If XPCopy = 0;
                         XPCopy = 1;
                      Endif;
                      XPTray = X4Tray;
                   Endif;
                Endif;

                If X4Intr = 'Y';
                   XPMail  = X4Mail;
                   XPSubj  = X4Subj;
                   XPBody  = X4Body;
                   XPmailAdd = X4mailAdd;
                Endif;
             Endif;
          Else;
          //* Pull from LDA normally when %key% not found in LDA data.

          //* Set data on master record to trigger data call.
          If XPXml <> ' ' and XPLink <> ' ';
             XPDart = xxDart;
             XPRMCD = xxDartRMCD;
             XPData = 'Y';
          Else;
             XPDart = xxDart;
             XPRMCD = xxDartRMCD;
          Endif;

          If xxCopy = 'Y';
             XPOutFld = %trim(userFolder);
          Else;
             XPOutFld = ' ';
          Endif;

          XPHold = xxHold;
          XPSave = xxSave;
          If XPForm = ' ';
             XPForm = '*STD';
          Endif;

          //* Set printer fields for printing.
          If xxPrinter <> ' ' and xxPrint = 'Y';
             Chain (xxPrinter) pp100;
             If %found;
                XPPnam = %trim(P1Name);
                If P1Type = 'N';
                   XPPunc = %trim(P1UNCN);
                Else;
                   XPOutq = %trim(P1KEY);
                   XPOLib = %trim(P1OLIB);
                Endif;
                If xx#CopiesA <> ' ';
                   XPCopy = %int(xx#CopiesA);
                Endif;
                If XPCopy = 0;
                   XPCopy = 1;
                Endif;
                XPTray = xxTray;
             Endif;
          Endif;

          If xxEmail = 'Y';
             XPMail  = %trim(xxEmailTo);
             XPSubj  = %trim(xxSubject);
             XPBody  = %trim(xxBody);
          Endif;

          Endif;

          Chain (XPCO#) pco01;

          If TestGrp(XPCO#:XPFND) = *on;
             XPTstGrp = 'Y';
          Endif;

          XPEnviro = CheckEnv();

          If XPLogo = ' ';
             Chain (XPCO#:XPFND) pf200;
             If %found and XPFND <> 0 and F2SLogo <> ' ';
                XPLogo = F2SLogo;
                XPLogoPth = F2SPath;
             //*** begin add ***  09/25/24  *****************************
             //*** begin chg ***  01/30/25  *****************************
             //******   If SCFlg6 = 'Y' and XPIntr <> ' ';
                If SCFlg6 = 'Y' and XPIntr = 'Y';
             //*** end chg *****  01/30/25  *****************************
                   XPLogoPth = F2SURL;
                Endif;
             //*** end add *****  09/25/24  *****************************
             Else;
                XPLogo = COSLogo;
                XPLogoPth = COSPath;
             //*** begin add ***  09/25/24  *****************************
             //*** begin chg ***  01/30/25  *****************************
             //******   If SCFlg6 = 'Y' and XPIntr <> ' ';
                If SCFlg6 = 'Y' and XPIntr = 'Y';
             //*** end chg *****  01/30/25  *****************************
                   XPLogoPth = COSURL;
                Endif;
             //*** end add *****  09/25/24  *****************************
             Endif;
          Endif;

          If XPLogo <> ' ' and SPNoLogo = 'Y' and processType <> ' ';
             Clear XPLogo;
             Clear XPLogoPth;
          Endif;



          XPTempIFS = %trim(SCSigTemp);
          Sa030r(suffix);

          If suffix <> ' ' and %subst(xptempIFS:1:5) <> '/PDS/' and
           %subst(xptempIFS:1:5) <> '/pds/';
             XPTempIFS = '/PDS/' + suffix + %trim(SCSigTemp);
          Endif;
          XPTempWin = %trim(SCSigDir);

          //*** Begin Add *** 01/16/23 ********************************
          //* Determine recipients from API if indicated.

          Clear xxRRN;
          Clear strRRNType;

          Select;
          When XPCat = 'PS';
             Open WDELL;
             Chain (XPCO#:XPFND:XPEMP#:XPDIV:XPFYR) pel01;
             If %found (WDELL);
                xxRRN = dbrrn1;
                strRRNType = 'Policy';
             Endif;
             Close WDELL;
          When XPCat = 'CS';
             Open WMCML;
             Chain (XPCO#:XPFND:XPEMP#:XPDIV:XPFYR:XPCASE) pcm00;
             If %found (WMCML);
                xxRRN = dbrrn2;
                strRRNType = 'Claim';
             Endif;
             Close WMCML;
          When XPCat = 'AS';
             Open WMAGL;
             Chain (XPCO#:XPFEIN:XPMOD) pag00;
             If %found (WMAGL);
                xxRRN = dbrrn3;
                strRRNType = 'Agency';
             Endif;
             Close WMAGL;
          When XPCat = 'MC';
             Open FMCML1;
             Chain (XPCO#:XPSUB#:XPFND:XPFYR:XPCASE) mcm01;
             If %found (FMCML1);
                xxRRN = dbrrn4;
                strRRNType = 'Medical Case';
             Endif;
             Close FMCML1;
          When XPCat = 'PC';
             Open UMPCL11;
             Chain (XPCO#:XPSUB#:XPFND:XPFYR:XPCASE:XPCERT:XPEXT) mpc11;
             If %found (UMPCL11);
                xxRRN = dbrrn5;
                strRRNType = 'Pre Cert';
             Endif;
             Close UMPCL11;
          Endsl;

          Reset dsRunInfo;

          If XPType <> ' ';
             strType = XPType;
          Else;
             strType = XPDcid;
          Endif;

          dsRunInfo.runInfo = strType;
          dsRunInfo.docCat  = XPcat;
          dsRunInfo.docSub  = XPsubC;
          dsRunInfo.docLang = XPlang;
          If XPmail = '%mass%';
             strList = %trim(GetDefaultEmailRecipients(dsRunInfo:xxRRN:
               strRRNType:strInsuredEmail:strAgencyEmail:
               strInsuredEmailA:strAgencyEmailA));
          Endif;

          XPinsMail  = strInsuredEmail;
          XPagtMail  = strAgencyEmail;
          XPinsMailA = strInsuredEmailA;
          XPagtMailA = strAgencyEmailA;
          //*** End   Add *** 01/16/23 ********************************

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = Dateto7(SystemDate());

          Write xp100;
          //*** begin add ***  08/04/23  **********************************
          Feod xmlprel1;
          //*** end add *****  08/04/23  **********************************

      /end-free
     C                   Endsr                                                  XMLREQUEST Subr
     C*****************************************************************
     C**************************************************************************************
     C*
     C* TEMPLRQST - Request an additional template XML file name and XMLTEMP record.
     C*
     C**************************************************************************************
     C     TEMPLRQST     Begsr
      /free

          //* Set order value to be greater of XMLPREP or highest XMLTEMP.

          Chain(n) (outFile) xp103;
          xxOrder = XPOrder + 1;

          Setgt (outFile) xt100;
          Readpe(n) (outFile) xt100;

          If not %eof and XTOrder > XPOrder;
             xxOrder = XTOrder + 1;
          Endif;

          If SCXMLF <> ' ';
             fldValue = %trim(SCXMLF);
          Endif;

          //* Call subroutine to build the prefix string for file.
          Exsr BuildXMLStr;

          myStamp = %subst(%char(%timestamp:*iso0):1:17);
          xmlCheck = *off;

          DoU xmlCheck = *on;

             xmlFile = %trim(xmlString) + '_' + myStamp + '.XML';

             Chain(n) (xmlFile) xp100;
             //* Also make sure xml file name is not used for a template xml file.
             If not %found;
                Chain(n) (xmlFile) xp102;
             Endif;
             If not %found;
                Chain(n) (xmlFile) xt101;
             Endif;

             //*  Check if xml file name already exists and increment time stamp.
             If %found;
                myStampInt = %int(myStamp);
                myStampInt = myStampInt + 1;
                myStamp = %editc(myStampInt:'X');
             Else;
                xmlCheck = *on;
             Endif;

          Enddo;

          Clear xt101;

          XTFile = outFile;
          XTXMLT = xmlFile;
          XTXMLF = %trim(fldValue);

          XTCat  = xxCat;

          If QQReturn <> ' ';
             processType = %trim(QQReturn);
             Chain(n) (processType) psp00;
             If %found;
                XTType = %trim(processType);

                XTIntr = SPInter;

                If SPSCltT <> ' ';
                   templateName = %trim(SPSCltT);
                   templatePath = %trim(SPPathC);
                   inLanguage = %trim(SPLangC);
                Else;
                   templateName = %trim(SPSDefT);
                   templatePath = %trim(SPPathD);
                   inLanguage = %trim(SPLangD);
                Endif;

                If XTIntr = 'Y';
                   XTInid = %trim(templateName);
                Else;
                   XTDcid = %trim(templateName);
                   XTPath = %trim(templatePath);
                Endif;

                //* Convert language into code.
                Exsr Language;
                XTLang = %trim(langReturn);

             Endif;

          Endif;


          Chain(n) (outFile) xp103;
          Chain (XPCO#) pco01;

          If XTLogo <> ' ';
             Chain (XPCO#:XPFND) pf200;
             If %found and XPFND <> 0 and F2SLogo <> ' ';
                XTLogo = F2SLogo;
             Else;
                XTLogo = COSLogo;
             Endif;
          Endif;

          If XTLogo <> ' ';
             XTLogoPth = SCURL8;
          Endif;

          XTOrder = xxOrder;

          XTPgm  = Q1PGM;
          XTUser = Q1USER;
          XTTime = Timeto6(SystemTime());
          XTDt   = Dateto7(SystemDate());

          Write xt101;

          Chain (outFile) xp103;
          If %found;
             XPAddT = 'Y';
             Update xp103;
          Endif;

      /end-free
     C                   Endsr                                                  TEMPLRQST Subr
     C**************************************************************************************
     C*
     C* BuildXMLStr - Build the XML document prefix string.
     C*
     C**************************************************************************************
     C     BuildXMLStr   Begsr
      /free

          Clear xmlString;
          //*** Begin Add *** 09/20/22 *************************************
          Clear chrFNPrefix;
          Clear chrFNData;
          needsUnderscore = *Off;

          // Access Scriptura Processes Setup File
          If qqReturn <> ' ';
             Chain (%Trim(qqReturn)) psp00;
             If %Found(stspl);
                chrFNPrefix = SPfnPrefix;
                chrFNData   = SPfnData;
             EndIf;
          EndIf;

          // Add underscore if not present
          If chrFNPrefix <> ' ';
             If %Len(%Trim(chrFNPrefix)) < 10;
                If %Scan('_':chrFNPrefix) = 0;
                   chrFNPrefix = %Trim(chrFNPrefix) + '_';
                EndIf;
             Else;
                needsUnderscore = *On;
             EndIf;
          EndIf;
          //*** End   Add *** 09/20/22 *************************************

          //* Build the file name based on incoming mode, or use generic document name.

          Select;
          When QQMode = 'E';
             Open WDELP;
             Chain xxRRN pel0p;
             Close WDELP;
             keyCo#  = ELCo#;
             keyFnd  = ELFnd;
             keyEmp# = ELEmp#;
             keyDiv  = ELDiv;
             keyFyr  = ELFyr;
             //*** Begin Chg *** 09/20/22 **********************************
             //***xmlString = 'Pol_' + %editc(keyFnd:'X') + %editc(keyEmp#:'X') +
             //*** %editc(keyFyr:'X');
             // Get Suffix Info
             Select;
             When chrFNData = 'Carrier Policy';
                RPcap# = ' ';
                Chain (keyCo#:keyFnd:keyEmp#:keyDiv:keyFyr) prp00;
             When chrFNData = 'DBA Name';
                chrEMName = ' ';
                Chain (keyCo#:keyFnd:keyEmp#:keyDiv) pem01;
                If %Found(wmeml);
                   chrEMName = %Subst(EMname:1:27);
                EndIf;
             EndSL;

             // Build the document name
             Select;
             When chrFNPrefix = ' ';
                Select;
                When chrFNData = ' ' Or chrFNData = 'File Info';
                   xmlString = 'Pol_' + %editc(keyFnd:'X') +
                    %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                When chrFNData = 'Carrier Policy';
                   If RPcap# = ' ';
                      xmlString = 'Pol_' + %editc(keyFnd:'X') +
                       %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                   Else;
                      xmlString = 'Pol_' + %Trim(RPcap#);
                   EndIf;
                When chrFNData = 'DBA Name';
                   If chrEMname = ' ';
                      xmlString = 'Pol_' + %editc(keyFnd:'X') +
                       %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                   Else;
                      xmlString = 'Pol_' + %Trim(chrEMName);
                   EndIf;
                EndSL;
             Other;
                Select;
                When chrFNData = ' ' Or chrFNData = 'File Info';
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                       %editc(keyFnd:'X') +
                       %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                       %editc(keyFnd:'X') +
                       %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                   EndIf;
                When chrFNData = 'Carrier Policy';
                   If RPcap# = ' ';
                      If needsUnderscore;
                         xmlString = %Trim(chrFNPrefix) + '_' +
                          %editc(keyFnd:'X') +
                          %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                      Else;
                         xmlString = %Trim(chrFNPrefix) +
                          %editc(keyFnd:'X') +
                          %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                      EndIf;
                   Else;
                      If needsUnderscore;
                         xmlString = %Trim(chrFNPrefix) + '_' + %Trim(RPcap#);
                      Else;
                         xmlString = %Trim(chrFNPrefix) + %Trim(RPcap#);
                      EndIf;
                   EndIf;
                When chrFNData = 'DBA Name';
                   If chrEMname = ' ';
                      If needsUnderscore;
                         xmlString = %Trim(chrFNPrefix) + '_' +
                          %editc(keyFnd:'X') +
                          %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                      Else;
                         xmlString = %Trim(chrFNPrefix) +
                          %editc(keyFnd:'X') +
                          %editc(keyEmp#:'X') + %editc(keyFyr:'X');
                      EndIf;
                   Else;
                      If needsUnderscore;
                         xmlString = %Trim(chrFNPrefix) + '_' +
                                     %Trim(chrEMName);
                      Else;
                         xmlString = %Trim(chrFNPrefix) +
                                     %Trim(chrEMName);
                      EndIf;
                   EndIf;
                EndSL;
             EndSL;
             //*** End   Chg *** 09/20/22 **********************************
             xxCat = 'PS';

          When QQMode = 'C';
             Open WMCMP;
             Chain xxRRN pcm0p;
             Close WMCMP;
             keyCo#  = CMCo#;
             keyFnd  = CMFnd;
             keyEmp# = CMEmp#;
             keyDiv  = CMDiv;
             keyFyr  = CMFyr;
             keyCase = CMCase;
             //*** Begin Chg *** 09/20/22 **********************************
             //***xmlString = 'Clm_' + %editc(keyFnd:'X') + %editc(keyFyr:'X') +
             //*** %editc(keyCase:'X');
             // Get Suffix Info
             If chrFNData <> ' ' And chrFNData <> 'Claim #';
                chrAccInfo = %EditC(dtYYMD(dtMDYY(CMacDt)):'X') +
                   '_' + %Trim(CMlsn) + '_' + %Trim(CMfsn);
             EndIf;

             // Build the document name
             Select;
             When chrFNPrefix = ' ';
                Select;
                When chrFNData = ' ' Or chrFNData = 'Claim #';
                   xmlString = 'Clm_' + %editc(keyFnd:'X') +
                    %editc(keyFyr:'X') + %editc(keyCase:'X');
                Other;
                   xmlString = 'Clm_' + %Subst(chrAccInfo:1:27);
                EndSL;
             Other;
                Select;
                When chrFNData = ' ' Or chrFNData = 'Claim #';
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                       %editc(keyFnd:'X') +
                       %editc(keyFyr:'X') + %editc(keyCase:'X');
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                       %editc(keyFnd:'X') +
                       %editc(keyFyr:'X') + %editc(keyCase:'X');
                   EndIf;
                Other;
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                                  %Subst(chrAccInfo:1:27);
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                                  %Subst(chrAccInfo:1:27);
                   EndIf;
                EndSL;
             EndSL;
             //*** End   Chg *** 09/20/22 **********************************
             xxCat = 'CS';

          When QQMode = 'M';
             Open FMCMP;
             Chain xxRRN mcm0p;
             Close FMCMP;
             keyCO#  = FMCO#;
             keySUB# = FMSUB#;
             keyFND  = FMFND;
             keyEMP# = FMEMP#;
             keyDIV  = FMDIV;
             keyFYR  = FMFYR;
             keyCASE = FMCASE;
             //*** Begin Chg *** 09/20/22 **********************************
             //***xmlString = 'Cas_' + %editc(keySub#:'X') + %editc(keyFnd:'X') +
             //*** %editc(keyFyr:'X') + %editc(keyCase:'X');
             // Get Suffix Info
             If chrFNData <> ' ' And chrFNData <> 'Claim #';
                chrAccInfo = %EditC(dtYYMD(dtMDYY(FMacDt)):'X') +
                   '_' + %Trim(FMlsn) + '_' + %Trim(FMfsn);
             EndIf;

             // Build the document name
             Select;
             When chrFNPrefix = ' ';
                Select;
                When chrFNData = ' ' Or chrFNData = 'Claim #';
                   xmlString = 'Cas_' + %editc(keySub#:'X') +
                    %editc(keyFnd:'X') + %editc(keyFyr:'X') +
                    %editc(keyCase:'X');
                Other;
                   xmlString = 'Cas_' + %Subst(chrAccInfo:1:27);
                EndSL;
             Other;
                Select;
                When chrFNData = ' ' Or chrFNData = 'Claim #';
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                       %editc(keySub#:'X') +
                       %editc(keyFnd:'X') + %editc(keyFyr:'X') +
                       %editc(keyCase:'X');
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                       %editc(keySub#:'X') +
                       %editc(keyFnd:'X') + %editc(keyFyr:'X') +
                       %editc(keyCase:'X');
                   EndIf;

                Other;
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                                  %Subst(chrAccInfo:1:27);
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                                  %Subst(chrAccInfo:1:27);
                   EndIf;
                EndSL;
             EndSL;
             //*** End   Chg *** 09/20/22 **********************************
             xxCat = 'MC';

          When QQMode = 'P';
             Open UMPCP;
             Chain xxRRN mpc0p;
             Close UMPCP;
             keyCO#  = PCCO#;
             keySUB# = PCSUB#;
             keyFND  = PCFND;
             keyFYR  = PCFYR;
             keyCASE = PCCASE;
             keyCERT = PCCERT;
             keyEXT  = PCEXT;
             //*** Begin Chg *** 09/20/22 **********************************
             //***xmlString = 'Pre_' + %trim(keyCert) + %editc(keyExt:'X');
             Select;
             When chrFNPrefix = ' ';
                xmlString = 'Pre_' + %trim(keyCert) + %editc(keyExt:'X');
             Other;
                If needsUnderscore;
                   xmlString = %Trim(chrFNPrefix) + '_' + %trim(keyCert) +
                     %editc(keyExt:'X');
                Else;
                   xmlString = %Trim(chrFNPrefix) + %trim(keyCert) +
                     %editc(keyExt:'X');
                EndIf;
             EndSL;
             //*** End   Chg *** 09/20/22 **********************************
             xxCat = 'PC';

          When QQMode = 'A';
             Open WMAGP;
             Chain xxRRN pag0p;
             Close WMAGP;
             keyCo#  = AGCo#;
             keyFein = AGFein;
             keyMod  = AGMod;
             //*** Begin Chg *** 09/20/22 **********************************
             //***xmlString = 'Agt_ + %editc(keyFein:'X') + %editc(keyMod:'X');

             Select;
             When chrFNPrefix = ' ';
                Select;
                When chrFNData = ' ' Or chrFNData = 'Fein/Mod';
                   xmlString = 'Agt_' + %editc(keyFein:'X') +
                    %editc(keyMod:'X');
                When chrFNData = 'Agency Name';
                   xmlString = 'Agt_' + %Trim(%Subst(AGname:1:27));
                EndSL;
             Other;
                Select;
                When chrFNData = ' ' Or chrFNData = 'Fein/Mod';
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                                  %editc(keyFein:'X') +
                                  %editc(keyMod:'X');
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                                  %editc(keyFein:'X') +
                                  %editc(keyMod:'X');
                   EndIf;
                When chrFNData = 'Agency Name';
                   If needsUnderscore;
                      xmlString = %Trim(chrFNPrefix) + '_' +
                                  %Trim(%Subst(AGname:1:27));
                   Else;
                      xmlString = %Trim(chrFNPrefix) +
                                  %Trim(%Subst(AGname:1:27));
                   EndIf;
                EndSL;
             EndSL;
             //*** End   Chg *** 09/20/22 **********************************
             xxCat = 'AS';

          Other;
             If QQString <> ' ';
                xmlString = %trim(QQString) + '_' + %trim(Q1User);
             Else;
                xmlString = 'Document_' + %trim(Q1User);
             Endif;
             xxCat = '  ';

          Endsl;

          //*** Begin Add *** 09/20/22 **********************************
          // Remove bothersome characters from document name
          chrXmlString = xmlString;
          chrXmlString = ClnFileName(chrXmlString);
          chrXmlString = %ScanRpl(',':'':chrXmlString);
          xmlString    = %Subst(chrXmlString:1:36);
          //*** End   Add *** 09/20/22 **********************************

      /end-free
     C                   Endsr                                                  BuildXMLStr Subr
     C**************************************************************************************
     C*
     C* MoveSig - Move the signature from permanent location to temporary for XML.
     C*
     C**************************************************************************************
     C     MoveSig       Begsr
      /free

          index = %scan('.':xxSig);
          If index <> 0;
             XPSig = %trim(%subst(xxSig:1:index-1)) + %trim(myStamp) +
              %trim(%subst(xxSig:index));
          Else;
             XPSig = %trim(xxSig) + %trim(myStamp);
          Endif;

          XPTempIFS = %trim(SCSigTemp);
          Sa030r(suffix);

          If suffix <> ' ' and %subst(xptempIFS:1:5) <> '/PDS/' and
           %subst(xptempIFS:1:5) <> '/pds/';
             XPTempIFS = '/PDS/' + suffix + %trim(SCSigTemp);
          Endif;
          XPTempWin = %trim(SCSigDir);

          Monitor;
             clear strCommand;
             strCommand = 'CPY OBJ(''' + %trim(xxFolder) +  %trim(xxSig) + ''')'
              + ' TOOBJ(''' + %trim(XPTempIFS) + %trim(XPSig) + ''')';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

      /end-free
     C                   Endsr                                                  MoveSig Subr
     C**************************************************************************************
     C*
     C* RunDataPgm - Call extra data program to perform other tasks.
     C*
     C**************************************************************************************
     C     RunDataPgm    Begsr
      /free

       //* Call program indicated in Scriptura module file for Phase 1/Department letters.
          If XPType = ' ' and SCKey1 <> ' ';

             Monitor;
                strPgm = SCKey1;
                ExtraDataPgm(XPXML);
             On-error;
             Endmon;

          Endif;

       //* Call program indicated in Scriptura Process file for Phase 2 processes.
          If XPType <> ' ';

             Chain(n) (XPType) psp00;
             If %found and SPDataP <> ' ';

                Monitor;
                   strPgm = SPDataP;
                   ExtraDataPgm(XPXML);
                On-error;
                Endmon;

             Endif;

          Endif;

      /end-free
     C                   Endsr                                                  RunDataPgm Subr
     C**************************************************************************************
     C*
     C* LANGUAGE - Convert language passed in to language code.
     C*
     C**************************************************************************************
     C     LANGUAGE      Begsr
      /free

          Clear langReturn;

          Select;
          When SCLang01 = inLanguage;
             langReturn = SCCode01;
          When SCLang02 = inLanguage;
             langReturn = SCCode02;
          When SCLang03 = inLanguage;
             langReturn = SCCode03;
          When SCLang04 = inLanguage;
             langReturn = SCCode04;
          When SCLang05 = inLanguage;
             langReturn = SCCode05;
          When SCLang06 = inLanguage;
             langReturn = SCCode06;
          When SCLang07 = inLanguage;
             langReturn = SCCode07;
          When SCLang08 = inLanguage;
             langReturn = SCCode08;
          When SCLang09 = inLanguage;
             langReturn = SCCode09;
          When SCLang10 = inLanguage;
             langReturn = SCCode10;
          Endsl;

          //* Default language to english if none are selected.
          If langReturn = ' ';
             langReturn = 'en';
          Endif;

      /end-free
     C                   Endsr                                                  LANGUAGE Subr
      //*** Begin Add *** 05/03/23 *****************************************
     C**************************************************************************************
     C*
     C* CVTLANGABB - Convert language code abbreviation to its fullname.
     C*
     C**************************************************************************************
     C     CVTLANGABB    Begsr
       Select;
       When XPlang = 'en' or XPlang = 'EN';
          dsRunInfo.docLang = 'English';
       When XPlang = 'es' or XPlang = 'ES';
          dsRunInfo.docLang = 'Spanish';
       When XPlang = 'fr' or XPlang = 'FR';
          dsRunInfo.docLang = 'French';
       When XPlang = 'de' or XPlang = 'DE';
          dsRunInfo.docLang = 'German';
       When XPlang = 'nl' or XPlang = 'NL';
          dsRunInfo.docLang = 'Dutch';
       When XPlang = 'ru' or XPlang = 'RU';
          dsRunInfo.docLang = 'Russian';
       When XPlang = 'ar' or XPlang = 'AR';
          dsRunInfo.docLang = 'Arabic';
       When XPlang = 'it' or XPlang = 'IT';
          dsRunInfo.docLang = 'Italian';
       When XPlang = 'el' or XPlang = 'EL';
          dsRunInfo.docLang = 'Greek';
       When XPlang = 'pl' or XPlang = 'PL';
          dsRunInfo.docLang = 'Polish';
       When XPlang = 'no' or XPlang = 'NO';
          dsRunInfo.docLang = 'Norweigan';
       When XPlang = 'ja' or XPlang = 'JA';
          dsRunInfo.docLang = 'Japanese';
       When XPlang = 'zh' or XPlang = 'ZH';
          dsRunInfo.docLang = 'Chinese';
       When XPlang = 'ko' or XPlang = 'KO';
          dsRunInfo.docLang = 'Korean';
       Other;
          dsRunInfo.docLang = XPlang;
       Endsl;
     C                   Endsr                                                  LANGUAGE Subr
      //*** End Add *** 05/03/23 *******************************************
     C**************************************************************************************
     C*
     C* WORKFILE - Return in QQXML if either scriptura or 3rd party xml is needed.
     C*
     C**************************************************************************************
     C     WORKFILE      Begsr
      /free

          Clear QQXML;

          Chain(n) (processType) psp00;
          If %found and SPStat = 'A';
             If SPScrO = 'Y' or SP3XML = 'Y';
                QQXML = 'Y';
             Endif;
          Endif;

      /end-free
     C                   Endsr                                                  WORKFILE Subr
     C**************************************************************************************
     C*
     C* Email - Generate an email for the request if indicated.
     C*
     C**************************************************************************************
     C     Email         Begsr
      /free

             Chain(n) Q#PNAM pkt01;
             If %found;
                Q#KTYP = KTKTYP;
             Else;
                Q#KTYP = *blanks;
             Endif;

          If XPMail = ' ' and XPMailAdd = ' ';
             Leavesr;
          Endif;

          //*** Begin Del *** 01/16/23 *****************************
          //*** strList = %trim(XPMail);
          //*** End   Del *** 01/16/23 *****************************


             Clear xxRRN;
             Clear strRRNType;
             Select;
             When XPCat = 'PS';
                Open WDELL;
                Chain (XPCO#:XPFND:XPEMP#:XPDIV:XPFYR) pel01;
                If %found (WDELL);
                   xxRRN = dbrrn1;
                   strRRNType = 'Policy';
                Endif;
                Close WDELL;
             When XPCat = 'CS';
                Open WMCML;
                Chain (XPCO#:XPFND:XPEMP#:XPDIV:XPFYR:XPCASE) pcm00;
                If %found (WMCML);
                   xxRRN = dbrrn2;
                   strRRNType = 'Claim';
                Endif;
                Close WMCML;
             When XPCat = 'AS';
                Open WMAGL;
                Chain (XPCO#:XPFEIN:XPMOD) pag00;
                If %found (WMAGL);
                   xxRRN = dbrrn3;
                   strRRNType = 'Agency';
                Endif;
                Close WMAGL;
             When XPCat = 'MC';
                Open FMCML1;
                Chain (XPCO#:XPSUB#:XPFND:XPFYR:XPCASE) mcm01;
                If %found (FMCML1);
                   xxRRN = dbrrn4;
                   strRRNType = 'Medical Case';
                Endif;
                Close FMCML1;
             When XPCat = 'PC';
                Open UMPCL11;
                Chain (XPCO#:XPSUB#:XPFND:XPFYR:XPCASE:XPCERT:XPEXT) mpc11;
                If %found (UMPCL11);
                   xxRRN = dbrrn5;
                   strRRNType = 'Pre Cert';
                Endif;
                Close UMPCL11;
             Endsl;

             If XPType <> ' ';
                strType = XPType;
             Else;
             //*** Begin Chg *** 05/03/23 *********************************
             //***   strType = XPDcid;
                If XPDcid <> ' ';
                   strType = XPDcid;
                Else;
                   strType = XPInid;
                Endif;
             //*** End Chg *** 05/03/23 *********************************
             Endif;

             //*** Begin Del *** 01/16/23 *********************************
             //*** Begin Chg *** 05/03/23 *********************************
             //*** Reset dsRunInfo;
             //*** dsRunInfo.runInfo = strType;
             //*** dsRunInfo.docCat = XPcat;
             //*** dsRunInfo.docSub = XPsubC;
             //*** dsRunInfo.docLang = XPlang;
             Reset dsRunInfo;
             dsRunInfo.runInfo = strType;
             dsRunInfo.docCat = XPcat;
             dsRunInfo.docSub = XPsubC;
             //*** Begin Add *** 05/03/23 *********************************
             // Convert abreviated language code to fullname.
             Exsr CVTLANGABB;
             //*** Begin Add *** 05/03/23 *********************************
             //*** End Chg *** 05/03/23 *********************************

             //* Determine recipients from API if indicated.
             //*** If strList = '%mass%';
             //***   strList = %trim(GetDefaultEmailRecipients(dsRunInfo:xxRRN:
             //***    strRRNType:strInsuredEmail:strAgentEmail));
             //***Endif;
             //*** End   Del *** 01/16/23 *********************************

             //*** Begin Chg *** 01/16/23 *********************************
             //*** strEmail = %trim(strList);
             If XPmail = '%mass%';
                strEmail   = %Trim(XPinsMailA);
                chrFeinMod = %EditC(XPfein:'X') + %EditC(XPmod:'X');
                Exec Sql
                   Select threshold
                   Into :chrThreshold
                   From qtemp/AgentInfoTbl
                   Where feinMod = :chrFeinMod;
                // Include Agency Email List
                If chrThreshold <> 'Y';
                   If strEmail = ' ';
                      strEmail = %Trim(XPagtMailA);
                   Else;
                      strEmail = %Trim(strEmail) + ',' +
                                 %Trim(XPagtMailA);
                   EndIf;
                EndIf;
             Else;
                strEmail = %Trim(XPmail);
             EndIf;
             //*** End   Chg *** 01/16/23 *********************************

             //* Add in additional recipients from prompt screen.
             If XPMailAdd <> ' ';
                If strEmail = ' ';
                   strEmail = %trim(XPMailAdd);
                Else;
                   strEmail = %trim(strEmail) + ', ' + %trim(XPMailAdd);
                Endif;
             Endif;

             //* Do not attempt to send email if no recipients.
          //*** begin del *** 01/13/23 ***********************************
          //*****   If strEmail = ' ';
          //*****      Leavesr;
          //*****   Endif;
          //*** end del ***** 01/13/23 ***********************************
          //*** begin add ***  12/09/24  *********************************
          //* Escape aprostrophe marks for the email command.
             strEmail = %scanrpl('''':'''''':strEmail);
          //*** end add *****  12/09/24  *********************************

             //* Change semi-colons to commas to allow next scan/replace to work
             strEmail = %scanrpl(';':',':strEmail);

             // Format recipients for XML build
             strEmail = '''' + %trim(%scanrpl(',':''' ''':strEmail)) + '''';

             //* Replace tokens with data.
             strSubject = ReplaceTokensEmail(XPSubj:dsRunInfo:xxRRN:
              strRRNType);
             //* Replace tokens with data.
             strBody = ReplaceTokensEmail(XPBody:dsRunInfo:xxRRN:
              strRRNType);

          //*** begin add *** 01/13/23 ***********************************
          Clear strCmd;

             //* Do not attempt to send email if no recipients.
          If strEmail <> ' ' And %Scan('@':strEmail) <> 0;
          //*** end add ***** 01/13/23 ***********************************
             //*String fields for email data.
             strEmail = %scanrpl(',':' ':strEmail);

             //*Email data.

            If XPType <> ' ';
               strDocName = XPType;
            Else;
               strDocName = 'Document';
            Endif;

          //* Escape aprostrophe marks for the email command.
             strSubject = %scanrpl('''':'''''':strSubject);
             strBody = %scanrpl('''':'''''':strBody);

          //* Build the SNDEMAIL command to send the email(s).
            If XPUsrMail <> ' ';
          //*** begin chg ***  12/09/24  *********************************
          //*****     strCmd = 'SNDEMAIL FROM(''' + %Trim(XPUsrMail) + ''') ';
          //* Escape aprostrophe marks for the email command.
               strCmd = 'SNDEMAIL FROM(''' +
                %Trim(%scanrpl('''':'''''':XPUsrMail)) + ''') ';
          //*** end chg *****  12/09/24  *********************************
            Else;
               strCmd = 'SNDEMAIL FROM(*NONE) ';
            Endif;

            strCmd = %Trim(strCmd) +
             ' TO(' + %Trim(strEmail) + ') ' +
             'SUBJECT(''' + %Trim(strSubject) + ''') ' +
             'BODY(''' + %Trim(strBody) + ''') ' +
             'SUFFIX(' + suffix + ') ' +
             'RETRY(' + %char(2) +') ';

            //*** Begin Add *** 01/16/23 ***********************************
            XPfile = %scanrpl('''':'''''':XPfile);
            //*** Beg   Del *** 08/03/23 ***********************************
            //*** XPfile = '"' + %Trim(XPfile) + '"';
            //*** End   Del *** 08/03/23 ***********************************
            //*** End   Add *** 01/16/23 ***********************************
            //*** Beg   Chg *** 08/03/23 *** WC2670 ************************
           // strCmd = %Trim(strCmd) +
           //  ' ATTACH((''' + %trim(strDocName) + ''' ''' +
           //  %Trim(XPDir) + %trim(XPFile) + '''))';
            //*** Beg Add *** 01/16/24 *** 153WAD ************************
              If %Len(%Trim(strDocName)) > 30;
                 strDocName = %Subst(strDocName : 1 : 30);
              EndIf;
            //*** End Add *** 01/16/24 *** 153WAD ************************
            strCmd = %Trim(strCmd) +
             ' ATTACH((''' + %trim(strDocName) + ''' ''' +
             %Trim(XPDir) + %trim(XPFile) + ''')';

            SetLL (XPDir:XPFile) Xp105;
            ReadE (XPDir:XPFile) Xp105;
            Dow Not %Eof(xmlprel5);
               If X5Type <> ' ';
                  strDocName = X5Type;
               Else;
                  strDocName = 'Document';
               Endif;

               strCmd = %Trim(strCmd) +
                '(''' + %trim(strDocName) + ''' ''' +
                %Trim(X5Dir) + %trim(X5File) + ''')';

               ReadE (XPDir:XPFile) Xp105;
            EndDo;
            strCmd = %Trim(strCmd) + ')';
            //*** End   Chg *** 08/03/23 *** WC2670 ************************

            If XPDart = 'Y' and XPRmcd <> ' ';
               strCmd = %Trim(strCmd) +
                ' DARTS(''' + %trim(Q#KEY) + ''' ' +
                %trim(Q#KTYP) + ' ' + %trim(Q#RMCD) +  ' ' +
                %trim(Q1User) + ')';
            Endif;

          //* Execute the SNDEMAIL command to send the email(s).
            RunCmd(strCmd:%Len(%Trim(strCmd)));
          //*** begin add *** 01/13/23 ***********************************
            //* Record email details in log file.
            logType     = 'E';
            emailType   = 'E';
            getPrepRcd  = *Off;
            Exsr ProcessLog;
            If XPinsMailA <> ' ';
               decInsDocE += 1;
            EndIf;
          Endif;

          //*** end add ***** 01/13/23 ***********************************

      /end-free
     C                   Endsr                                                  Email Subr
       //===================================================================
       // Record email/ftp/print details in log file.
       //===================================================================
       Begsr ProcessLog;

          If getPrepRcd;
             chrExtractedDocID = %ScanRpl('''''':'''':chrExtractedDocID);
             chrExtractedDocID = %ScanRpl('"':'':chrExtractedDocID);
             Exec Sql
                Select *
                Into :dsXMLPREP
                From XMLPREP
                Where Trim(XPdir) || Trim(XPfile) =
                      :chrExtractedDocID;
          EndIf;

          Clear sem0p;

          EMCo#  = XPCo#;
          EMFnd  = XPFnd;
          EMSub# = XPSub#;
          EMEmp# = XPEmp#;
          EMDiv  = XPDiv;
          EMFyr  = XPFyr;
          EMCase = XPCase;
          EMCert = XPCert;
          EMExt  = XPExt;
          EMFein = XPFein;
          EMMod  = XPMod;
          EMEob# = XPEob#;

          EMDir  = XPDir;
          EMFile = XPFile;

          EMCat  = XPCat;
          EMSubC = XPSubc;
          EMType = XPType;

          If XPinsMail <> ' ';
             EMInsMail = XPinsMail;
          Else;
             EMInsMail  = 'N';
          EndIf;

          If XPagtMail <> ' ';
             EMAgtMail  = XPagtMail;
          Else;
             EMAgtMail  = 'N';
          EndIf;

          If XPagtMailA <> ' ';
             EMagtEmail = XPagtMailA;
          Else;
             EMagtEmail = 'none';
          EndIf;

          If XPinsMailA <> ' ';
             EMinsEmail = XPinsMailA;
          Else;
             EMinsEmail = 'none';
          EndIf;

          EMSubj     = %subst(XPsubj:1:250);
          EMBody     = %subst(XPbody:1:4000);

          Select;
          When logType = 'C'; // Command Information
             Select;
             When chrCmdType = 'MKDIR';
                EMcmd      = %subst(strCmd:1:1000);
                EMfromUncn = %Trim(chrFromDirFtp) + %Trim(chrFromDocID);
                EMtoUncn   = %Trim(chrFtpFailPath);
                EMresults  = 'Failed to make directory';
             When chrCmdType = 'CPY';
                EMcmd      = %subst(strCmd:1:1000);
                EMfromUncn = %Trim(chrFromDirFtp) + %Trim(chrFromDocID);
                EMtoUncn   = %Trim(chrFtpFailPath) + %Trim(chrFromDocID);
                EMresults  = 'Failed to copy document';
             When chrCmdType = 'No Directory';
                EMcmd      = ' ';
                EMfromUncn = ' ';
                EMtoUncn   = ' ';
                EMresults  = 'No ftp failure directory';
             Other;
                EMcmd      = %subst(strCmd:1:1000);
                EMfromUncn = %Trim(chrFromDirFtp) + %Trim(chrFromDocID);
                EMtoUncn   = %Trim(chrFtpFailPath) + %Trim(chrFromDocID);
                EMresults  = 'Document copied to ftp failure directory';
             EndSl;
          When logType = 'E'; // Email Information
             EMMailRec  = %subst(strEmail:1:750);
             EMCmd      = %subst(strCmd:1:6000);
             Select;
             When emailType = 'E';
                EMresults  = 'Document has been emailed.';
             When emailType = 'F';
                EMresults  = 'Ftp status has been emailed.';
             EndSl;
          When logType = 'F'; // Ftp Information
             EMCmd      = %subst(ftpCommand2:1:1000);
             EMfromUncn = %Trim(chrFromDirFtp) + %Trim(chrFromDocID);
             EMtoUncn   = %Trim(chrToDir) + %Trim(chrFromDocID);
             If errorFound;
                EMresults  = 'Document has failed to upload to ftp.';
             Else;
                EMresults  = 'Document has been ftp''d.';
             EndIf;
          When logType = 'P'; // Print Information
             EMCmd      = %subst(strCmd:1:6000);
             EMfromUncn = strStream;
             EMresults  = 'Document has been printed.';

          When logType = 'M'; // Merge Information
             EMCmd     = %subst(strCommand:1:1000);
             Select;
             When mergeType = 'A';
                EMfromUncn = chrExtractedDocID;
                EMtoUncn   = chrAgtInfoPath;
                EMresults  = 'Document has been merged into agent file.';
             When mergeType = 'I';
                EMFromUncn = chrExtractedDocID;
                EMtoUncn   = chrInsInfoPath;
                EMresults  = 'Document has been merged into insured file.';
             EndSl;

          EndSl;

          EMjob#    = Q1jobN;
          EMjobUser = Q1cUsr;
          EMjobName = Q1job;

          If errorFound;
             errorFound = *Off;
             EMmsgID    = Q1extP + Q1exNm;
             EMmsgTxt   = Q1red;
          EndIf;

          EMPgm  = Q1PGM;
          EMUser = Q1USER;
          EMTime = Timeto6(SystemTime());
          EMDt   = Dateto7(SystemDate());

          Write sem0p;

       Endsr;
     C*** end add ***** 01/13/23 ***********************************
     C*** End   Add *** 09/20/22 *******************************************
     C**************************************************************************************
     C*
     C* EmailQueue - Create data queue for email if needed.
     C*
     C**************************************************************************************
     C     EmailQueue    Begsr
      /free

          Sa030r(suffix);

          strLib = 'PDSTOOLS';
          strQueue = 'MAILSND' + suffix;
          Select;
          When suffix <> *blank;
             %subst(strLib:8:3) = suffix;
          Endsl;

          If SCKey2 = ' ';
             CheckObjInLIBL(strQueue:'*DTAQ':notAvail);
             If notAvail = '1';
                CreateDataQ(strQueue:strLib:64512:'Email Data Queue':inRC);
             Endif;
             Setll 1 psc0p;
             Read psc0p;
             SCKey2 = strQueue;
             Update psc0p;
          Endif;

      /end-free
     C                   Endsr                                                  EmailQueue Subr
     C**************************************************************************************
     C*
     C* Print - Send document to an outqueue for printing.
     C*
     C**************************************************************************************
     C     Print         Begsr
     C                   TIME                    DSDTTM
      /free

          strStream = %trim(XPDir) + %trim(XPFile);

          If XPHold = 'Y';
             strHold = '*YES';
          Else;
             strHold = '*NO ';
          Endif;

          If XPSave = 'Y';
             strSave = '*YES';
          Else;
             strSave = '*NO ';
          Endif;

          If XPCopy <= 0;
             XPCopy = 1;
          Endif;

          Monitor;
             Dow XPCopy <> 0;
                PrtStmF(strStream:XPOutq:XPForm:strHold:strSave:1);
                XPCopy = XPCopy - 1;
             Enddo;
          On-error;
             message = 'Print failure:  OutQ:  ' + %trim(XPOutq) + ' Form Ty' +
              'pe: ' + %trim(XPForm) + ' Hold: ' + %trim(strHold) + ' Save: ' +
              %trim(strSave) + '  Please see Job Log.';
             Open Qsysprt;
             Except Head;
             Close  Qsysprt;
          Endmon;

      /end-free
     C                   Endsr                                                  Print Subr

     C**************************************************************************************
     C*
     C* CopyDoc - Copy the iDarts document to the user folder.
     C*
     C**************************************************************************************
     C     CopyDoc       Begsr
      /free

          Monitor;
             clear strCommand;
             strCommand = 'CPY OBJ(''' + %trim(XPDir) +  %trim(XPFile) +
              ''')' + ' TOOBJ(''' + %trim(XPOutFld) + %trim(XPFile) + ''')';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;


      /end-free
     C                   Endsr                                                  Print Subr

     C**************************************************************************************
     C*
     C* Timeout - Determine the # of seconds for java/scriptura timeout.
     C*
     C**************************************************************************************
     C     Timeout       Begsr
      /free

       timeoutVal = 117;
       If SC1103 > 0;
          timeoutVal = SC1103;
       Endif;
       //*** begin add ***  02/23/24  *****************************************
       If XPXmlT <> ' ' and SCXMLF <> ' ';
          // Get the docment size
          chrPath    = %Trim(SCXMLF) + %Trim(XPXmlT);
          intLnkSize = 0;
          chrRtnCode = ' ';
          RtvLnkSize(chrPath:intLnkSize:chrRtnCode);
          If intLnkSize <> 0;
             timeoutVal = timeoutVal + 3 + intLnkSize / 15000;
          Endif;
       Endif;
       //*** end add *****  02/23/24  *****************************************

       If XPAddT = 'Y';
          Setll XPFile xt100;
          Reade(n) XPFile xt100;
          Dow not %eof;
          //*** begin chg ***  02/23/24  *****************************************
          //*****   timeoutVal = timeoutVal + 3;
             If XTXmlT <> ' ' and SCXMLF <> ' ';
                // Get the docment size
                chrPath    = %Trim(SCXMLF) + %Trim(XTXmlT);
                intLnkSize = 0;
                chrRtnCode = ' ';
                RtvLnkSize(chrPath:intLnkSize:chrRtnCode);
                If intLnkSize <> 0;
                   timeoutVal = timeoutVal + 3 + intLnkSize / 15000;
                Endif;
             Endif;
          //*** end chg *****  02/23/24  *****************************************
             Reade(n) XPFile xt100;
          Enddo;
       Endif;

       If XPLink <> ' ';
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade(n) key1XML xp100;

          Dow not %eof;
          //*** begin chg ***  02/23/24  *****************************************
          //*****   timeoutVal = timeoutVal + 3;
             If XPXmlT <> ' ' and SCXMLF <> ' ';
                // Get the docment size
                chrPath    = %Trim(SCXMLF) + %Trim(XPXmlT);
                intLnkSize = 0;
                chrRtnCode = ' ';
                RtvLnkSize(chrPath:intLnkSize:chrRtnCode);
                If intLnkSize <> 0;
                   timeoutVal = timeoutVal + 3 + intLnkSize / 15000;
                Endif;
             Endif;
          //*** end chg *****  02/23/24  *****************************************

             If XPAddT = 'Y';
                Setll XPFile xt100;
                Reade(n) XPFile xt100;
                Dow not %eof;
                //*** begin chg ***  02/23/24  *****************************************
                //*****   timeoutVal = timeoutVal + 3;
                   If XTXmlT <> ' ' and SCXMLF <> ' ';
                      // Get the docment size
                      chrPath    = %Trim(SCXMLF) + %Trim(XTXmlT);
                      intLnkSize = 0;
                      chrRtnCode = ' ';
                      RtvLnkSize(chrPath:intLnkSize:chrRtnCode);
                      If intLnkSize <> 0;
                         timeoutVal = timeoutVal + 3 + intLnkSize / 15000;
                      Endif;
                   Endif;
                //*** end chg *****  02/23/24  *****************************************
                   Reade(n) XPFile xt100;
                Enddo;
             Endif;

             Reade(n) key1XML xp100;
          Enddo;

       Endif;

       Clear QQReturn;
       QQReturn = %editc(timeoutVal:'X');

      /end-free
     C                   Endsr                                                  Timeout Subr
     OQSYSPRT   E            HEAD           2 06
     O                                           40 'Documents Processing'
     O                                           70 'Run Date:'
     O                       DSDATE        Y     80
     O*** begin add ***  08/15/23  ****************************************
     O          E            HEAD           2
     O                                           35 'Flow File Name:'
     O                       XPXml               96
     O*** end add *****  08/15/23  ****************************************
     O          E            HEAD           1
     O                                           20 'Co#:'
     O                       keyCo#        4     24
     O                                           32 'Fund#:'
     O                       keyFnd        4     36
     O                                           43 'Fyr#:'
     O                       keyFyr        4     47
     O                                           53 'Emp#:'
     O                       keyEmp#       4     63
     O                                           70 'Div#:'
     O                       keyDiv        4     76
     O                                           85 'Case:'
     O                       keyCase       4     93
     O          E            HEAD           2
     O                                           20 'Sub#:'
     O                       keySub#       4     24
     O                                           34 'Cert#:'
     O                       keyCert             50
     O          E            HEAD           1
     O                       message            130
     O          E            HEAD2          1
     O                       message2           130
     O          E            HEAD2          1
     O                       message3           130
     O*** begin add ***  08/15/23  ****************************************
     O          E            HEAD2       1  2
     O                                           34 'Java or Scriptura Error:'
     O                       errorType           45
     O*** end add *****  08/15/23  ****************************************
     O*** Begin Add *** 03/30/23 *******************************************
     OQSYSPRT7  E            PAGEBRK        1 06
     O          E            HEAD1          1
     O                                           20 'Documents Processing'
     O                                           40 '-Mass Output Distrib'
     O                                           60 'ution               '
     O                       companyHead        110
     O                                          120 'Run Date:'
     O                       DSDATE        Y    132
     O          E            HEAD1          1
     O                                           20 'XML Flow File Name: '
     O                       chrFlowXml          80
     O          E            HEAD1          1
     O                                           17 'Type of Process: '
     O                       chrType             67
     O                                           85 'Job Name: '
     O                       chrJobName          95
     O                                          107 'Job User: '
     O                       chrJobUser         117
     O                                          126 'Job #: '
     O                       chrJob#            132
     O          E            HEAD1          1
     O                                           18 'Mass Output File: '
     O                       chrMassPath         78
     O                                           98 'All Printer'
     O                                          115 'Insured Printer'
     O                                          132 'Agency Printer'
     O          E            HEAD1          1
     O                                           19 'Insured Copy File: '
     O                       chrInsuredPath      79
     O                       chrAllPrinter       98
     O                       chrInsPrinter      113
     O                       chrAgtPrinter      130
     O          E            HEAD1          1
     O          E            detail1        1
     O                                            5 'Group'
     O                                           16 ' Employer'
     O                                           26 'Division'
     O                                           32 'Year'
     O                                           41 '   Case'
     O                                           66 'Employer/Claimant Name'
     O                                           89 'Insured Emailed'
     O                                          109 'Agency Emailed/FTP'
     O          E            detail1        1
     O                       decGroup#     4      3
     O                       decEmp#       4     16
     O                       decDiv        4     25
     O                       decYear       4     32
     O                       decCase       4     41
     O                       chrName             74
     O                       chrInsMail          83
     O                       chrAgtMail         110
     O          E            detail1        1
     O                                           22 'Agency Name(fein/mod):'
     O                       chrAgtName          83
     O          E            detail1        1
     O                                            8 'Results:'
     O                       chrResults         132
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O**********                                 15 'Document Name: '
     O          E            detail2        1
     O                       chrDocTitle         15
     O*** End   Chg *** 04/23/24 *******************************************
     O                       chrDocPath         130
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O          E            detail3        1
     O*** End   Chg *** 04/23/24 *******************************************
     O                                           17 'Inusured Emails: '
     O                       chrInsEmail        130
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O          E            detail3        1
     O*** End   Chg *** 04/23/24 *******************************************
     O                                           15 'Agency Emails: '
     O                       chrAgtEmail        130
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O          E            detail3        1
     O*** End   Chg *** 04/23/24 *******************************************
     O                                           17 'Agency FTP Site: '
     O                       chrAgtFtp          130
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O          E            detail3        1
     O*** End   Chg *** 04/23/24 *******************************************
     O                                           18 'Agency Copy File: '
     O                       chrAgtPath         130
     O*** Begin Chg *** 04/23/24 *******************************************
     O**********E            detail1        1
     O          E            detail3        1
     O*** End   Chg *** 04/23/24 *******************************************
     O          E            total1         1
     O                                           20 'Total Documents Emai'
     O                                           36 'led to Insured: '
     O                       decInsDocE          41 '   0 '
     O                                           65 'Total Documents in I'
     O                                           78 'nsured Copy: '
     O                       decInsDocC          81 '   0 '
     O          E            total1         1
     O                                           20 'Total Documents Emai'
     O                                           37 'led to Agencies: '
     O                       decAgtDocE          42 '   0 '
     O                                           65 'Total Documents in A'
     O                                           77 'gency Copy: '
     O                       decAgtDocC          82 '   0 '
     O                                          107 'Total # Of Agencies '
     O                                          114 'FTP''d: '
     O                       decAgtTotalFtp     119 '   0 '
     O          E            total1         1
     O                                           20 'Total Documents Prod'
     O                                           26 'uced: '
     O                       decDocTotals        31 '   0 '
     O*** End   Add *** 03/30/23 *******************************************
     P*** Begin Add *** 09/20/22 *******************************************
     P*=====================================================================
     P* BuildAgentFtpEmailList - Builds the agent ftp email list
     P*=====================================================================
     PBuildAgentFtpEmailList...
     P                 B                   Export
     dBuildAgentFtpEmailList...
     D                 PI          1000a
     d inEmailList1                1000a
     d inEmailList2                1000a

     d chrMergedEmailList...
     d                 s           1000a
     d chrExtractedEmail...
     d                 s             60a
     d emailLocation   s              5s 0

      /free

       If inEmailList2 = ' ';
          chrMergedEmailList = inEmailList1;
       Else;
          // Extract emails and see if in existing list
          chrMergedEmailList = inEmailList2;
          str = 1;
          DoU pos = 0;
             pos = %Scan(',':inEmailList1:str);
             If pos = 0;
                // Extract Email
                chrExtractedEmail = %Subst(inEmailList1:str);
                // Does this email exist in the ftp email list
                emailLocation =
                   %Scan(%trim(chrExtractedEmail):inEmailList2:1);
                If emailLocation = 0;
                   If chrMergedEmailList = ' ';
                      chrMergedEmailList = chrExtractedEmail;
                   Else;
                      chrMergedEmailList =
                         %Trim(chrMergedEmailList) + ',' +
                         %Trim(chrExtractedEmail);
                   EndIf;
                EndIf;
                Leave;
             Else;
                end = pos;
                len = end - str;
                chrExtractedEmail = %Subst(inEmailList1:str:len);
                str = end + 1;
                // Does this email exist in the ftp email list
                emailLocation =
                   %Scan(%trim(chrExtractedEmail):inEmailList2:1);
                If emailLocation = 0;
                   If chrMergedEmailList = ' ';
                      chrMergedEmailList = chrExtractedEmail;
                   Else;
                      chrMergedEmailList =
                         %Trim(chrMergedEmailList) + ',' +
                         %Trim(chrExtractedEmail);
                   EndIf;
                EndIf;
             EndIf;
          EndDo;
       EndIf;

       Return chrMergedEmailList;

      /end-free

     PBuildAgentFtpEmailList...
     P                 E
     P*=====================================================================
     P* CreateAgtInfoTbl - Create Agent Information Table
     P*=====================================================================
     PCreateAgtInfoTable...
     P                 B                   Export
     dCreateAgtInfoTable...
     D                 PI

      /free

       // Create Agent Information Table
       Exec SQL Create Table qtemp/AgentInfoTbl
         (feinMod      Char( 12 ),
          docCount     Num ( 11, 0),
          docSize      Num ( 10, 0),
          threshold    Char ( 1 ),
          docList      Clob ( 1000000 ),
          docMail      Char ( 10000 ),
          hasMail      Char ( 1 ),
          docMailAgt   Char ( 1000 ));

       // Clear Agent Information Table
       Exec SQL Delete From qtemp/AgentInfoTbl;

      /end-free

     PCreateAgtInfoTable...
     P                 E
     P*=====================================================================
     P* FtpDocuments - Ftp Documents
     P*  inPath      - Document That Needs to Be Merged
     P*  Returns     - Nothing
     P*=====================================================================
     PFtpDocuments     B                   Export
     DFtpDocuments     PI
     d inPath                       256

     d beg             s             11s 0
     d end             s             11s 0
     d len             s             11s 0
     d pos             s             11s 0
     d str             s             11s 0

      /free

       // Get FTP Information
       inTypeKey   = %EditC(AGfein:'X') + %EditC(AGmod:'X');
       outData1    = ' ';
       outData2    = ' ';
       StRtvDta(AGco#:inType:inTypeKey:inNumber1:outData1:
                inNumber2:outData2);
       chrUserID   = outData1;
       chrPassword = outData2;
       chrToSystem = AGEFTP;
       foundSlash  = *Off;

       // Extract the document path and document ID
       str = 1;
       DoU foundSlash;
          pos = %Scan('/':inPath:str);
          If pos = 0;
             chrFromDirFtp = %Subst(inPath:1:str-1);
             chrFromDocID  = %Subst(inPath:str);
             foundSlash    = *On;
          Else;
             str = pos + 1;
          EndIf;
       EndDo;

       chrFromDocID = %scanrpl('''':'''''':chrFromDocID);

       If AGeFtpD = ' ';
          chrToDir = '/ftp/';
       Else;
          chrToDir = AGeFtpD;
       EndIf;

       strToPath   = inPath;

       // FTP Here
       ftpCommand2 = 'SNDFTPD ' +
                     'USERID('     + %Trim(chrUserID)       + ') ' +
                     'PASSWORD(''' + '************'         + ''') ' +
                     'FROMDIR('''  + %Trim(chrFromDirFtp)   + ''') ' +
                     'FROMDOC('''  + %Trim(chrFromDocID)    + ''') ' +
                     'TOSYSTEM(''' + %Trim(chrToSystem)     + ''') ' +
                     'TODIR('''    + %Trim(chrToDir)        + ''')';
       ftpCommand  = 'SNDFTPD ' +
                     'USERID('     + %Trim(chrUserID)       + ') ' +
                     'PASSWORD(''' + %Trim(chrPassword)     + ''') ' +
                     'FROMDIR('''  + %Trim(chrFromDirFtp)   + ''') ' +
                     'FROMDOC('''  + %Trim(chrFromDocID)    + ''') ' +
                     'TOSYSTEM(''' + %Trim(chrToSystem)     + ''') ' +
                     'TODIR('''    + %Trim(chrToDir)        + ''')';
       Monitor;
          QCMDEXC(ftpCommand:%len(%Trim(ftpCommand)));
       On-Error;
          // FTP Error
          ftpFailed  = *On;
          errorFound = *On;
       EndMon;

       // Build Body of email
       If isFirstDoc;
          isFirstDoc = *Off;
          chrDocList2 = inPath;
       Else;
          chrDocList2 = %Trim(chrDocList2) + crlf + %Trim(inPath);
       EndIf;

       // Something was Ftp'd
       documentFtpD = *On;

      /end-free

     PFtpDocuments     E
     P*** Begin Add *** 04/23/24 *******************************************
     P*=====================================================================
     P* PrintDocPath - Read thru all documents for a policy
     P*=====================================================================
     PPrintDocPath     B                   Export
     DPrintDocPath     PI
     d inFnd                               Like(EMfnd)
     d inEmp#                              Like(EMemp#)
     d inDiv                               Like(EMdiv)
     d inFyr                               Like(EMfyr)

     D isFirst         s               n
     D strSql          s           1000    inz(' ')

      /free

       // Read thru Log File
       strSql  = 'Select EMdir, EMfile from sxemp where EMjob# = ' +
                 %EditC(Q1jobn:'X') + ' And EMjobUser = ''' +
                 %Trim(Q1cUsr) + ''' And EMjobName = ''' +
                 %Trim(Q1job) + ''' And EMresults <> ''' +
                 'Ftp status has been emailed.'' And EMfnd=' +
                 %EditC(inFnd:'X') + ' And EMemp#=' + %EditC(inEmp#:'X') +
                 ' And EMdiv=' + %EditC(inDiv:'X') +
                 ' And EMfyr=' + %EditC(infyr:'X') + ' ' +
                 'Group by EMdir, EMfile ' +
                 'Order by EMdir, EMfile';

       isFirst = *On;
       sqlStt  ='00000';

       Exec SQL close cursorSXEMP2;
       Exec Sql Prepare sqlSXEMP2 From :strSql;
       Exec Sql Declare cursorSXEMP2 Cursor For sqlSXEMP2;
       Exec Sql Open cursorSXEMP2;

       Dou Not IsValidFetch(sqlStt);
          Exec SQL fetch next from cursorSXEMP2 into :EMdir, :EMfile;
          If IsValidFetch(sqlStt);

             If isFirst;
                chrDocTitle = 'Document Name: ';
                isFirst     = *Off;
             Else;
                chrDocTitle = ' ';
             EndIf;

             chrDocPath = %Trim(EMdir) + %Trim(EMfile);
             chrDocPath = %ScanRpl('"':'':chrDocPath);
             chrDocPath = %ScanRpl('''''':'''':chrDocPath);

             Except detail2;

          Else;
              sqlStt ='00000';
              Leave;
          Endif;
       Enddo;

       Exec SQL close cursorSXEMP2;

      /end-free

     PPrintDocPath     E
     P*** End   Add *** 04/23/24 *******************************************
     P*=====================================================================
     P* MergeDocuments - Merge Documents into Master Merge Document
     P*  mergeDocID - Name of Merge Document
     P*  documentID - Document That Needs to Be Merged
     P*  Returns    - Nothing
     P*=====================================================================
     PMergeDocuments   B                   Export
     DMergeDocuments   PI
     D mergeDocID                   256a
     d documentID                   256a
     d*** begin add ***  01/09/25  *******************************
     d numDocs                       11s 0
     d*** end add *****  01/09/25  *******************************


      /free

       // If this is the first document then there isn't any merge
       // document yet so the first document has to be copied and
       // named as the merge document
       documentID = %scanrpl('''':'''''':documentID);
       If isFirst = *On;

          // Fix document id
          documentID = '"' + %Trim(documentID) + '"';

          strCommand = 'CPY OBJ(' +
                    '''' + %Trim(documentID) + ''') ' +
                    'TOOBJ(' +
                    '''' + %Trim(mergeDocID) + ''')';
          Monitor;
             QCmdExc(%Trim(strCommand):%Len(%Trim(strCommand)));
          On-Error;
             errorFound = *On;
             Return;
          EndMon;
      //*** begin del ***  01/09/25  *******************************
      //*****    isfirst = *Off;
      //*****    Return;
      //*** end del *****  01/09/25  *******************************
       Endif;

      //*** begin add ***  01/09/25  *******************************
      //* Create new PDF merger object if first time in, or a merge just occurred prior.
       If numDocs = 0 or isFirst = *on;

      //* Create a PDF merger object.
          pdfMergerObj = PDFMCreateObj();

      //* Set the desintation file.
          javaString = NewString(%trim(mergeDocID));
          PDFMSetDestin(pdfMergerObj:javaString);

      //* Add same PDF as 1st document to be merged in.
          javaString = newString(%trim(mergeDocID));
          Monitor;
             PDFMAddSource(pdfMergerObj:javaString);
          On-error;
             Clear strCommand;
             strCommand = 'SNDPGMMSG MSG(''Document ' + %trim(mergeDocID) +
              ' not found for merge command.'')';
             Callp QCMDEXC(strCommand:%len(strCommand));
          EndMon;
          numDocs += 1;
          If isFirst = *on;
             isFirst = *off;
             Return;
          Endif;
       Endif;

      //* Add PDF to be merged in.  Catch error if method throws exception.
       javaString = newString(%trim(documentID));
       Monitor;
          PDFMAddSource(pdfMergerObj:javaString);
       On-error;
          Clear strCommand;
          strCommand = 'SNDPGMMSG MSG(''Document ' + %trim(documentID) +
           ' not found for merge command.'')';
          Callp QCMDEXC(strCommand:%len(strCommand));
       EndMon;

       numDocs += 1;

      //* If the maximum has not been reached then return and do not run
      //* merge method yet.
       If numDocs < maxDocs;
          Return;
       Endif;

      //* Merge the documents together into destination file.
       Monitor;
          PDFMMergeDocs(pDFMergerObj);
       On-error;
          Clear strCommand;
          strCommand = 'SNDPGMMSG MSG(''Merge of document ' +
           %trim(mergeDocID) + ' received error from MergePDFUtility class.';
          Callp QCMDEXC(strCommand:%len(strCommand));
          errorFound = *on;
       EndMon;
       numDocs = 0;

      //*** end add *****  01/09/25  *******************************

      //*** begin chg ***  08/21/24  ***********************************
      //***** strCommand = 'JAVA CLASS(''/awx/pdfbox-app-2.0.26.jar'') ' +
      //*****       'PARM(''PDFMerger'' ' +
      //*****       '''' + %Trim(mergeDocID) + ''' ' +
      //*****       '''' + %Trim(documentID) + ''' ' +
      //*****       '''' + %Trim(mergeDocID) + ''')';
      //*** begin del ***  01/09/25  *******************************
      //***** strCommand = 'JAVA CLASS(''/www/' + %trim(strLibEnv) +
      //*****       '/scriptura/pdfbox-app-2.0.26.jar'') ' +
      //*****       'PARM(''PDFMerger'' ' +
      //*****       '''' + %Trim(mergeDocID) + ''' ' +
      //*****       '''' + %Trim(documentID) + ''' ' +
      //*****       '''' + %Trim(mergeDocID) + ''')';
      //*** end chg *****  08/21/24  ***********************************
      //***** Monitor;
      //*****    QCmdExc(%Trim(strCommand):%Len(%Trim(strCommand)));
      //***** On-Error;
      //*****    errorFound = *On;
      //***** EndMon;
      //*** end del *****  01/09/25  *******************************

      /end-free

     PMergeDocuments   E
     P*** End   Add *** 09/20/22 *******************************************
**
A document
was requested by
