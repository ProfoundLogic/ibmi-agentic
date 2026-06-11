     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* UPDATE THE GENERIC TABLE FILE
     H**
     H* CREATED: 09/22/98   MIKE FLOWER
     H*                                                                    *
     H*   02/08/25 C:342 Project: FS709                                    *
     H*                  Medicare ASC Fee Schedule Generic Table file      *
     H*                  updates for ASC Payment Indicators                *
     H*                  ***Fix array issues from 146MKP**
     H*                                                                    *
     H*   09/22/23 C:062 Project: 146MKP                                   *
     H*                  Add new other fee table values.                   *
     H*                                                                    *
     H*   02/09/21 C:295 Project: FS606                                    *
     H*                  Add new charged types for FS                      *
     H**
     H*   10/30/15 C:212 Project: FS468                                    *
     H*                  Add new charged types for FS                      *
     H**
     H* LAST CHANGED FOR UPDATE RELEASE: 06/26/20
     H* LAST CHANGED FOR UPDATE TAPE: 04/06/18
     H* LAST CHANGED FOR UPDATE TAPE: 06/30/17
     H* LAST CHANGED FOR UPDATE TAPE: 05/08/06
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FFTGTL     UF A E           K DISK
     FFTGIL     UF A E           K DISK

     D*
     D*** Begin Add *** 09/22/23 **************************************
     D*** Begin Chg *** 02/08/25 **************************************
tag  D*** ITID            S              4    DIM(9) CTDATA PERRCD(1)              TBL ID, DESC
tag  D*** IDES            S             40    DIM(9) ALT(ITID)
tag  D*** TKEY            S             10    DIM(873) CTDATA PERRCD(1)            KEY, DESC
tag  D*** TDES            S             40    DIM(873) ALT(TKEY)
tag  D*** TTID            S              4    DIM(873) CTDATA PERRCD(1)            TBL ID
tag  D ITID            S              4    DIM(10) CTDATA PERRCD(1)              TBL ID, DESC
tag  D IDES            S             40    DIM(10) ALT(ITID)
tag  D TKEY            S             10    DIM(875) CTDATA PERRCD(1)            KEY, DESC
tag  D TDES            S             40    DIM(875) ALT(TKEY)
tag  D TTID            S              4    DIM(875) CTDATA PERRCD(1)            TBL ID
     D*** End   Chg *** 02/08/25 **************************************
     D*** End   Add *** 09/22/23 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     GIKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C     GTKEY1        KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  90
     C     *IN90         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#
     C*
     C*** PROCESS FTGIP
     C*** Begin Chg *** 09/22/23 *************************************
tag  C**********         DO        7             X                 4 0
tag  C                   DO        9             X                 4 0
     C*** End   Chg *** 09/22/23 *************************************
     C*
     C     ITID(X)       IFNE      *BLANKS
     C                   MOVEL     ITID(X)       KKTBID
     C*
     C     GIKEY         CHAIN     PGI01                              99
     C                   Z-ADD     KKCO#         GICO#
     C                   MOVEL     IDES(X)       GIDESC
     C                   MOVEL     ITID(X)       GITBID
     C                   MOVE      'A'           GISTAT
     C                   MOVEL(P)  'CODE'        GIFN01
     C                   MOVEL     Q1USER        GIUSER
     C                   MOVEL     Q1PGM         GIPGM
     C                   MOVEL     XXDT          GIDT
     C                   MOVEL     DSTIME        GITIME
     C*
     C     *IN99         IFEQ      *ON
     C                   WRITE     PGI01
     C                   ELSE
     C                   UPDATE    PGI01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** PROCESS FTGTP
     C*** Begin Chg *** 09/22/23 *************************************
tag  C**********         DO        871           X
     D*** Beg   Chg *** 02/08/25 **************************************
tag  C***                DO        873           X
tag  C                   DO        875           X
     D*** End   Chg *** 02/08/25 **************************************
     C*** Enb   Chg *** 09/22/23 *************************************
     C*
     C     TKEY(X)       IFNE      *BLANKS
     C*
     C                   MOVEL     TKEY(X)       KKKEY
     C                   MOVEL     TTID(X)       KKTBID
     C*
     C     GTKEY1        CHAIN     PGT01                              99
     C*
     C                   Z-ADD     KKCO#         GTCO#
     C                   MOVEL     TDES(X)       GTDESC
     C                   MOVEL     TTID(X)       GTTBID
     C                   MOVEL     TKEY(X)       GTKEY
     C                   MOVE      'A'           GTSTAT
     C                   MOVEL     Q1USER        GTUSER
     C                   MOVEL     Q1PGM         GTPGM
     C                   MOVEL     XXDT          GTDT
     C                   MOVEL     DSTIME        GTTIME
     C*
     C     *IN99         IFEQ      *ON
     C                   WRITE     PGT01
     C                   ELSE
     C                   UPDATE    PGT01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C                   ENDDO                                                  DO *IN90
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
** FTGIP ID'S & DESCRIPTIONS (ITID, IDES)
NJBGANSI X12 Bill Adjustment Group Codes
PABGANSI X12 Bill Adjustment Group Codes
CKBGANSI X12 Bill Adjustment Group Codes
NJRENJ ANSI X12 Reason Codes
PAREPA ANSI X12 Reason Codes
CKRECK ANSI X12 Reason Codes
ADSRAdmission Source
ASPIASC PAYMENT INDICATORS
NWFTNETWORK FEE TYPE (USED IN GROUP SETUP)
ASPIASC PAYMENT INDICATORS                                                        ** 02/08/25
** FTGTP KEYS & DESCRIPTIONS (TKEY, TDES)
CO        Contractual Obligation
CR        Corrections and Reversal
OA        Other Adjustment
PI        Payer Initiated Reductions
PR        Patient Responsibility
CO        Contractual Obligation
CR        Corrections and Reversal
OA        Other Adjustment
PI        Payer Initiated Reductions
PR        Patient Responsibility
CO        Contractual Obligation
CR        Corrections and Reversal
OA        Other Adjustment
PI        Payer Initiated Reductions
PR        Patient Responsibility
1         Deductible Amount
2         Coinsurance Amount
3         Co-payment Amount
4         The procedure code is inconsistent with
5         The procedure code/type of bill is incon
6         The procedure/revenue code is inconsiste
7         The procedure/revenue code is inconsiste
8         The procedure code is inconsistent with
9         The diagnosis is inconsistent with the p
10        The diagnosis is inconsistent with the p
11        The diagnosis is inconsistent with the p
12        The diagnosis is inconsistent with the p
13        The date of death precedes the date of s
14        The date of birth follows the date of se
15        The authorization number is missing, inv
16        Claim/service lacks information or has s
18        Exact duplicate claim/service (Use only
19        This is a work-related injury/illness an
20        This injury/illness is covered by the li
21        This injury/illness is the liability of
22        This care may be covered by another paye
23        The impact of prior payer(s) adjudicatio
24        Charges are covered under a capitation a
26        Expenses incurred prior to coverage.
27        Expenses incurred after coverage termina
29        The time limit for filing has expired.
31        Patient cannot be identified as our insu
32        Our records indicate the patient is not
33        Insured has no dependent coverage.
34        Insured has no coverage for newborns.
35        Lifetime benefit maximum has been reache
39        Services denied at the time authorizatio
40        Charges do not meet qualifications for e
44        Prompt-pay discount.
45        Charge exceeds fee schedule/maximum allo
49        This is a non-covered service because it
50        These are non-covered services because t
51        These are non-covered services because t
53        Services by an immediate relative or a m
54        Multiple physicians/assistants are not c
55        Procedure/treatment/drug is deemed exper
56        Procedure/treatment has not been deemed
58        Treatment was deemed by the payer to hav
59        Processed based on multiple or concurren
60        Charges for outpatient services are not
61        Adjusted for failure to obtain second su
66        Blood Deductible.
69        Day outlier amount.
70        Cost outlier - Adjustment to compensate
74        Indirect Medical Education Adjustment.
75        Direct Medical Education Adjustment.
76        Disproportionate Share Adjustment.
78        Non-Covered days/Room charge adjustment.
85        Patient Interest Adjustment (Use Only Gr
89        Professional fees removed from charges.
90        Ingredient cost adjustment. Usage: To be
91        Dispensing fee adjustment.
94        Processed in Excess of charges.
95        Plan procedures not followed.
96        Non-covered charge(s). At least one Rema
97        The benefit for this service is included
100       Payment made to patient/insured/responsi
101       Predetermination: anticipated payment up
102       Major Medical Adjustment.
103       Provider promotional discount (e.g., Sen
104       Managed care withholding.
105       Tax withholding.
106       Patient payment option/election not in e
107       The related or qualifying claim/service
108       Rent/purchase guidelines were not met. U
109       Claim/service not covered by this payer/
110       Billing date predates service date.
111       Not covered unless the provider accepts
112       Service not furnished directly to the pa
114       Procedure/product not approved by the Fo
115       Procedure postponed, canceled, or delaye
116       The advance indemnification notice signe
117       Transportation is only covered to the cl
118       ESRD network support adjustment.
119       Benefit maximum for this time period or
121       Indemnification adjustment - compensatio
122       Psychiatric reduction.
128       Newborn's services are covered in the mo
129       Prior processing information appears inc
130       Claim submission fee.
131       Claim specific negotiated discount.
132       Prearranged demonstration project adjust
133       The disposition of this service line is
134       Technical fees removed from charges.
135       Interim bills cannot be processed.
136       Failure to follow prior payer's coverage
137       Regulatory Surcharges, Assessments, Allo
138       Appeal procedures not followed or time l
139       Contracted funding agreement - Subscribe
140       Patient/Insured health identification nu
142       Monthly Medicaid patient liability amoun
143       Portion of payment deferred.
144       Incentive adjustment, e.g. preferred pro
146       Diagnosis was invalid for the date(s) of
147       Provider contracted/negotiated rate expi
148       Information from another provider was no
149       Lifetime benefit maximum has been reache
150       Payer deems the information submitted do
151       Payment adjusted because the payer deems
152       Payer deems the information submitted do
153       Payer deems the information submitted do
154       Payer deems the information submitted do
155       Patient refused the service/procedure.
157       Service/procedure was provided as a resu
158       Service/procedure was provided outside o
159       Service/procedure was provided as a resu
160       Injury/illness was the result of an acti
161       Provider performance bonus
163       Attachment/other documentation reference
164       Attachment/other documentation reference
165       Referral absent or exceeded.
166       These services were submitted after this
167       This (these) diagnosis(es) is (are) not
168       Service(s) have been considered under th
169       Alternate benefit has been provided.
170       Payment is denied when performed/billed
171       Payment is denied when performed/billed
172       Payment is adjusted when performed/bille
173       Service/equipment was not prescribed by
174       Service was not prescribed prior to deli
175       Prescription is incomplete.
176       Prescription is not current.
177       Patient has not met the required eligibi
178       Patient has not met the required spend d
179       Patient has not met the required waiting
180       Patient has not met the required residen
181       Procedure code was invalid on the date o
182       Procedure modifier was invalid on the da
183       The referring provider is not eligible t
184       The prescribing/ordering provider is not
185       The rendering provider is not eligible t
186       Level of care change adjustment.
187       Consumer Spending Account payments (incl
188       This product/procedure is only covered w
189       Not otherwise classified or unlisted
190       Payment is included in the allowance for
192       Non standard adjustment code from paper
193       Original payment decision is being maint
194       Anesthesia performed by the operating ph
195       Refund issued to an erroneous priority p
197       Precertification/authorization/notificat
198       Precertification/authorization exceeded.
199       Revenue code and Procedure code do not m
200       Expenses incurred during lapse in covera
201       Patient is responsible for amount of thi
202       Non-covered personal comfort or convenie
203       Discontinued or reduced service.
204       This service/equipment/drug is not cover
205       Pharmacy discount card processing fee
206       National Provider Identifier - missing.
207       National Provider identifier - Invalid f
208       National Provider Identifier - Not match
209       Per regulatory or other agreement. The p
210       Payment adjusted because pre-certificati
211       National Drug Codes (NDC) not eligible f
212       Administrative surcharges are not covere
213       Non-compliance with the physician self r
215       Based on subrogation of a third party se
216       Based on the findings of a review organi
219       Based on extent of injury. Usage: If adj
222       Exceeds the contracted maximum number of
223       Adjustment code for mandated federal, st
224       Patient identification compromised by id
225       Penalty or Interest Payment by Payer (On
226       Information requested from the Billing/R
227       Information requested from the patient/i
228       Denied for failure of this provider, ano
229       Partial charge amount not considered by
231       Mutually exclusive procedures cannot be
232       Institutional Transfer Amount. Usage: Ap
233       Services/charges related to the treatmen
234       This procedure is not paid separately. A
235       Sales Tax
236       This procedure or procedure/modifier com
237       Legislated/Regulatory Penalty. At least
238       Claim spans eligible and ineligible peri
239       Claim spans eligible and ineligible peri
240       The diagnosis is inconsistent with the p
241       Low Income Subsidy (LIS) Co-payment Amou
242       Services not provided by network/primary
243       Services not authorized by network/prima
245       Provider performance program withhold.
246       This non-payable code is for required re
247       Deductible for Professional service rend
248       Coinsurance for Professional service ren
249       This claim has been identified as a read
250       The attachment/other documentation that
251       The attachment/other documentation that
252       An attachment/other documentation is req
253       Sequestration - reduction in federal pay
254       Claim received by the dental plan, but b
256       Service not payable per managed care con
257       The disposition of the claim/service is
258       Claim/service not covered when patient i
259       Additional payment for Dental/Vision ser
260       Processed under Medicaid ACA Enhanced Fe
261       The procedure or service is inconsistent
262       Adjustment for delivery cost. Usage: To
263       Adjustment for shipping cost. Usage: To
264       Adjustment for postage cost. Usage: To b
265       Adjustment for administrative cost. Usag
266       Adjustment for compound preparation cost
267       Claim/service spans multiple months. At
268       The Claim spans two calendar years. Plea
269       Anesthesia not covered for this service/
270       Claim received by the medical plan, but
271       Prior contractual reductions related to
272       Coverage/program guidelines were not met
273       Coverage/program guidelines were exceede
274       Fee/Service not payable per patient Care
275       Prior payer's (or payers') patient respo
276       Services denied by the prior payer(s) ar
277       The disposition of the claim/service is
278       Performance program proficiency requirem
279       Services not provided by Preferred netwo
280       Claim received by the medical plan, but
281       Deductible waived per contractual agreem
282       The procedure/revenue code is inconsiste
283       Attending provider is not eligible to pr
284       Precertification/authorization/notificat
285       Appeal procedures not followed
286       Appeal time limits not met
287       Referral exceeded
288       Referral absent
289       Services considered under the dental and
290       Claim received by the dental plan, but b
291       Claim received by the medical plan, but
292       Claim received by the medical plan, but
293       Payment made to employer.
294       Payment made to attorney.
295       Pharmacy Direct/Indirect Remuneration (D
A0        Patient refund amount.
A1        Claim/Service denied. At least one Remar
A5        Medicare Claim PPS Capital Cost Outlier
A6        Prior hospitalization or 30 day transfer
A8        Ungroupable DRG.
B1        Non-covered visits.
B4        Late filing penalty.
B7        This provider was not certified/eligible
B8        Alternative services were available, and
B9        Patient is enrolled in a Hospice.
B10       Allowed amount has been reduced because
B11       The claim/service has been transferred t
B12       Services not documented in patient's med
B13       Previously paid. Payment for this claim/
B14       Only one visit or consultation per physi
B15       This service/procedure requires that a q
B16       'New Patient' qualifications were not me
B20       Procedure/service was partially or fully
B22       This payment is adjusted based on the di
B23       Procedure billed is not authorized per y
P1        State-mandated Requirement for Property
P2        Not a work related injury/illness and th
P3        Workers' Compensation case settled. Pati
P4        Workers' Compensation claim adjudicated
P5        Based on payer reasonable and customary
P6        Based on entitlement to benefits. Usage:
P7        The applicable fee schedule/fee database
P8        Claim is under investigation. Usage: If
P9        No available or correlating CPT/HCPCS co
P10       Payment reduced to zero due to litigatio
P11       The disposition of the related Property
P12       Workers' compensation jurisdictional fee
P13       Payment reduced or denied based on worke
P14       The Benefit for this Service is included
P15       Workers' Compensation Medical Treatment
P16       Medical provider not authorized/certifie
P17       Referral not authorized by attending phy
P18       Procedure is not listed in the jurisdict
P19       Procedure has a relative value of zero i
P20       Service not paid under jurisdiction allo
P21       Payment denied based on the Medical Paym
P22       Payment adjusted based on the Medical Pa
P23       Medical Payments Coverage (MPC) or Perso
P24       Payment adjusted based on Preferred Prov
P25       Payment adjusted based on Medical Provid
P26       Payment adjusted based on Voluntary Prov
P27       Payment denied based on the Liability Co
P28       Payment adjusted based on the Liability
P29       Liability Benefits jurisdictional fee sc
1         Deductible Amount
2         Coinsurance Amount
3         Co-payment Amount
4         The procedure code is inconsistent with
5         The procedure code/type of bill is incon
6         The procedure/revenue code is inconsiste
7         The procedure/revenue code is inconsiste
8         The procedure code is inconsistent with
9         The diagnosis is inconsistent with the p
10        The diagnosis is inconsistent with the p
11        The diagnosis is inconsistent with the p
12        The diagnosis is inconsistent with the p
13        The date of death precedes the date of s
14        The date of birth follows the date of se
15        The authorization number is missing, inv
16        Claim/service lacks information or has s
18        Exact duplicate claim/service (Use only
19        This is a work-related injury/illness an
20        This injury/illness is covered by the li
21        This injury/illness is the liability of
22        This care may be covered by another paye
23        The impact of prior payer(s) adjudicatio
24        Charges are covered under a capitation a
26        Expenses incurred prior to coverage.
27        Expenses incurred after coverage termina
29        The time limit for filing has expired.
31        Patient cannot be identified as our insu
32        Our records indicate the patient is not
33        Insured has no dependent coverage.
34        Insured has no coverage for newborns.
35        Lifetime benefit maximum has been reache
39        Services denied at the time authorizatio
40        Charges do not meet qualifications for e
44        Prompt-pay discount.
45        Charge exceeds fee schedule/maximum allo
49        This is a non-covered service because it
50        These are non-covered services because t
51        These are non-covered services because t
53        Services by an immediate relative or a m
54        Multiple physicians/assistants are not c
55        Procedure/treatment/drug is deemed exper
56        Procedure/treatment has not been deemed
58        Treatment was deemed by the payer to hav
59        Processed based on multiple or concurren
60        Charges for outpatient services are not
61        Adjusted for failure to obtain second su
66        Blood Deductible.
69        Day outlier amount.
70        Cost outlier - Adjustment to compensate
74        Indirect Medical Education Adjustment.
75        Direct Medical Education Adjustment.
76        Disproportionate Share Adjustment.
78        Non-Covered days/Room charge adjustment.
85        Patient Interest Adjustment (Use Only Gr
89        Professional fees removed from charges.
90        Ingredient cost adjustment. Usage: To be
91        Dispensing fee adjustment.
94        Processed in Excess of charges.
95        Plan procedures not followed.
96        Non-covered charge(s). At least one Rema
97        The benefit for this service is included
100       Payment made to patient/insured/responsi
101       Predetermination: anticipated payment up
102       Major Medical Adjustment.
103       Provider promotional discount (e.g., Sen
104       Managed care withholding.
105       Tax withholding.
106       Patient payment option/election not in e
107       The related or qualifying claim/service
108       Rent/purchase guidelines were not met. U
109       Claim/service not covered by this payer/
110       Billing date predates service date.
111       Not covered unless the provider accepts
112       Service not furnished directly to the pa
114       Procedure/product not approved by the Fo
115       Procedure postponed, canceled, or delaye
116       The advance indemnification notice signe
117       Transportation is only covered to the cl
118       ESRD network support adjustment.
119       Benefit maximum for this time period or
121       Indemnification adjustment - compensatio
122       Psychiatric reduction.
128       Newborn's services are covered in the mo
129       Prior processing information appears inc
130       Claim submission fee.
131       Claim specific negotiated discount.
132       Prearranged demonstration project adjust
133       The disposition of this service line is
134       Technical fees removed from charges.
135       Interim bills cannot be processed.
136       Failure to follow prior payer's coverage
137       Regulatory Surcharges, Assessments, Allo
138       Appeal procedures not followed or time l
139       Contracted funding agreement - Subscribe
140       Patient/Insured health identification nu
142       Monthly Medicaid patient liability amoun
143       Portion of payment deferred.
144       Incentive adjustment, e.g. preferred pro
146       Diagnosis was invalid for the date(s) of
147       Provider contracted/negotiated rate expi
148       Information from another provider was no
149       Lifetime benefit maximum has been reache
150       Payer deems the information submitted do
151       Payment adjusted because the payer deems
152       Payer deems the information submitted do
153       Payer deems the information submitted do
154       Payer deems the information submitted do
155       Patient refused the service/procedure.
157       Service/procedure was provided as a resu
158       Service/procedure was provided outside o
159       Service/procedure was provided as a resu
160       Injury/illness was the result of an acti
161       Provider performance bonus
163       Attachment/other documentation reference
164       Attachment/other documentation reference
165       Referral absent or exceeded.
166       These services were submitted after this
167       This (these) diagnosis(es) is (are) not
168       Service(s) have been considered under th
169       Alternate benefit has been provided.
170       Payment is denied when performed/billed
171       Payment is denied when performed/billed
172       Payment is adjusted when performed/bille
173       Service/equipment was not prescribed by
174       Service was not prescribed prior to deli
175       Prescription is incomplete.
176       Prescription is not current.
177       Patient has not met the required eligibi
178       Patient has not met the required spend d
179       Patient has not met the required waiting
180       Patient has not met the required residen
181       Procedure code was invalid on the date o
182       Procedure modifier was invalid on the da
183       The referring provider is not eligible t
184       The prescribing/ordering provider is not
185       The rendering provider is not eligible t
186       Level of care change adjustment.
187       Consumer Spending Account payments (incl
188       This product/procedure is only covered w
189       Not otherwise classified or unlisted
190       Payment is included in the allowance for
192       Non standard adjustment code from paper
193       Original payment decision is being maint
194       Anesthesia performed by the operating ph
195       Refund issued to an erroneous priority p
197       Precertification/authorization/notificat
198       Precertification/authorization exceeded.
199       Revenue code and Procedure code do not m
200       Expenses incurred during lapse in covera
201       Patient is responsible for amount of thi
202       Non-covered personal comfort or convenie
203       Discontinued or reduced service.
204       This service/equipment/drug is not cover
205       Pharmacy discount card processing fee
206       National Provider Identifier - missing.
207       National Provider identifier - Invalid f
208       National Provider Identifier - Not match
209       Per regulatory or other agreement. The p
210       Payment adjusted because pre-certificati
211       National Drug Codes (NDC) not eligible f
212       Administrative surcharges are not covere
213       Non-compliance with the physician self r
215       Based on subrogation of a third party se
216       Based on the findings of a review organi
219       Based on extent of injury. Usage: If adj
222       Exceeds the contracted maximum number of
223       Adjustment code for mandated federal, st
224       Patient identification compromised by id
225       Penalty or Interest Payment by Payer (On
226       Information requested from the Billing/R
227       Information requested from the patient/i
228       Denied for failure of this provider, ano
229       Partial charge amount not considered by
231       Mutually exclusive procedures cannot be
232       Institutional Transfer Amount. Usage: Ap
233       Services/charges related to the treatmen
234       This procedure is not paid separately. A
235       Sales Tax
236       This procedure or procedure/modifier com
237       Legislated/Regulatory Penalty. At least
238       Claim spans eligible and ineligible peri
239       Claim spans eligible and ineligible peri
240       The diagnosis is inconsistent with the p
241       Low Income Subsidy (LIS) Co-payment Amou
242       Services not provided by network/primary
243       Services not authorized by network/prima
245       Provider performance program withhold.
246       This non-payable code is for required re
247       Deductible for Professional service rend
248       Coinsurance for Professional service ren
249       This claim has been identified as a read
250       The attachment/other documentation that
251       The attachment/other documentation that
252       An attachment/other documentation is req
253       Sequestration - reduction in federal pay
254       Claim received by the dental plan, but b
256       Service not payable per managed care con
257       The disposition of the claim/service is
258       Claim/service not covered when patient i
259       Additional payment for Dental/Vision ser
260       Processed under Medicaid ACA Enhanced Fe
261       The procedure or service is inconsistent
262       Adjustment for delivery cost. Usage: To
263       Adjustment for shipping cost. Usage: To
264       Adjustment for postage cost. Usage: To b
265       Adjustment for administrative cost. Usag
266       Adjustment for compound preparation cost
267       Claim/service spans multiple months. At
268       The Claim spans two calendar years. Plea
269       Anesthesia not covered for this service/
270       Claim received by the medical plan, but
271       Prior contractual reductions related to
272       Coverage/program guidelines were not met
273       Coverage/program guidelines were exceede
274       Fee/Service not payable per patient Care
275       Prior payer's (or payers') patient respo
276       Services denied by the prior payer(s) ar
277       The disposition of the claim/service is
278       Performance program proficiency requirem
279       Services not provided by Preferred netwo
280       Claim received by the medical plan, but
281       Deductible waived per contractual agreem
282       The procedure/revenue code is inconsiste
283       Attending provider is not eligible to pr
284       Precertification/authorization/notificat
285       Appeal procedures not followed
286       Appeal time limits not met
287       Referral exceeded
288       Referral absent
289       Services considered under the dental and
290       Claim received by the dental plan, but b
291       Claim received by the medical plan, but
292       Claim received by the medical plan, but
293       Payment made to employer.
294       Payment made to attorney.
295       Pharmacy Direct/Indirect Remuneration (D
A0        Patient refund amount.
A1        Claim/Service denied. At least one Remar
A5        Medicare Claim PPS Capital Cost Outlier
A6        Prior hospitalization or 30 day transfer
A8        Ungroupable DRG.
B1        Non-covered visits.
B4        Late filing penalty.
B7        This provider was not certified/eligible
B8        Alternative services were available, and
B9        Patient is enrolled in a Hospice.
B10       Allowed amount has been reduced because
B11       The claim/service has been transferred t
B12       Services not documented in patient's med
B13       Previously paid. Payment for this claim/
B14       Only one visit or consultation per physi
B15       This service/procedure requires that a q
B16       'New Patient' qualifications were not me
B20       Procedure/service was partially or fully
B22       This payment is adjusted based on the di
B23       Procedure billed is not authorized per y
P1        State-mandated Requirement for Property
P2        Not a work related injury/illness and th
P3        Workers' Compensation case settled. Pati
P4        Workers' Compensation claim adjudicated
P5        Based on payer reasonable and customary
P6        Based on entitlement to benefits. Usage:
P7        The applicable fee schedule/fee database
P8        Claim is under investigation. Usage: If
P9        No available or correlating CPT/HCPCS co
P10       Payment reduced to zero due to litigatio
P11       The disposition of the related Property
P12       Workers' compensation jurisdictional fee
P13       Payment reduced or denied based on worke
P14       The Benefit for this Service is included
P15       Workers' Compensation Medical Treatment
P16       Medical provider not authorized/certifie
P17       Referral not authorized by attending phy
P18       Procedure is not listed in the jurisdict
P19       Procedure has a relative value of zero i
P20       Service not paid under jurisdiction allo
P21       Payment denied based on the Medical Paym
P22       Payment adjusted based on the Medical Pa
P23       Medical Payments Coverage (MPC) or Perso
P24       Payment adjusted based on Preferred Prov
P25       Payment adjusted based on Medical Provid
P26       Payment adjusted based on Voluntary Prov
P27       Payment denied based on the Liability Co
P28       Payment adjusted based on the Liability
P29       Liability Benefits jurisdictional fee sc
1         Deductible Amount
2         Coinsurance Amount
3         Co-payment Amount
4         The procedure code is inconsistent with
5         The procedure code/type of bill is incon
6         The procedure/revenue code is inconsiste
7         The procedure/revenue code is inconsiste
8         The procedure code is inconsistent with
9         The diagnosis is inconsistent with the p
10        The diagnosis is inconsistent with the p
11        The diagnosis is inconsistent with the p
12        The diagnosis is inconsistent with the p
13        The date of death precedes the date of s
14        The date of birth follows the date of se
15        The authorization number is missing, inv
16        Claim/service lacks information or has s
18        Exact duplicate claim/service (Use only
19        This is a work-related injury/illness an
20        This injury/illness is covered by the li
21        This injury/illness is the liability of
22        This care may be covered by another paye
23        The impact of prior payer(s) adjudicatio
24        Charges are covered under a capitation a
26        Expenses incurred prior to coverage.
27        Expenses incurred after coverage termina
29        The time limit for filing has expired.
31        Patient cannot be identified as our insu
32        Our records indicate the patient is not
33        Insured has no dependent coverage.
34        Insured has no coverage for newborns.
35        Lifetime benefit maximum has been reache
39        Services denied at the time authorizatio
40        Charges do not meet qualifications for e
44        Prompt-pay discount.
45        Charge exceeds fee schedule/maximum allo
49        This is a non-covered service because it
50        These are non-covered services because t
51        These are non-covered services because t
53        Services by an immediate relative or a m
54        Multiple physicians/assistants are not c
55        Procedure/treatment/drug is deemed exper
56        Procedure/treatment has not been deemed
58        Treatment was deemed by the payer to hav
59        Processed based on multiple or concurren
60        Charges for outpatient services are not
61        Adjusted for failure to obtain second su
66        Blood Deductible.
69        Day outlier amount.
70        Cost outlier - Adjustment to compensate
74        Indirect Medical Education Adjustment.
75        Direct Medical Education Adjustment.
76        Disproportionate Share Adjustment.
78        Non-Covered days/Room charge adjustment.
85        Patient Interest Adjustment (Use Only Gr
89        Professional fees removed from charges.
90        Ingredient cost adjustment. Usage: To be
91        Dispensing fee adjustment.
94        Processed in Excess of charges.
95        Plan procedures not followed.
96        Non-covered charge(s). At least one Rema
97        The benefit for this service is included
100       Payment made to patient/insured/responsi
101       Predetermination: anticipated payment up
102       Major Medical Adjustment.
103       Provider promotional discount (e.g., Sen
104       Managed care withholding.
105       Tax withholding.
106       Patient payment option/election not in e
107       The related or qualifying claim/service
108       Rent/purchase guidelines were not met. U
109       Claim/service not covered by this payer/
110       Billing date predates service date.
111       Not covered unless the provider accepts
112       Service not furnished directly to the pa
114       Procedure/product not approved by the Fo
115       Procedure postponed, canceled, or delaye
116       The advance indemnification notice signe
117       Transportation is only covered to the cl
118       ESRD network support adjustment.
119       Benefit maximum for this time period or
121       Indemnification adjustment - compensatio
122       Psychiatric reduction.
128       Newborn's services are covered in the mo
129       Prior processing information appears inc
130       Claim submission fee.
131       Claim specific negotiated discount.
132       Prearranged demonstration project adjust
133       The disposition of this service line is
134       Technical fees removed from charges.
135       Interim bills cannot be processed.
136       Failure to follow prior payer's coverage
137       Regulatory Surcharges, Assessments, Allo
138       Appeal procedures not followed or time l
139       Contracted funding agreement - Subscribe
140       Patient/Insured health identification nu
142       Monthly Medicaid patient liability amoun
143       Portion of payment deferred.
144       Incentive adjustment, e.g. preferred pro
146       Diagnosis was invalid for the date(s) of
147       Provider contracted/negotiated rate expi
148       Information from another provider was no
149       Lifetime benefit maximum has been reache
150       Payer deems the information submitted do
151       Payment adjusted because the payer deems
152       Payer deems the information submitted do
153       Payer deems the information submitted do
154       Payer deems the information submitted do
155       Patient refused the service/procedure.
157       Service/procedure was provided as a resu
158       Service/procedure was provided outside o
159       Service/procedure was provided as a resu
160       Injury/illness was the result of an acti
161       Provider performance bonus
163       Attachment/other documentation reference
164       Attachment/other documentation reference
165       Referral absent or exceeded.
166       These services were submitted after this
167       This (these) diagnosis(es) is (are) not
168       Service(s) have been considered under th
169       Alternate benefit has been provided.
170       Payment is denied when performed/billed
171       Payment is denied when performed/billed
172       Payment is adjusted when performed/bille
173       Service/equipment was not prescribed by
174       Service was not prescribed prior to deli
175       Prescription is incomplete.
176       Prescription is not current.
177       Patient has not met the required eligibi
178       Patient has not met the required spend d
179       Patient has not met the required waiting
180       Patient has not met the required residen
181       Procedure code was invalid on the date o
182       Procedure modifier was invalid on the da
183       The referring provider is not eligible t
184       The prescribing/ordering provider is not
185       The rendering provider is not eligible t
186       Level of care change adjustment.
187       Consumer Spending Account payments (incl
188       This product/procedure is only covered w
189       Not otherwise classified or unlisted
190       Payment is included in the allowance for
192       Non standard adjustment code from paper
193       Original payment decision is being maint
194       Anesthesia performed by the operating ph
195       Refund issued to an erroneous priority p
197       Precertification/authorization/notificat
198       Precertification/authorization exceeded.
199       Revenue code and Procedure code do not m
200       Expenses incurred during lapse in covera
201       Patient is responsible for amount of thi
202       Non-covered personal comfort or convenie
203       Discontinued or reduced service.
204       This service/equipment/drug is not cover
205       Pharmacy discount card processing fee
206       National Provider Identifier - missing.
207       National Provider identifier - Invalid f
208       National Provider Identifier - Not match
209       Per regulatory or other agreement. The p
210       Payment adjusted because pre-certificati
211       National Drug Codes (NDC) not eligible f
212       Administrative surcharges are not covere
213       Non-compliance with the physician self r
215       Based on subrogation of a third party se
216       Based on the findings of a review organi
219       Based on extent of injury. Usage: If adj
222       Exceeds the contracted maximum number of
223       Adjustment code for mandated federal, st
224       Patient identification compromised by id
225       Penalty or Interest Payment by Payer (On
226       Information requested from the Billing/R
227       Information requested from the patient/i
228       Denied for failure of this provider, ano
229       Partial charge amount not considered by
231       Mutually exclusive procedures cannot be
232       Institutional Transfer Amount. Usage: Ap
233       Services/charges related to the treatmen
234       This procedure is not paid separately. A
235       Sales Tax
236       This procedure or procedure/modifier com
237       Legislated/Regulatory Penalty. At least
238       Claim spans eligible and ineligible peri
239       Claim spans eligible and ineligible peri
240       The diagnosis is inconsistent with the p
241       Low Income Subsidy (LIS) Co-payment Amou
242       Services not provided by network/primary
243       Services not authorized by network/prima
245       Provider performance program withhold.
246       This non-payable code is for required re
247       Deductible for Professional service rend
248       Coinsurance for Professional service ren
249       This claim has been identified as a read
250       The attachment/other documentation that
251       The attachment/other documentation that
252       An attachment/other documentation is req
253       Sequestration - reduction in federal pay
254       Claim received by the dental plan, but b
256       Service not payable per managed care con
257       The disposition of the claim/service is
258       Claim/service not covered when patient i
259       Additional payment for Dental/Vision ser
260       Processed under Medicaid ACA Enhanced Fe
261       The procedure or service is inconsistent
262       Adjustment for delivery cost. Usage: To
263       Adjustment for shipping cost. Usage: To
264       Adjustment for postage cost. Usage: To b
265       Adjustment for administrative cost. Usag
266       Adjustment for compound preparation cost
267       Claim/service spans multiple months. At
268       The Claim spans two calendar years. Plea
269       Anesthesia not covered for this service/
270       Claim received by the medical plan, but
271       Prior contractual reductions related to
272       Coverage/program guidelines were not met
273       Coverage/program guidelines were exceede
274       Fee/Service not payable per patient Care
275       Prior payer's (or payers') patient respo
276       Services denied by the prior payer(s) ar
277       The disposition of the claim/service is
278       Performance program proficiency requirem
279       Services not provided by Preferred netwo
280       Claim received by the medical plan, but
281       Deductible waived per contractual agreem
282       The procedure/revenue code is inconsiste
283       Attending provider is not eligible to pr
284       Precertification/authorization/notificat
285       Appeal procedures not followed
286       Appeal time limits not met
287       Referral exceeded
288       Referral absent
289       Services considered under the dental and
290       Claim received by the dental plan, but b
291       Claim received by the medical plan, but
292       Claim received by the medical plan, but
293       Payment made to employer.
294       Payment made to attorney.
295       Pharmacy Direct/Indirect Remuneration (D
A0        Patient refund amount.
A1        Claim/Service denied. At least one Remar
A5        Medicare Claim PPS Capital Cost Outlier
A6        Prior hospitalization or 30 day transfer
A8        Ungroupable DRG.
B1        Non-covered visits.
B4        Late filing penalty.
B7        This provider was not certified/eligible
B8        Alternative services were available, and
B9        Patient is enrolled in a Hospice.
B10       Allowed amount has been reduced because
B11       The claim/service has been transferred t
B12       Services not documented in patient's med
B13       Previously paid. Payment for this claim/
B14       Only one visit or consultation per physi
B15       This service/procedure requires that a q
B16       'New Patient' qualifications were not me
B20       Procedure/service was partially or fully
B22       This payment is adjusted based on the di
B23       Procedure billed is not authorized per y
P1        State-mandated Requirement for Property
P2        Not a work related injury/illness and th
P3        Workers' Compensation case settled. Pati
P4        Workers' Compensation claim adjudicated
P5        Based on payer reasonable and customary
P6        Based on entitlement to benefits. Usage:
P7        The applicable fee schedule/fee database
P8        Claim is under investigation. Usage: If
P9        No available or correlating CPT/HCPCS co
P10       Payment reduced to zero due to litigatio
P11       The disposition of the related Property
P12       Workers' compensation jurisdictional fee
P13       Payment reduced or denied based on worke
P14       The Benefit for this Service is included
P15       Workers' Compensation Medical Treatment
P16       Medical provider not authorized/certifie
P17       Referral not authorized by attending phy
P18       Procedure is not listed in the jurisdict
P19       Procedure has a relative value of zero i
P20       Service not paid under jurisdiction allo
P21       Payment denied based on the Medical Paym
P22       Payment adjusted based on the Medical Pa
P23       Medical Payments Coverage (MPC) or Perso
P24       Payment adjusted based on Preferred Prov
P25       Payment adjusted based on Medical Provid
P26       Payment adjusted based on Voluntary Prov
P27       Payment denied based on the Liability Co
P28       Payment adjusted based on the Liability
P29       Liability Benefits jurisdictional fee sc
G         Transfer From a Designated Disaster Alte
K5        Pricing/Claims Data not Available.
2         Per Savings (Percentage of Other 2 Reductions)
D1        Ancillary dental service/item;no Separar                                ** 02/08/25
D2        Non office-based dental procedure added                                 ** 02/08/25
** FTGTP TSite ID  (TTID)
NJBG
NJBG
NJBG
NJBG
NJBG
PABG
PABG
PABG
PABG
PABG
CKBG
CKBG
CKBG
CKBG
CKBG
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
NJRE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
PARE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
CKRE
ADSR
ASPI
NWFT
ASPI                                                                              ** 02/08/25
ASPI                                                                              ** 02/08/25
