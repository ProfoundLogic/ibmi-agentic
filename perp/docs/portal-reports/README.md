# PERP exception reports for CoderFlow for Business

There are 24 plain-text prompts here. Each one starts a new report on the CoderFlow for Business (Portal) Reports page, using the PERP demo data in library `PERPDEMO` for company `ACM` (ACME Distribution).

**To use one:** open the `.txt` file, copy all of it, and paste it into a new report conversation. The prompts contain no markdown and no SQL. They describe the idea, the tables, the rules and what to show, and the report builder works out the queries. Refine the report in the conversation, check the preview against the numbers below, then activate it.

## Reports and what the preview should show

The numbers below come from the ACME dataset as reseeded on 23 Oct 2026 by `perp/tools/acme_seed` (anchor date 2026-10-23). If the data has been regenerated with a later anchor, or changed through the PERP screens, the numbers move but the same exceptions stay planted (see the catalogue below).

| # | File | Who | Preview should show |
|---|---|---|---|
| 01 | 01-approved-requisitions-waiting-for-a-po.txt | Buyer | 8 requisitions, $3,371.94, 3 past need-by. Requisitions 1070 and 1074 (CON-008) have no preferred vendor, so they cannot be converted. 1063 (Great Lakes Plumbing) is 11 days past need-by. |
| 02 | 02-reorder-exceptions.txt | Buyer | 16 items, $46,385.90 suggested: 6 critical, 3 below safety, 7 below reorder. Two of them (SAF-013 and PLM-009) have no vendor at all, so the action is "fix sourcing first". |
| 03 | 03-single-source-supply-risk.txt | Buyer | 27 single-source items: 4 high risk (CON-005, LUB-006, TOL-001, FAS-006) and 11 watch. $14,579 exposure. |
| 04 | 04-late-vendor-deliveries.txt | Buyer | 18 late deliveries on 13 POs, $10,216.66, across 11 vendors. They range from a few days to two months late. Apex is the biggest at $6,382, including the blanket PO 4035 releases. Great Lakes Plumbing PO 4056 and Pinnacle PO 4055 are about two months late. |
| 05 | 05-preferred-vendor-price-check.txt | Buyer | 36 items, $4,351.03 recoverable now. The top one is LUB-001: 100 still open on PO 4035, 16.2% cheaper at Summit Supply. |
| 06 | 06-vendor-price-increase-watch.txt | Buyer | 25 increases of 10% or more, 15 on preferred vendors. The top one is PKG-007 at Pinnacle (+30.2%). Several pairs also have announced future-dated increases. |
| 07 | 07-overstock-alert.txt | Buyer | 8 items. PKG-012 is already 588 ft over its maximum with 7,500 ft more on PO 4104. Duplicate open POs push ELE-001, ELE-005, FAS-004 and SAF-004 over their maximum. |
| 08 | 08-stale-draft-purchase-orders.txt | Buyer | 3 drafts: PO 4063 (empty, 46 days), PO 4072 (2 lines, $601.40, stale at 37 days) and PO 4102 (in progress). |
| 09 | 09-todays-dock.txt | Receiver | 3 due today (POs 4080, 4089, 4094), 15 in the next 7 days, 18 overdue. 5 of them need lot numbers. |
| 10 | 10-partially-received-purchase-orders.txt | Receiver | 9 partial lines, $18,444.80 owed. The largest is Apex PO 4035: 100 tubes, $13,311. |
| 11 | 11-lot-integrity-check.txt | Inventory | 6 lot mismatches: no lots for JAN-006 or PLM-005, lots short on LUB-010 and SAF-012, and lots over on CON-009 and LUB-013. 3 expired lots still hold stock (LUB-003, JAN-002, JAN-003) and 4 expire within 30 days. One shelf-life lot (CON-003) has no expiry date. |
| 12 | 12-stock-availability-check.txt | Inventory | 2 items. FAS-006 shows 5 available against 209 on hand, because the last receipt never updated available. CON-012 shows 0 available against 93. |
| 13 | 13-on-order-quantity-check.txt | Inventory | 3 items. FAS-006 shows 48 on order with nothing open. CON-011 shows 0 while PO 4103 has 32. ELE-012 doesn't match because of its PO line in CS. |
| 14 | 14-waiting-for-my-approval.txt | Approver | 8 in the queue. Requisition 1071 has waited 7 working days, which breaches the SLA. Requisitions 1076, 1080 and 1081 are over the auto limit. 1084 (SAF-011, HIGH) is needed by Oct 26. |
| 15 | 15-ai-approval-audit.txt | Controller | **1 policy breach**: requisition 1033 was approved at 82.5% under the old 80% threshold. 3 are near the threshold and 29 were routed to a human. Decisions take about 14 h for CoderFlow and 58 h for humans. |
| 16 | 16-approval-control-exceptions.txt | Controller | 3 exceptions: requisition 1053 was self-approved (Declan Walsh), 1044 was approved by buyer Lisa Chen, and 1058 was approved by requester Gary Jones. |
| 17 | 17-my-requisitions.txt | Requester | 87 requisitions: 52 received, 8 approved with no PO yet, 8 waiting for approval, 7 on order, 7 rejected, 4 not submitted, and 1 empty draft. |
| 18 | 18-morning-briefing.txt | Manager | KPI tiles matching the numbers above. Open PO value is $68,791.08 on 26 POs. |
| 19 | 19-vendor-scorecard.txt | Purchasing manager | 16 vendors scored. Apex (VND07) is worst: late lubricant drums, a short and a missed blanket release, and +11.9% on LUB-001. 3 approved vendors have never had a PO. |
| 20 | 20-untapped-vendors.txt | Buyer | 3 untapped vendors: Ironclad (VND09, PREPAID), Summit (VND10, NET60) and Tri-State MRO (VND19). That's about $4,980 per refill. |
| 21 | 21-cash-commitment-forecast.txt | Finance | $68,791.08 committed: NET15 $33,255.16, NET30 $26,582.38, NET60 $5,229.80 and COD $3,723.74. Deliveries run into December (blanket releases). |
| 22 | 22-bin-slotting-review.txt | Warehouse | 97 of 800 bins in use. There are 5 shared bins, and 3 of them mix classes (LUB-008 is in the consumables aisle at A4-B3-S3). |
| 23 | 23-master-data-health-check.txt | Inventory / IT | The issues are listed in the catalogue below. In summary: 2 items with no vendor, 2 with no preferred vendor, 1 inactive preferred vendor, vendors missing tax IDs and emails, ELE-012's missing unit conversion, 2 expired quotes, 1 overlapping price, and the lot problems. |
| 24 | 24-unrealistic-need-by-dates.txt | Approver | 2 lines. Requisition 1084 (SAF-011) is a genuine risk: 1 in stock, 4 days allowed against a 14-day lead time. Requisition 1079 (PKG-008) can be filled from stock. |

## Exception catalogue (planted by the generator)

| Area | Exceptions |
|---|---|
| Deliveries | 18 late deliveries on 13 POs across 11 vendors, aged from 4 days to two months. Backorder notes are on the oldest ones. Apex has a short-shipped and a backordered blanket release. There are short shipments with balances still owed, and duplicate open POs causing overstock. |
| Vendors and sourcing | PLM-009 and SAF-013 have no approved vendor. CON-008 and ELE-008 have no preferred vendor, which leaves requisitions 1070 and 1074 stuck. FAS-005's preferred vendor (VND20 Northern Hardware) is inactive. VND13 has no email, VND17 no tax ID, and VND19 neither. Quotes on TOL-006 and PLM-003 from VND19 expired with no replacement. ELE-004/VND05 has overlapping prices. Three cheaper vendors are approved but never used. |
| Lots | 2 items have no lots on file, 2 have lots short of on-hand, and 2 have lots exceeding on-hand. 3 expired lots still hold stock, 4 lots expire within 30 days, and 1 lot has no expiry date. |
| Stock | Items are below reorder point or critical level, some single-source and some with no vendor. One item's available figure is understated after a receipt, one shows 0 available, and two have stale on-order figures. There is overstock, and shared or mixed-class bins. |
| Approvals | A self-approval, an approval by a buyer, an approval by a requester, an AI approval under the old threshold, an SLA breach in the queue, approved requisitions with no PO, a stale draft and an empty draft, and unrealistic need-by dates. |
| Master data | ELE-012 is stocked in CS with no conversion, so PO 4095 cannot be received. |

## Demo data notes

- **The dates are relative to the run date.** There's about six months of history, plus open POs, blanket releases and announced price increases dated into the coming weeks and months. To refresh the dates, regenerate and reload; see `perp/tools/acme_seed/README.md`.
- **`ITEM.qty_on_order` and `ITEM.qty_available` are not maintained by the PERP programs.** The seed sets them correctly apart from the planted cases, but new activity through the screens will drift them. The prompts tell the builder to work out on-order from open PO lines.
- **`po_number` and `requisition_number` are BIGINT,** which breaks JSON output in JavaScript data tools. Every prompt tells the builder to cast them to integer.
