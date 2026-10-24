# PERP exception reports for CoderFlow for Business

There are 24 plain-text prompts here. Each one starts a new report on the CoderFlow for Business (Portal) Reports page, using the PERP demo data in library `PERPDEMO` for company `ACM` (ACME Distribution).

**To use one:** open the `.txt` file, copy all of it, and paste it into a new report conversation. The prompts contain no markdown and no SQL. They describe the idea, the tables, the rules and what to show, and the report builder works out the queries. Refine the report in the conversation, check the preview against the numbers below, then activate it.

## Reports and what the preview should show

The numbers below are from live data on 23 Oct 2026. They will change as the data changes.

| # | File | Who | Preview should show |
|---|---|---|---|
| 01 | 01-approved-requisitions-waiting-for-a-po.txt | Buyer | 10 requisitions, $1,572.77, 5 vendor groups |
| 02 | 02-reorder-exceptions.txt | Buyer | 6 items, $17,011.43 suggested (TOL-004 rounds to 168, pack of 24) |
| 03 | 03-single-source-supply-risk.txt | Buyer | 20 items: 3 high (ELE-003, TOL-004, PKG-005), 1 watch; $44,894 exposure |
| 04 | 04-late-vendor-deliveries.txt | Buyer | 8 late deliveries on 5 POs, $10,062.55; worst is 80 days |
| 05 | 05-preferred-vendor-price-check.txt | Buyer | 15 items; $1,305.15 recoverable now (LUB-001 is $1,298.40) |
| 06 | 06-vendor-price-increase-watch.txt | Buyer | 19 increases of 10% or more, 11 on preferred vendors; top is PKG-007 at +30.2% |
| 07 | 07-overstock-alert.txt | Buyer | 3 items (FAS-001, FAS-003, LUB-001); $1,697.40 excess |
| 08 | 08-stale-draft-purchase-orders.txt | Buyer | 7 drafts, 6 empty; PO 19 is $54 |
| 09 | 09-todays-dock.txt | Receiver | 0 due today, 8 overdue, 4 need lot numbers |
| 10 | 10-partially-received-purchase-orders.txt | Receiver | 2 lines (PO 25/1, PO 23/1), $8,571.45 owed |
| 11 | 11-lot-integrity-check.txt | Inventory | 9 items, 720 units, $21,111.55; 7 with no lots on file |
| 12 | 12-stock-availability-check.txt | Inventory | WIDGET1 only (127 on hand, 0 available); 50 of 51 clean |
| 13 | 13-on-order-quantity-check.txt | Inventory | 7 items, 502 units; stored on-order is 0 everywhere, so the banner shows |
| 14 | 14-waiting-for-my-approval.txt | Approver | Empty queue (no SUBMITTED requisitions); policy 85% and $500 |
| 15 | 15-ai-approval-audit.txt | Controller | 4 rows, 0 breaches; requisition 13 is near the threshold, requisition 4 was routed to a human; about 12 h for AI vs 131 h for humans |
| 16 | 16-approval-control-exceptions.txt | Controller | 7 decisions by AIDEMO (not a PERP user), $1,292.97 |
| 17 | 17-my-requisitions.txt | Requester | 22 requisitions across 6 stages; requisition 7 on PO 22 has nothing received |
| 18 | 18-morning-briefing.txt | Manager | KPI tiles that match the numbers above |
| 19 | 19-vendor-scorecard.txt | Purchasing manager | 6 vendors scored (ACME01, VND01, VND02, VND03, VND04, VND07); Apex (VND07) is worst, with 2 missed deliveries worth $7,987, a short delivery on PO 25 and +11.9% on LUB-001; 6 approved vendors have never had a PO |
| 20 | 20-untapped-vendors.txt | Buyer | 4 untapped vendors (VND08 Crestline, VND09 Ironclad, VND10 Summit, ACME02); they are cheaper on 16 item-vendor pairs covering 15 items, worth about $5,894 per refill; Ironclad is PREPAID and Summit NET60 |
| 21 | 21-cash-commitment-forecast.txt | Finance | $10,062.55 committed; every delivery is overdue so it is assumed to arrive today: $584.85 (NET15) due about Nov 7 and $9,477.70 (NET30) due about Nov 22; largest single delivery is $3,993.30 |
| 22 | 22-bin-slotting-review.txt | Warehouse | 21 of 800 bins in use; 20 shared bins, all 20 mixing classes; 50 items affected |
| 23 | 23-master-data-health-check.txt | Inventory / IT | About 28 issues: 12 vendors with no tax ID, 7 lot items with no lots, ACME01 and ACME02 missing address, email and phone, CON-002 bought in EA but stocked in LB with no conversion, WIDGET1 reorder point below safety stock, 2 approved vendors with no current price, 1 overlapping price |
| 24 | 24-unrealistic-need-by-dates.txt | Approver | 2 open lines flagged: requisition 3 (WIDGET1, 5 days allowed against a 7-day lead time, 0 in stock, so at risk) and requisition 13 (SAF-004, 13 against 14, but 160 in stock, so it can be filled from stock) |

## Demo data notes

- **All the ACME activity is dated July and August 2026,** so everything shows as overdue and nothing is due today. To make reports 09 and 18 look live, seed a few delivery dates of today and tomorrow.
- **Report 14's queue is empty** because no requisitions are SUBMITTED. Move requisitions 15, 16 and 20 to SUBMITTED to demo a full queue.
- **`ITEM.qty_on_order` is never maintained.** The prompts tell the builder to work out on-order quantity from open PO lines instead.
- **`po_number` and `requisition_number` are BIGINT,** which breaks JSON output in JavaScript data tools. Every prompt tells the builder to cast them to integer.
