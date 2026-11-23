# ACME demo dataset (company ACM)

This tool generates and loads the complete demo dataset for **ACME Distribution** (company `ACM`) in library `PERPDEMO`. That covers master data, sourcing, price history, lots, requisitions, purchase orders, blanket schedules, receipts and the lot reconciliation log.

| File | What it is |
|---|---|
| `gen_acme_seed.py` | The generator. It is deterministic: a fixed random seed plus an anchor date. |
| `acme_base_catalogue.json` | The original 50-item / 10-vendor catalogue and its price history, captured from PERPDEMO on 2026-10-23. |
| `load.sh` | Copies the script to the IBM i and runs it with `RUNSQLSTM COMMIT(*CHG)`, so the whole reseed commits or rolls back as one unit. |
| `../../qddlsrc/seed/030_acme_demo_data.sql` | The generated script. This committed copy is a dated snapshot. |

## Refreshing the data

The data is anchored to the date the generator runs. History covers the previous ~125 working days, and open orders fall due over the following weeks. As time passes, open orders turn late and the dock empties. To bring it back to the present:

```bash
python3 perp/tools/acme_seed/gen_acme_seed.py          # anchor = today (use --as-of YYYY-MM-DD to reproduce a snapshot)
perp/tools/acme_seed/load.sh
```

The load **replaces every ACM-scoped row**, including anything entered through the PERP screens since the last load. `company`, `company_config`, `warehouse_layout`, `code_master` and `uom` are untouched. `perp_user` only gains rows: ERAMIREZ (buyer) and JKIM (requester). The REQ, PO and RCP document sequences are reset to the highest numbers loaded, so the screens carry on from there.

Re-running the generator rewrites the committed SQL file, so expect a diff.

## Size (anchor 2026-10-23)

| Table | Before | After |
|---|---|---|
| item / item_class | 51 / 8 | 102 / 9 (adds Plumbing and Janitorial) |
| vendor | 12 | 20 (one inactive) |
| item_vendor / item_vendor_price | 91 / 386 | 195 / 778 |
| item_lot / item_uom_conversion | 8 / 1 | 33 / 4 |
| requisition_header / line | 23 / 23 | 87 / 162 |
| po_header / line / schedule | 16 / 15 / 3 | 104 / 193 / 13 |
| po_receipt / line | 6 / 10 | 157 / 178 |
| reconciliation_log | 1 | 5 |

Document numbers start at REQ 1001, PO 4001 and RCP 7001.

## How it is modelled

The generator follows what the PERP programs themselves write, so screens and data agree.

- **Requisitions** are raised on weekdays by six requesters. Each one gets a status from its own lifecycle (draft, submitted, approved or rejected), not from a fixed quota.
- **Approvals.** CoderFlow scoring starts about 70 working days back. `reqauto` rules apply: confidence of at least `approval.auto_threshold` (85) and a total of at most `approval.auto_max_amount` (500), with its exact note text. Everything else goes to a human approver (Riya Patel or Declan Walsh).
- **Converting requisitions to POs.** Buyers run `poreqr` weekly on Tuesdays (urgent requisitions the next day). That gives one PO per preferred vendor, with `source_requisition_number/line`. Converted requisitions stay `APPROVED`, because the app never writes `CONVERTED`.
- **Other POs:**
  - buyer replenishment POs created through `poentr`, more of them in recent weeks
  - two long-lead import orders from Pacific Rim Tool Imports
  - three blanket POs with monthly or bi-weekly schedules running into the coming months
  - three drafts
- **Deliveries.** Vendors behave according to a reliability profile: Apex is unreliable, Titan and Pacific Rim sometimes slip, the rest are mostly on time. Some deliveries are short-shipped. Receipt rules follow `rcventr`: a fully received line becomes `CLOSED`, and the header becomes `RECEIVED` when all lines are closed, otherwise `PARTIAL`. Receipts are `POSTED`, and lot items get a lot per receipt.
- **Stock levels** are fitted to each item's open orders. Items with open POs were ordered because they were low; the others are healthy apart from the planted exceptions. Lot quantities are consumed oldest first, so lot totals equal on-hand.
- **Dates and audit columns.** All transactional dates fall on weekdays, and nothing in history is future-dated. `created_at/by` and `updated_at/by` reflect when each record was actually created or last touched, and by whom.

## Planted exceptions

These exist so every portal report in `perp/docs/portal-reports` has rows. The generator prints the exact list on every run.

- **Deliveries:**
  - 12 deliveries the vendor never shipped, on top of the naturally late ones (about 18 late deliveries in total, across 11 vendors)
  - ages spread across 2–14, 15–30 and 31–45 working days, with backorder notes on the oldest
  - an Apex blanket release shipped short and another backordered
  - short shipments with balances still owed
  - duplicate open POs that push items over their maximum
- **Vendors and sourcing:**
  - two items with no approved vendor (PLM-009, SAF-013)
  - two with no preferred vendor (CON-008, ELE-008), which leaves an approved requisition that `poreqr` cannot convert
  - one with an inactive preferred vendor (FAS-005 with VND20)
  - vendors missing a tax ID or email (VND13, VND17, VND19)
  - expired quotes with no replacement
  - an overlapping price row
  - three cheaper vendors that are approved but never used
  - preferred vendors that are not the cheapest
  - past and announced (future-dated) price increases
- **Lots:**
  - 2 items with no lots on file
  - 2 with lots short of on-hand
  - 2 with lots exceeding on-hand
  - 3 expired lots still holding stock
  - 4 lots expiring within 30 days
  - 1 lot with no expiry date on a shelf-life item
- **Stock:**
  - items below reorder point or critical level, some single-source and some with no vendor
  - available understated after a receipt, and an item with available 0
  - stale on-order figures
  - overstock
  - shared and mixed-class bins
  - ELE-012 stocked in CS with no conversion, and an open PO line in CS that cannot be received
- **Approvals:**
  - a self-approval (DWALSH)
  - approvals by a buyer (LCHEN) and by a requester (GJONES)
  - a CoderFlow approval at 82.5% under the old 80% threshold (a policy breach against today's 85%)
  - a submitted requisition waiting 7 working days
  - approved requisitions with no PO, a stale draft and an empty draft
  - unrealistic need-by dates

## Checks

The generator validates itself before writing the script:
- nothing over-received
- no negative balances
- exactly one preferred vendor per item, apart from the planted sourcing gaps
- alternate prices within 0.4–2.5× the preferred price
- no weekend transaction dates

After loading, these consistency queries should all return zero, except for the planted counts:
- received quantity equals the sum of receipts
- line and header statuses follow the `rcventr` rules
- header totals equal the sum of their lines
- schedule received quantities equal the line totals
- no PO is dated before its requisition was approved

The verification used on 2026-10-23 is recorded in that task's summary.
