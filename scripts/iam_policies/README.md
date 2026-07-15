# HopDesk IAM Policy Templates

## Overview

One IAM user (or role) per client deployment.  
Each key may only read/write **its own** client prefix — it cannot enumerate other clients or
access `hd-admin/` unless it is the hd-admin key.

## Files

| File | Purpose |
|------|---------|
| `client_policy_template.json` | One per client deployment (networks, hopdesk, ...) |
| `hd_admin_policy.json` | The hd-admin deployment's key — broader read + write to `hd-admin/` prefix AND the ability to read/write any client prefix for cross-client admin operations |

## How to apply in AWS Console

1. **AWS Console → IAM → Users → Create user**
   - Username: `hopdesk-<client_id>-app` (e.g. `hopdesk-networks-app`)
   - Select "Attach policies directly"
   - Choose "Create policy" → JSON tab → paste the template (with substitutions done)

2. **Create access key:**
   - User → Security credentials → Access keys → Create access key → Application running outside AWS
   - Download `.csv` — this is the only time you see the secret key

3. **Add to `.Renviron` for that deployment:**
   ```
   AWS_ACCESS_KEY_ID=<key id from CSV>
   AWS_SECRET_ACCESS_KEY=<secret from CSV>
   ```

4. **Repeat for each deployment** with a fresh IAM user and the same template (substituting CLIENT_ID each time).

## Current state (single shared key)

Until IAM hardening is applied, all deployments share one key.  
The `ListBucket` restriction in these policies is the most important control — apply it even
on the shared key by restricting the allowed prefixes to the union of all client prefixes.

## Verification after applying

From an R console authenticated as the networks key:

```r
library(aws.s3)
# Should SUCCEED — own prefix
aws.s3::get_bucket("antiguedad-rds-prod", prefix = "networks/", max = 5)

# Should FAIL with 403 or empty — another client's prefix
aws.s3::get_bucket("antiguedad-rds-prod", prefix = "hopdesk/", max = 5)

# Should FAIL with 403 — root listing
aws.s3::get_bucket("antiguedad-rds-prod", max = 1)
```
