# Public Fund Smart Contract - Documentation

## 📋 Overview

### Problem Statement
Traditional fund management systems often suffer from:
- **Centralized Control**: Single points of failure/trust
- **Lack of Transparency**: Opaque approval processes
- **Inefficient Dispute Resolution**: Lengthy manual intervention required
- **Limited Accessibility**: Geographic and bureaucratic barriers
- **Audit Complexity**: Difficulty in tracking fund allocation decisions

### Core Requirements
1. **Multi-party approval** for fund release
2. **Time-bound** execution with automatic refunds
3. **Transparent** approval tracking on-chain
4. **Secure** signature verification for all actions
5. **Flexible** m-of-n threshold configuration
6. **Non-repudiation** of approvals

---

## 🏗️ Architecture & Design

### System Components

#### 1. **State Management (Datum)**
```
EscrowDatum Structure:
├── edDepositor: PubKeyHash     // Fund provider
├── edBeneficiary: PubKeyHash   // Fund recipient
├── edOfficials: [PubKeyHash]   // Authorized approvers (m)
├── edApprovals: [PubKeyHash]   // Collected approvals
├── edRequired: Integer         // Required approvals (n ≤ m)
└── edDeadline: POSIXTime       // Approval deadline
```

#### 2. **Action Types (Redeemer)**
```haskell
data EscrowAction
    = Approve   -- Official approval (pre-deadline)
    | Release   -- Fund release (pre-deadline, ≥n approvals)
    | Refund    -- Fund return (post-deadline, <n approvals)
```

#### 3. **State Transition Diagram**
```
Initial State
    ↓ (Approve × n times)
State with n approvals
    ↓ (Release by beneficiary)
FUNDS RELEASED

Initial State
    ↓ (Time passes > deadline)
    ↓ (Refund by depositor)
FUNDS REFUNDED
```
flowchart TD
subgraph OffChain[Off-Chain Layer]
D[Depositor Wallet]
B[Beneficiary Wallet]
O1[Official 1]
O2[Official 2]
EP[Contract Endpoints]
end


subgraph OnChain[On-Chain Layer]
V[Validator Script]
DT[EscrowDatum]
RD[EscrowAction Redeemer]
end


L[Cardano Ledger / UTxO Set]


D --> EP
B --> EP
O1 --> EP
O2 --> EP
EP --> V
V --> L
L --> V
V --> DT
V --> RD
---

## 🔐 Security Design

### 1. **Authentication Matrix**
| Action         | Required Signer   | Time Constraint    | Additional Conditions          |
|----------------|-------------------|--------------------|-------------------------------|
| **Approve**    | Official          | Before deadline    | Official hasn't approved yet   |
| **Release**    | Beneficiary       | Before deadline    | ≥ n approvals collected       |
| **Refund**     | Depositor         | After deadline     | < n approvals collected       |

### 2. **Validation Logic**
```haskell
-- Approve: Ensure single, unique official approval
Approve → before deadline ∧ official ∈ officials ∧ official ∉ approvals

-- Release: Ensure sufficient approvals
Release → before deadline ∧ |approvals| ≥ n ∧ signed by beneficiary

-- Refund: Ensure timeout with insufficient approvals
Refund → after deadline ∧ |approvals| < n ∧ signed by depositor
```

### 3. **Invariants**
- No official can approve more than once
- Total approvals never exceed total officials
- Funds cannot be both released and refunded
- Time constraints are strictly enforced

---

## ⚙️ Implementation Details

### 1. **Time Handling**
```haskell
-- Using PlutusV1 intervals for compatibility
beforeDeadline dl ctx = contains (to dl) (txInfoValidRange info)
afterDeadline  dl ctx = contains (from dl) (txInfoValidRange info)
```

### 2. **Signature Verification**
```haskell
-- Ensure exactly one signer for Approve
case txInfoSignatories info of
    [s] -> s  -- Valid: exactly one
    _   -> traceError "exactly one signer required"
```

### 3. **Approval Tracking**
- Implemented as append-only list in datum
- No modification or deletion of approvals
- Maintains chronological order

---

## 🚧 Limitations & Considerations

### 1. **Technical Limitations**

#### **Storage Constraints**
- **Datum Size**: Each PubKeyHash = 28 bytes
- Maximum officials limited by transaction size (~16KB)
- Typical limit: ~50 officials (28 × 50 = 1.4KB + overhead)

#### **Time Constraints**
- POSIXTime granularity = 1 millisecond
- Requires accurate time oracle/block timestamps
- No timezone support (UTC only)

#### **Transaction Costs**
```plaintext
Cost Factors:
1. Base transaction fee
2. Script execution units
3. Datum storage costs
4. Signature verification overhead

Estimated Costs (approximate):
- Deploy: ~2-3 ADA
- Approve: ~1-2 ADA
- Release/Refund: ~1.5-2.5 ADA
```

### 2. **Functional Limitations**

#### **No Partial Releases**
- All-or-nothing fund release
- Cannot release funds incrementally
- Workaround: Deploy multiple contracts

#### **Static Configuration**
- m and n cannot be changed after deployment
- Officials list is immutable
- Beneficiary cannot be changed

#### **No Emergency Override**
- No super-user or admin override
- If keys are lost, funds may be locked
- Consider multisig backup for depositor

### 3. **Operational Considerations**

#### **Key Management**
- Each official requires secure key storage
- No key rotation mechanism
- Lost official keys reduce available approvals

#### **Deadline Management**
- Fixed deadline, no extension mechanism
- Requires careful planning
- Consider buffer period for approvals

#### **Approval Coordination**
- No built-in notification system
- Off-chain coordination required
- Potential for approval race conditions

---

## 🔄 State Transition Examples

### **Scenario 1: Successful Fund Release**
```
Initial: 0 approvals, deadline = t+30d
Day 10: Official A approves → 1 approval
Day 15: Official B approves → 2 approvals
Day 20: Beneficiary releases funds → SUCCESS
```

### **Scenario 2: Timeout Refund**
```
Initial: 0 approvals, deadline = t+30d
Day 10: Official A approves → 1 approval
Day 35: Deadline passed, only 1 approval (< n)
Depositor executes refund → SUCCESS
```

### **Scenario 3: Invalid Attempts**
```
1. Official tries to approve twice → REJECTED
2. Beneficiary tries release with 1 approval (needs 2) → REJECTED
3. Depositor tries refund before deadline → REJECTED
4. Non-official tries to approve → REJECTED
```

---

## 🧪 Testing Strategy

### 1. **Unit Tests**
```haskell
-- Test cases for each validation path
testApproveValid
testApproveDuplicate
testApproveNonOfficial
testApproveAfterDeadline

testReleaseSufficientApprovals
testReleaseInsufficientApprovals
testReleaseWrongSigner
testReleaseAfterDeadline

testRefundAfterDeadlineInsufficient
testRefundBeforeDeadline
testRefundSufficientApprovals
testRefundWrongSigner
```

### 2. **Integration Tests**
- End-to-end workflow testing
- Concurrency testing (parallel approvals)
- Network condition simulations
- Gas/execution unit estimation

### 3. **Property-Based Tests**
```haskell
-- Invariant preservation
prop_approvalsNeverDecrease
prop_approvalsNeverExceedOfficials
prop_fundsNeverLost
prop_timeConstraintsRespected
```

---

## 🛡️ Security Considerations

### 1. **Known Risks**

#### **Front-Running**
- Officials might see pending approvals
- No protection against approval observation
- Consider commit-reveal pattern for sensitive cases

#### **Transaction Ordering**
- Approvals near deadline risk being too late
- Network congestion could affect timing
- Recommended: Complete approvals well before deadline

#### **Key Compromise**
- Compromised official key = unwanted approval
- Compromised beneficiary key = unauthorized release
- Regular key rotation recommended

### 2. **Mitigation Strategies**

#### **For Depositors**
- Set realistic deadlines with buffers
- Choose n > m/2 for majority consensus
- Monitor approval progress off-chain

#### **For Officials**
- Use hardware wallets for key storage
- Implement approval policies/checklists
- Coordinate with other officials

#### **For Beneficiaries**
- Verify approval threshold will be met
- Prepare release transaction in advance
- Monitor deadline approaching

---

## 🔮 Future Enhancements

### 1. **Planned Improvements**
- **Dynamic Thresholds**: Adjust n based on conditions
- **Approval Withdrawal**: Allow officials to retract approvals
- **Partial Releases**: Release funds incrementally
- **Deadline Extension**: With depositor consent

### 2. **Architecture Extensions**
```haskell
-- Proposed enhanced datum
data EnhancedEscrowDatum = EnhancedEscrowDatum
    { eedDepositor    :: PubKeyHash
    , eedBeneficiary  :: PubKeyHash
    , eedOfficials    :: Map PubKeyHash ApprovalStatus
    , eedRequired     :: Integer
    , eedDeadline     :: POSIXTime
    , eedMinAmount    :: Lovelace     -- Minimum release amount
    , eedReleased     :: Lovelace     -- Already released amount
    , eedExtensions   :: [POSIXTime]  -- Approved deadline extensions
    }
```

### 3. **Integration Features**
- **Oracle Integration**: External data for conditional release
- **Governance Voting**: DAO-style approval mechanisms
- **Cross-Chain**: Multi-chain fund management
- **Privacy**: Zero-knowledge approval proofs

---

## 📊 Performance Characteristics

### **Transaction Size Estimates**
| Component          | Size (bytes) | Notes                          |
|--------------------|--------------|--------------------------------|
| Base transaction   | ~200         | Header, inputs, outputs        |
| Datum (5 officials)| ~500         | 28 bytes × 5 + structure       |
| Redeemer           | ~50          | Small action type              |
| Script execution   | Varies       | Based on validation complexity |
| **Total**          | **~750-1000**| Typical transaction size       |

### **Execution Units**
| Operation          | CPU Units    | Memory Units   |
|--------------------|--------------|----------------|
| Signature check    | ~100,000     | ~100           |
| List operations    | ~50,000      | ~500           |
| Time validation    | ~20,000      | ~200           |
| **Total/Approve**  | **~170,000** | **~800**       |

---

## 🎯 Use Cases

### 1. **DAO Treasury Management**
- Community fund disbursement
- Proposal-based funding
- Multi-signature requirements

### 2. **Corporate Finance**
- Board-approved expenditures
- Department budget releases
- Vendor payment approvals

### 3. **Grant Disbursement**
- Philanthropic fund distribution
- Research grant releases
- Milestone-based funding

### 4. **Legal Escrow**
- Property transaction escrow
- Settlement fund management
- Contract fulfillment assurance

---

## 📝 Deployment Checklist

### **Pre-Deployment**
- [ ] Verify all PubKeyHashes are correct
- [ ] Set appropriate deadline (UTC timestamp)
- [ ] Choose m and n values carefully
- [ ] Test with small amount first
- [ ] Document contract parameters

### **During Operation**
- [ ] Monitor approval progress
- [ ] Track deadline approaching
- [ ] Coordinate with officials
- [ ] Maintain key backups

### **Post-Execution**
- [ ] Verify on-chain state changes
- [ ] Update records/accounting
- [ ] Archive contract data
- [ ] Conduct post-mortem if needed

---

## 🆘 Troubleshooting

### **Common Issues**

1. **"exactly one signer required"**
   - Ensure transaction has exactly one signature for Approve
   - Check wallet configuration

2. **Transaction fails near deadline**
   - Network latency may cause timing issues
   - Submit transactions well before deadline

3. **Insufficient funds for fees**
   - Remember: Each action requires transaction fees
   - Maintain ADA for gas in all wallets

4. **Datum too large**
   - Reduce number of officials
   - Consider splitting into multiple contracts

### **Recovery Procedures**

#### **Lost Official Key**
- Other officials continue normally
- May need to adjust quorum expectations
- Consider redeploying with new officials

#### **Missed Deadline**
- If < n approvals: Depositor can refund
- If ≥ n approvals: Beneficiary can still release
- No action = funds remain locked indefinitely

#### **Contract Bug**
- No upgrade mechanism
- Funds may be permanently locked
- Emphasizes need for thorough testing

---

## 📚 References & Resources

### **Plutus Documentation**
- [PlutusTx Documentation](https://plutus.readthedocs.io/)
- [Ledger API Reference](https://intersectMBO.github.io/plutus-apps/)
- [Cardano Node CLI](https://docs.cardano.org/developers/cardano-node/)

### **Security Best Practices**
- [Plutus Security Guidelines](https://github.com/input-output-hk/plutus/blob/master/SECURITY.md)
- [Smart Contract Auditing](https://iohk.io/en/blog/posts/2021/02/02/how-we-audit-cardano-smart-contracts/)

### **Community Resources**
- [Plutus Pioneer Program](https://plutus-pioneer-program.readthedocs.io/)
- [Cardano Stack Exchange](https://cardano.stackexchange.com/)
- [IOG Technical Discord](https://discord.gg/inputoutput)

---

**Disclaimer**: This contract has been designed for educational purposes. Always conduct thorough security audits and consider professional advice before deploying for production use with significant value.
