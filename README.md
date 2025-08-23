# 🧩 TaskChain – Decentralized Skill-Sharing and Micro-Tasking on Stacks

**TaskChain** is a decentralized skill-sharing and micro-tasking platform built on the **Stacks blockchain** using **Clarity smart contracts**. It enables users to **offer, request, and complete tasks** in exchange for **STX tokens** or a **platform-specific token ($TCHN)** — all in a **trustless**, **transparent**, and **censorship-resistant** manner.

> ⚡ Powered by Bitcoin through Stacks, TaskChain leverages Clarity’s predictability and smart contract safety to build a robust peer-to-peer task marketplace.

---

## 📌 Key Features

- 🔒 **Bitcoin-Secured Blockchain** – Transactions settle on Bitcoin through Stacks, offering unmatched security.
- 📝 **Trustless Task Contracts** – Every task is governed by transparent, immutable Clarity smart contracts.
- 💼 **Reputation-Based Matching** – Users earn ratings and badges that influence task visibility and trust.
- 🪙 **STX & $TCHN Payments** – TaskChain supports native STX and custom token payments using SIP-010 standards.
- 🧠 **On-Chain Governance** – Dispute resolution and platform upgrades are community-driven through proposals and voting.
- 🧺 **Decentralized Marketplace** – No central authority controls the listings or the transactions.

---

## 🧱 Built With

- **Clarity**: Smart contract language designed for Stacks (predictable and decidable)
- **Stacks Blockchain**: Secured by Bitcoin, powers smart contracts with Clarity
- **Clarinet**: For local development, testing, and simulation of Clarity contracts
- **Stacks.js**: JavaScript library for interacting with Clarity contracts from the frontend
- **React.js**: User-friendly frontend interface
- **IPFS or Gaia**: Decentralized storage for task files and user assets

---

## 🧠 How TaskChain Works

### 1. 🚀 Task Creation
- A **Requester** creates a new task with parameters like:
  - Title, Description, Budget, Deadline
  - Payment type: `STX` or `TCHN`
- The platform deploys or updates a **task-specific Clarity contract**.
- Funds are locked in a **Clarity-based escrow**.

### 2. 💼 Bidding & Selection
- **Doers** browse available tasks and submit **proposals** (with estimated time and optional revised pricing).
- Requester **selects a Doer**, and the task contract updates the assignment on-chain.

### 3. 📦 Delivery & Approval
- Doer submits deliverables via decentralized storage (IPFS or Gaia).
- Requester approves the task, and funds are **released via smart contract**.

### 4. 🔁 Disputes & Arbitration
- If disputes arise, both parties can **initiate arbitration**.
- Arbitrators (community-selected or DAO-governed) vote on outcome.
- Escrow funds are distributed accordingly.

### 5. 🎖️ Reputation System
- Both users rate each other.
- Ratings are **stored on-chain** and used to build user reputation and rank.

---

## 💰 Token Economy

### 💎 $TCHN Token
- Platform-native SIP-010 token (ERC-20-like standard)
- Utility:
  - Task payments
  - Staking for arbitration roles
  - Governance voting
  - Loyalty and bonus rewards

### 🛡️ Escrow Mechanism
- Powered by Clarity contracts
- Funds are **locked during task execution** and only released on confirmation or arbitration

---

## 🛠️ Developer Guide

### Clarity Contract Example Snippet
```clojure
(define-data-var tasks (list 100 task) [])

(define-public (create-task (title (string-ascii 100)) (budget uint))
  (begin
    (asserts! (> budget u0) (err u100))
    ;; Logic to store task
    (ok true)))
```

### Setup Instructions
1. Clone the repo:
   ```bash
   git clone https://github.com/your-org/taskchain.git
   cd taskchain
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Run a local Clarity devnet:
   ```bash
   clarinet devnet
   ```

4. Deploy smart contracts:
   ```bash
   clarinet deploy
   ```

5. Start the frontend:
   ```bash
   npm run start
   ```

---

## 🗳️ Governance & DAO Integration

- Token holders can vote on:
  - Platform upgrades
  - Dispute outcomes
  - Feature proposals
- Voting is conducted via a **Clarity-based DAO module**.

---

## 📄 License

MIT License © 2025 TaskChain Contributors

---

## 🌐 Contributing

We welcome community contributions!

- Fork the repo
- Create a feature branch
- Submit a pull request with clear documentation

