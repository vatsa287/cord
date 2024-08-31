// This file is part of CORD – https://cord.network

// Copyright (C) Dhiway Networks Pvt. Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later

// CORD is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// CORD is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with CORD. If not, see <https://www.gnu.org/licenses/>.

//! Code related to benchmarking a node.

use crate::service::{Chain, FullClient};
use cord_primitives::AccountId;
use sc_cli::Result;
use sc_client_api::UsageProvider;
use sp_inherents::{InherentData, InherentDataProvider};
use sp_keyring::Sr25519Keyring;
use sp_runtime::OpaqueExtrinsic;

use std::{sync::Arc, time::Duration};

macro_rules! identify_chain {
	(
		$chain:expr,
		$nonce:ident,
		$current_block:ident,
		$period:ident,
		$genesis:ident,
		$signer:ident,
		$generic_code:expr $(,)*
	) => {
		match $chain {
			Chain::Braid => {
				#[cfg(feature = "braid-native")]
				{
					use cord_braid_runtime as runtime;

					let call = $generic_code;

					Ok(braid_sign_call(call, $nonce, $current_block, $period, $genesis, $signer))
				}

				#[cfg(not(feature = "braid-native"))]
				{
					Err("`braid-native` feature not enabled")
				}
			},
			Chain::Loom => {
				#[cfg(feature = "loom-native")]
				{
					use cord_loom_runtime as runtime;

					let call = $generic_code;

					Ok(loom_sign_call(call, $nonce, $current_block, $period, $genesis, $signer))
				}

				#[cfg(not(feature = "loom-native"))]
				{
					Err("`loom-native` feature not enabled")
				}
			},
			Chain::Weave => {
				#[cfg(feature = "weave-native")]
				{
					use cord_weave_runtime as runtime;

					let call = $generic_code;

					Ok(weave_sign_call(call, $nonce, $current_block, $period, $genesis, $signer))
				}

				#[cfg(not(feature = "weave-native"))]
				{
					Err("`weave-native` feature not enabled")
				}
			},
			Chain::Unknown => {
				let _ = $nonce;
				let _ = $current_block;
				let _ = $period;
				let _ = $genesis;
				let _ = $signer;

				Err("Unknown chain")
			},
		}
	};
}

// Generates `System::Remark` extrinsics for the benchmarks.
///
/// Note: Should only be used for benchmarking.
pub struct RemarkBuilder {
	client: Arc<FullClient>,
	chain: Chain,
}

impl RemarkBuilder {
	/// Creates a new [`Self`] from the given client.
	pub fn new(client: Arc<FullClient>, chain: Chain) -> Self {
		Self { client, chain }
	}
}

impl frame_benchmarking_cli::ExtrinsicBuilder for RemarkBuilder {
	fn pallet(&self) -> &str {
		"system"
	}

	fn extrinsic(&self) -> &str {
		"remark"
	}

	fn build(&self, nonce: u32) -> std::result::Result<OpaqueExtrinsic, &'static str> {
		// We apply the extrinsic directly, so let's take some random period.
		let period = 128;
		let genesis = self.client.usage_info().chain.best_hash;
		let signer = Sr25519Keyring::Bob.pair();
		let current_block = 0;

		identify_chain! {
			self.chain,
			nonce,
			current_block,
			period,
			genesis,
			signer,
			{
				runtime::RuntimeCall::System(
					runtime::SystemCall::remark { remark: vec![] }
				)
			},
		}
	}
}

/// Generates `Balances::TransferKeepAlive` extrinsics for the benchmarks.
///
/// Note: Should only be used for benchmarking.
pub struct TransferKeepAliveBuilder {
	client: Arc<FullClient>,
	dest: AccountId,
	chain: Chain,
}

impl TransferKeepAliveBuilder {
	/// Creates a new [`Self`] from the given client.
	pub fn new(client: Arc<FullClient>, dest: AccountId, chain: Chain) -> Self {
		Self { client, dest, chain }
	}
}

impl frame_benchmarking_cli::ExtrinsicBuilder for TransferKeepAliveBuilder {
	fn pallet(&self) -> &str {
		"balances"
	}

	fn extrinsic(&self) -> &str {
		"transfer_keep_alive"
	}

	fn build(&self, nonce: u32) -> std::result::Result<OpaqueExtrinsic, &'static str> {
		let signer = Sr25519Keyring::Bob.pair();
		// We apply the extrinsic directly, so let's take some random period.
		let period = 128;
		let genesis = self.client.usage_info().chain.best_hash;
		let current_block = 0;
		let _dest = self.dest.clone();

		identify_chain! {
			self.chain,
			nonce,
			current_block,
			period,
			genesis,
			signer,
			{
				runtime::RuntimeCall::Balances(runtime::BalancesCall::transfer_keep_alive {
					dest: _dest.into(),
					value: runtime::ExistentialDeposit::get(),
				})
			},
		}
	}
}

#[cfg(feature = "braid-native")]
fn braid_sign_call(
	call: cord_braid_runtime::RuntimeCall,
	nonce: u32,
	current_block: u64,
	period: u64,
	genesis: sp_core::H256,
	acc: sp_core::sr25519::Pair,
) -> OpaqueExtrinsic {
	use codec::Encode;
	use cord_braid_runtime as runtime;
	use sp_core::Pair;

	let extra: runtime::SignedExtra = (
		pallet_blacklist_members::CheckMemberBlacklist::<runtime::Runtime>::new(),
		frame_system::CheckNonZeroSender::<runtime::Runtime>::new(),
		frame_system::CheckSpecVersion::<runtime::Runtime>::new(),
		frame_system::CheckTxVersion::<runtime::Runtime>::new(),
		frame_system::CheckGenesis::<runtime::Runtime>::new(),
		frame_system::CheckMortality::<runtime::Runtime>::from(sp_runtime::generic::Era::mortal(
			period,
			current_block,
		)),
		frame_system::CheckNonce::<runtime::Runtime>::from(nonce),
		frame_system::CheckWeight::<runtime::Runtime>::new(),
		pallet_transaction_payment::ChargeTransactionPayment::<runtime::Runtime>::from(0),
	);

	let payload = runtime::SignedPayload::from_raw(
		call.clone(),
		extra.clone(),
		(
			(),
			(),
			runtime::VERSION.spec_version,
			runtime::VERSION.transaction_version,
			genesis,
			genesis,
			(),
			(),
			(),
		),
	);

	let signature = payload.using_encoded(|p| acc.sign(p));
	runtime::UncheckedExtrinsic::new_signed(
		call,
		sp_runtime::AccountId32::from(acc.public()).into(),
		cord_primitives::Signature::Sr25519(signature),
		extra,
	)
	.into()
}

#[cfg(feature = "loom-native")]
fn loom_sign_call(
	call: cord_loom_runtime::RuntimeCall,
	nonce: u32,
	current_block: u64,
	period: u64,
	genesis: sp_core::H256,
	acc: sp_core::sr25519::Pair,
) -> OpaqueExtrinsic {
	use codec::Encode;
	use cord_loom_runtime as runtime;
	use sp_core::Pair;

	let extra: runtime::SignedExtra = (
		pallet_network_membership::CheckNetworkMembership::<runtime::Runtime>::new(),
		frame_system::CheckNonZeroSender::<runtime::Runtime>::new(),
		frame_system::CheckSpecVersion::<runtime::Runtime>::new(),
		frame_system::CheckTxVersion::<runtime::Runtime>::new(),
		frame_system::CheckGenesis::<runtime::Runtime>::new(),
		frame_system::CheckMortality::<runtime::Runtime>::from(sp_runtime::generic::Era::mortal(
			period,
			current_block,
		)),
		frame_system::CheckNonce::<runtime::Runtime>::from(nonce),
		frame_system::CheckWeight::<runtime::Runtime>::new(),
		pallet_transaction_payment::ChargeTransactionPayment::<runtime::Runtime>::from(0),
	);

	let payload = runtime::SignedPayload::from_raw(
		call.clone(),
		extra.clone(),
		(
			(),
			(),
			runtime::VERSION.spec_version,
			runtime::VERSION.transaction_version,
			genesis,
			genesis,
			(),
			(),
			(),
		),
	);

	let signature = payload.using_encoded(|p| acc.sign(p));
	runtime::UncheckedExtrinsic::new_signed(
		call,
		sp_runtime::AccountId32::from(acc.public()).into(),
		cord_primitives::Signature::Sr25519(signature),
		extra,
	)
	.into()
}

#[cfg(feature = "weave-native")]
fn weave_sign_call(
	call: cord_weave_runtime::RuntimeCall,
	nonce: u32,
	current_block: u64,
	period: u64,
	genesis: sp_core::H256,
	acc: sp_core::sr25519::Pair,
) -> OpaqueExtrinsic {
	use codec::Encode;
	use cord_weave_runtime as runtime;
	use sp_core::Pair;

	let extra: runtime::SignedExtra = (
		pallet_network_membership::CheckNetworkMembership::<runtime::Runtime>::new(),
		frame_system::CheckNonZeroSender::<runtime::Runtime>::new(),
		frame_system::CheckSpecVersion::<runtime::Runtime>::new(),
		frame_system::CheckTxVersion::<runtime::Runtime>::new(),
		frame_system::CheckGenesis::<runtime::Runtime>::new(),
		frame_system::CheckMortality::<runtime::Runtime>::from(sp_runtime::generic::Era::mortal(
			period,
			current_block,
		)),
		frame_system::CheckNonce::<runtime::Runtime>::from(nonce),
		frame_system::CheckWeight::<runtime::Runtime>::new(),
		pallet_transaction_payment::ChargeTransactionPayment::<runtime::Runtime>::from(0),
	);

	let payload = runtime::SignedPayload::from_raw(
		call.clone(),
		extra.clone(),
		(
			(),
			(),
			runtime::VERSION.spec_version,
			runtime::VERSION.transaction_version,
			genesis,
			genesis,
			(),
			(),
			(),
		),
	);

	let signature = payload.using_encoded(|p| acc.sign(p));
	runtime::UncheckedExtrinsic::new_signed(
		call,
		sp_runtime::AccountId32::from(acc.public()).into(),
		cord_primitives::Signature::Sr25519(signature),
		extra,
	)
	.into()
}

/// Generates inherent data for the `benchmark overhead` command.
pub fn inherent_benchmark_data() -> Result<InherentData> {
	let mut inherent_data = InherentData::new();
	let d = Duration::from_millis(0);
	let timestamp = sp_timestamp::InherentDataProvider::new(d.into());

	futures::executor::block_on(timestamp.provide_inherent_data(&mut inherent_data))
		.map_err(|e| format!("creating inherent data: {:?}", e))?;
	Ok(inherent_data)
}
