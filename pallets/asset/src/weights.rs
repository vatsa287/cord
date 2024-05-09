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

//! Autogenerated weights for `pallet_asset`
//!
//! THIS FILE WAS AUTO-GENERATED USING THE SUBSTRATE BENCHMARK CLI VERSION 32.0.0
//! DATE: 2024-05-09, STEPS: `50`, REPEAT: `20`, LOW RANGE: `[]`, HIGH RANGE: `[]`
//! WORST CASE MAP SIZE: `1000000`
//! HOSTNAME: `cord-benchmark-8gb`, CPU: `AMD EPYC 7B12`
//! WASM-EXECUTION: `Compiled`, CHAIN: `Some("dev")`, DB CACHE: `1024`

// Executed Command:
// ./target/production/cord
// benchmark
// pallet
// --chain=dev
// --steps=50
// --repeat=20
// --pallet=pallet_asset
// --extrinsic=*
// --wasm-execution=compiled
// --heap-pages=4096
// --output=./pallets/asset/src/weights.rs
// --header=./HEADER-GPL3
// --template=./.maintain/frame-weight-template.hbs

#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(missing_docs)]

use frame_support::{traits::Get, weights::{Weight, constants::RocksDbWeight}};
use core::marker::PhantomData;

/// Weight functions needed for `pallet_asset`.
pub trait WeightInfo {
	fn create() -> Weight;
	fn issue() -> Weight;
	fn transfer() -> Weight;
	fn status_change() -> Weight;
}

/// Weights for `pallet_asset` using the CORD node and recommended hardware.
pub struct SubstrateWeight<T>(PhantomData<T>);
impl<T: frame_system::Config> WeightInfo for SubstrateWeight<T> {
	/// Storage: `ChainSpace::Authorizations` (r:1 w:0)
	/// Proof: `ChainSpace::Authorizations` (`max_values`: None, `max_size`: Some(184), added: 2659, mode: `MaxEncodedLen`)
	/// Storage: `ChainSpace::Spaces` (r:1 w:1)
	/// Proof: `ChainSpace::Spaces` (`max_values`: None, `max_size`: Some(206), added: 2681, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Assets` (r:1 w:1)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Asset::AssetLookup` (r:0 w:1)
	/// Proof: `Asset::AssetLookup` (`max_values`: None, `max_size`: Some(98), added: 2573, mode: `MaxEncodedLen`)
	fn create() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `719`
		//  Estimated: `6667`
		// Minimum execution time: 38_630_000 picoseconds.
		Weight::from_parts(39_750_000, 6667)
			.saturating_add(T::DbWeight::get().reads(4_u64))
			.saturating_add(T::DbWeight::get().writes(4_u64))
	}
	/// Storage: `ChainSpace::Authorizations` (r:1 w:0)
	/// Proof: `ChainSpace::Authorizations` (`max_values`: None, `max_size`: Some(184), added: 2659, mode: `MaxEncodedLen`)
	/// Storage: `ChainSpace::Spaces` (r:1 w:1)
	/// Proof: `ChainSpace::Spaces` (`max_values`: None, `max_size`: Some(206), added: 2681, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Assets` (r:1 w:1)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Distribution` (r:1 w:1)
	/// Proof: `Asset::Distribution` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:2 w:2)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:0 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Asset::AssetLookup` (r:0 w:1)
	/// Proof: `Asset::AssetLookup` (`max_values`: None, `max_size`: Some(98), added: 2573, mode: `MaxEncodedLen`)
	fn issue() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `968`
		//  Estimated: `6667`
		// Minimum execution time: 53_840_000 picoseconds.
		Weight::from_parts(54_980_000, 6667)
			.saturating_add(T::DbWeight::get().reads(6_u64))
			.saturating_add(T::DbWeight::get().writes(7_u64))
	}
	/// Storage: `Asset::Assets` (r:1 w:0)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:1 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	fn transfer() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `864`
		//  Estimated: `6799`
		// Minimum execution time: 30_990_000 picoseconds.
		Weight::from_parts(31_840_000, 6799)
			.saturating_add(T::DbWeight::get().reads(3_u64))
			.saturating_add(T::DbWeight::get().writes(2_u64))
	}
	/// Storage: `Asset::Assets` (r:1 w:0)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:1 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	fn status_change() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `864`
		//  Estimated: `6799`
		// Minimum execution time: 30_150_000 picoseconds.
		Weight::from_parts(31_440_000, 6799)
			.saturating_add(T::DbWeight::get().reads(3_u64))
			.saturating_add(T::DbWeight::get().writes(2_u64))
	}
}

// For backwards compatibility and tests.
impl WeightInfo for () {
	/// Storage: `ChainSpace::Authorizations` (r:1 w:0)
	/// Proof: `ChainSpace::Authorizations` (`max_values`: None, `max_size`: Some(184), added: 2659, mode: `MaxEncodedLen`)
	/// Storage: `ChainSpace::Spaces` (r:1 w:1)
	/// Proof: `ChainSpace::Spaces` (`max_values`: None, `max_size`: Some(206), added: 2681, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Assets` (r:1 w:1)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Asset::AssetLookup` (r:0 w:1)
	/// Proof: `Asset::AssetLookup` (`max_values`: None, `max_size`: Some(98), added: 2573, mode: `MaxEncodedLen`)
	fn create() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `719`
		//  Estimated: `6667`
		// Minimum execution time: 38_630_000 picoseconds.
		Weight::from_parts(39_750_000, 6667)
			.saturating_add(RocksDbWeight::get().reads(4_u64))
			.saturating_add(RocksDbWeight::get().writes(4_u64))
	}
	/// Storage: `ChainSpace::Authorizations` (r:1 w:0)
	/// Proof: `ChainSpace::Authorizations` (`max_values`: None, `max_size`: Some(184), added: 2659, mode: `MaxEncodedLen`)
	/// Storage: `ChainSpace::Spaces` (r:1 w:1)
	/// Proof: `ChainSpace::Spaces` (`max_values`: None, `max_size`: Some(206), added: 2681, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Assets` (r:1 w:1)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Distribution` (r:1 w:1)
	/// Proof: `Asset::Distribution` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:2 w:2)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:0 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Asset::AssetLookup` (r:0 w:1)
	/// Proof: `Asset::AssetLookup` (`max_values`: None, `max_size`: Some(98), added: 2573, mode: `MaxEncodedLen`)
	fn issue() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `968`
		//  Estimated: `6667`
		// Minimum execution time: 53_840_000 picoseconds.
		Weight::from_parts(54_980_000, 6667)
			.saturating_add(RocksDbWeight::get().reads(6_u64))
			.saturating_add(RocksDbWeight::get().writes(7_u64))
	}
	/// Storage: `Asset::Assets` (r:1 w:0)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:1 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	fn transfer() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `864`
		//  Estimated: `6799`
		// Minimum execution time: 30_990_000 picoseconds.
		Weight::from_parts(31_840_000, 6799)
			.saturating_add(RocksDbWeight::get().reads(3_u64))
			.saturating_add(RocksDbWeight::get().writes(2_u64))
	}
	/// Storage: `Asset::Assets` (r:1 w:0)
	/// Proof: `Asset::Assets` (`max_values`: None, `max_size`: Some(3202), added: 5677, mode: `MaxEncodedLen`)
	/// Storage: `Asset::Issuance` (r:1 w:1)
	/// Proof: `Asset::Issuance` (`max_values`: None, `max_size`: Some(3334), added: 5809, mode: `MaxEncodedLen`)
	/// Storage: `Identifier::Identifiers` (r:1 w:1)
	/// Proof: `Identifier::Identifiers` (`max_values`: None, `max_size`: Some(4294967295), added: 2474, mode: `MaxEncodedLen`)
	fn status_change() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `864`
		//  Estimated: `6799`
		// Minimum execution time: 30_150_000 picoseconds.
		Weight::from_parts(31_440_000, 6799)
			.saturating_add(RocksDbWeight::get().reads(3_u64))
			.saturating_add(RocksDbWeight::get().writes(2_u64))
	}
}
