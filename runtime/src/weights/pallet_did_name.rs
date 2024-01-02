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

//! Autogenerated weights for `pallet_did_name`
//!
//! THIS FILE WAS AUTO-GENERATED USING THE SUBSTRATE BENCHMARK CLI VERSION 4.0.0-dev
//! DATE: 2024-01-02, STEPS: `50`, REPEAT: `20`, LOW RANGE: `[]`, HIGH RANGE: `[]`
//! WORST CASE MAP SIZE: `1000000`
//! HOSTNAME: `smohan-dev-host`, CPU: `Intel(R) Xeon(R) CPU @ 2.20GHz`
//! WASM-EXECUTION: `Compiled`, CHAIN: `Some("dev")`, DB CACHE: 1024

// Executed Command:
// ./target/production/cord
// benchmark
// pallet
// --chain=dev
// --steps=50
// --repeat=20
// --pallet=pallet_did_name
// --extrinsic=*
// --wasm-execution=compiled
// --heap-pages=4096
// --header=./HEADER-GPL3
// --output=./runtime/src/weights/

#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(missing_docs)]

use frame_support::{traits::Get, weights::Weight};
use core::marker::PhantomData;

/// Weight functions for `pallet_did_name`.
pub struct WeightInfo<T>(PhantomData<T>);
impl<T: frame_system::Config> pallet_did_name::WeightInfo for WeightInfo<T> {
	/// Storage: `DidName::Names` (r:1 w:1)
	/// Proof: `DidName::Names` (`max_values`: None, `max_size`: Some(114), added: 2589, mode: `MaxEncodedLen`)
	/// Storage: `DidName::Owner` (r:1 w:1)
	/// Proof: `DidName::Owner` (`max_values`: None, `max_size`: Some(118), added: 2593, mode: `MaxEncodedLen`)
	/// Storage: `DidName::Banned` (r:1 w:0)
	/// Proof: `DidName::Banned` (`max_values`: None, `max_size`: Some(82), added: 2557, mode: `MaxEncodedLen`)
	/// The range of component `n` is `[13, 64]`.
	fn register(n: u32, ) -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `3`
		//  Estimated: `3583`
		// Minimum execution time: 22_432_000 picoseconds.
		Weight::from_parts(23_230_002, 0)
			.saturating_add(Weight::from_parts(0, 3583))
			// Standard Error: 1_055
			.saturating_add(Weight::from_parts(15_629, 0).saturating_mul(n.into()))
			.saturating_add(T::DbWeight::get().reads(3))
			.saturating_add(T::DbWeight::get().writes(2))
	}
	/// Storage: `DidName::Names` (r:1 w:1)
	/// Proof: `DidName::Names` (`max_values`: None, `max_size`: Some(114), added: 2589, mode: `MaxEncodedLen`)
	/// Storage: `DidName::Owner` (r:1 w:1)
	/// Proof: `DidName::Owner` (`max_values`: None, `max_size`: Some(118), added: 2593, mode: `MaxEncodedLen`)
	fn release() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `275`
		//  Estimated: `3583`
		// Minimum execution time: 24_539_000 picoseconds.
		Weight::from_parts(25_492_000, 0)
			.saturating_add(Weight::from_parts(0, 3583))
			.saturating_add(T::DbWeight::get().reads(2))
			.saturating_add(T::DbWeight::get().writes(2))
	}
	/// Storage: `DidName::Banned` (r:1 w:1)
	/// Proof: `DidName::Banned` (`max_values`: None, `max_size`: Some(82), added: 2557, mode: `MaxEncodedLen`)
	/// Storage: `DidName::Owner` (r:1 w:1)
	/// Proof: `DidName::Owner` (`max_values`: None, `max_size`: Some(118), added: 2593, mode: `MaxEncodedLen`)
	/// Storage: `DidName::Names` (r:0 w:1)
	/// Proof: `DidName::Names` (`max_values`: None, `max_size`: Some(114), added: 2589, mode: `MaxEncodedLen`)
	/// The range of component `n` is `[13, 64]`.
	fn ban(n: u32, ) -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `113 + n * (1 ±0)`
		//  Estimated: `3583`
		// Minimum execution time: 25_614_000 picoseconds.
		Weight::from_parts(26_595_889, 0)
			.saturating_add(Weight::from_parts(0, 3583))
			// Standard Error: 1_199
			.saturating_add(Weight::from_parts(34_863, 0).saturating_mul(n.into()))
			.saturating_add(T::DbWeight::get().reads(2))
			.saturating_add(T::DbWeight::get().writes(3))
	}
	/// Storage: `DidName::Banned` (r:1 w:1)
	/// Proof: `DidName::Banned` (`max_values`: None, `max_size`: Some(82), added: 2557, mode: `MaxEncodedLen`)
	/// The range of component `n` is `[13, 64]`.
	fn unban(n: u32, ) -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `41 + n * (1 ±0)`
		//  Estimated: `3547`
		// Minimum execution time: 15_823_000 picoseconds.
		Weight::from_parts(16_195_565, 0)
			.saturating_add(Weight::from_parts(0, 3547))
			// Standard Error: 545
			.saturating_add(Weight::from_parts(30_309, 0).saturating_mul(n.into()))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
}
