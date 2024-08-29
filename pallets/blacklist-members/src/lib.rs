// CORD Blockchain – https://dhiway.network
// Copyright (C) Dhiway Networks Pvt. Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

//! # Accounts/Members Blacklisting Management
#![warn(unused_extern_crates)]
#![cfg_attr(not(feature = "std"), no_std)]

use codec::{Decode, Encode};
use frame_support::dispatch::DispatchInfo;
pub use pallet::*;

use sp_runtime::{
	traits::{DispatchInfoOf, Dispatchable, SignedExtension},
	transaction_validity::{
		InvalidTransaction, TransactionLongevity, TransactionValidity, TransactionValidityError,
		ValidTransaction,
	},
};
use sp_std::{marker::PhantomData, prelude::*};

pub use frame_system::WeightInfo;

#[frame_support::pallet]
pub mod pallet {
	use super::*;
	use frame_support::{pallet_prelude::*, traits::StorageVersion};
	use frame_system::pallet_prelude::*;

	/// The current storage version.
	const STORAGE_VERSION: StorageVersion = StorageVersion::new(1);

	pub(crate) type CordAccountOf<T> = <T as frame_system::Config>::AccountId;

	#[pallet::config]
	pub trait Config: frame_system::Config {
		type BlackListOrigin: EnsureOrigin<Self::RuntimeOrigin>;

		type RuntimeEvent: From<Event<Self>> + IsType<<Self as frame_system::Config>::RuntimeEvent>;

		type WeightInfo: WeightInfo;
	}

	#[pallet::pallet]
	#[pallet::storage_version(STORAGE_VERSION)]
	pub struct Pallet<T>(_);

	#[pallet::storage]
	pub type BlackListedMembers<T: Config> = StorageMap<_, Blake2_128Concat, CordAccountOf<T>, ()>;

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		MemberAddedToBlacklist { member: CordAccountOf<T> },
		MemberRemovedFromBlacklist { member: CordAccountOf<T> },
	}

	#[pallet::error]
	pub enum Error<T> {
		/// Member has been already blacklisted,
		MemberAlreadyBlacklisted,
		/// Member not found in the blacklist.
		MemberNotFoundInBlacklist,
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		#[pallet::call_index(0)]
		#[pallet::weight({0})]
		pub fn add_to_blacklist(origin: OriginFor<T>, member: CordAccountOf<T>) -> DispatchResult {
			T::BlackListOrigin::ensure_origin(origin)?;

			ensure!(
				!<BlackListedMembers<T>>::contains_key(&member),
				Error::<T>::MemberAlreadyBlacklisted
			);

			Self::add_member_to_blacklist(&member);

			Self::deposit_event(Event::MemberAddedToBlacklist { member });

			Ok(())
		}

		#[pallet::call_index(1)]
		#[pallet::weight({0})]
		pub fn remove_from_blacklist(
			origin: OriginFor<T>,
			member: CordAccountOf<T>,
		) -> DispatchResult {
			T::BlackListOrigin::ensure_origin(origin)?;

			ensure!(
				<BlackListedMembers<T>>::contains_key(&member),
				Error::<T>::MemberNotFoundInBlacklist
			);

			Self::remove_member_from_blacklist(&member);

			Self::deposit_event(Event::MemberRemovedFromBlacklist { member });

			Ok(())
		}
	}
}

impl<T: Config> Pallet<T> {
	pub fn add_member_to_blacklist(member: &CordAccountOf<T>) {
		<BlackListedMembers<T>>::insert(member, ());
	}

	pub fn remove_member_from_blacklist(member: &CordAccountOf<T>) {
		<BlackListedMembers<T>>::remove(member);
	}
}

/// The `CheckMemberBlacklist` struct.
#[derive(Encode, Decode, Clone, Eq, PartialEq, Default, scale_info::TypeInfo)]
#[scale_info(skip_type_params(T))]
pub struct CheckMemberBlacklist<T: Config + Send + Sync>(PhantomData<T>);

impl<T: Config + Send + Sync> sp_std::fmt::Debug for CheckMemberBlacklist<T> {
	#[cfg(feature = "std")]
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "CheckMemberBlacklist")
	}

	#[cfg(not(feature = "std"))]
	fn fmt(&self, _: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		Ok(())
	}
}

impl<T: Config + Send + Sync> CheckMemberBlacklist<T> {
	/// Create new `SignedExtension` to check author permission.
	pub fn new() -> Self {
		Self(sp_std::marker::PhantomData)
	}
}

/// Implementation of the `SignedExtension` trait for the
/// `CheckMemberBlacklist` struct.
impl<T: Config + Send + Sync> SignedExtension for CheckMemberBlacklist<T>
where
	T::RuntimeCall: Dispatchable<Info = DispatchInfo>,
{
	type AccountId = T::AccountId;
	type Call = T::RuntimeCall;
	type AdditionalSigned = ();
	type Pre = ();
	const IDENTIFIER: &'static str = "CheckMemberBlacklist";

	fn additional_signed(&self) -> sp_std::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> Result<Self::Pre, TransactionValidityError> {
		self.validate(who, call, info, len).map(|_| ())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		_call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> TransactionValidity {
		if !<BlackListedMembers<T>>::contains_key(who) {
			Ok(ValidTransaction {
				priority: 0,
				longevity: TransactionLongevity::max_value(),
				propagate: true,
				..Default::default()
			})
		} else {
			Err(InvalidTransaction::Call.into())
		}
	}
}
