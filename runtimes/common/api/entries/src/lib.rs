// This file is part of CORD – https://cord.network

// Copyright (C) 2019-2023 BOTLabs GmbH.
// Copyright (C) Dhiway Networks Pvt. Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later
// Adapted to meet the requirements of the CORD project.

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

//! Runtime API definition for Registry Entries.

#![cfg_attr(not(feature = "std"), no_std)]

// use codec::Codec;
use sp_std::vec::Vec;
use pallet_entries::RegistryEntryIdOf;

sp_api::decl_runtime_apis! {
    #[api_version(1)]
    pub trait EntriesApi {
        
        /// Retrieves all the identifiers of Registry Entries
        fn retrieve_all_identifiers() -> Vec<RegistryEntryIdOf>;
        
        /// Checks if the input registry entry identifiers exists or not
        fn does_identifier_exists(identifier: RegistryEntryIdOf) -> bool;
    }
}
