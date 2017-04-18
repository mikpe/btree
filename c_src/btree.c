/*
 * File        : btree.c
 * Author      : Mikael Pettersson <mikael.pettersson@klarna.com>
 * Description : Erlang implementation of B-tree sets
 *
 * Copyright (c) 2017 Klarna AB
 *
 * This file is provided to you under the Apache License,
 * Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain
 * a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include "erl_nif.h"

static ERL_NIF_TERM btree_binsearch(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int vector_arity, item_arity, compare;
    int left, right, middle;
    const ERL_NIF_TERM *vector_array, *item_array;
    ERL_NIF_TERM search_key, item, item_key, tmp;

    /* argv[0] is the vector, which must be a tuple */
    if (!enif_get_tuple(env, argv[0], &vector_arity, &vector_array))
        return enif_make_badarg(env);

    search_key = argv[1];

    left = 0;
    right = vector_arity - 1;

    while (left <= right) {
        middle = (unsigned int)(left + right) / 2;
        item = vector_array[middle];

        /* each tuple element must be a tuple with arity >= 2 */
        if (!enif_get_tuple(env, item, &item_arity, &item_array)
            || item_arity < 2)
            return enif_make_badarg(env);

        item_key = item_array[0];
        compare = enif_compare(search_key, item_key);
        if (compare < 0) {
            right = middle - 1;
        } else if (compare == 0) {
            /* Found.  Adjust MIDDLE [0,N-1] to Erlang [1,N] and return it.  */
            return enif_make_int(env, middle + 1);
        } else {
            left = middle + 1;
        }
    }

    /* Not found.  Adjust RIGHT [-1,N-1] to Erlang [0,N] and return it wrapped in a CONS cell.
       The CDR of that cell is immaterial; NIL would be ideal, but constructing it involves
       calling a variadic function which is sub-optimal, so we store RIGHT there too.  */
    tmp = enif_make_int(env, right + 1);
    return enif_make_list_cell(env, tmp, tmp);
}

static ErlNifFunc btree_funcs[] = {
    { "binsearch", 2, btree_binsearch }
};

static int btree_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(btree, btree_funcs, /*load=*/NULL, /*reload=*/NULL, btree_upgrade, /*unload=*/NULL);
