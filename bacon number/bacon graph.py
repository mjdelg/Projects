"""
6.1010 Spring '23 Lab 3: Bacon Number
"""
#!/usr/bin/env python3

import pickle

# NO ADDITIONAL IMPORTS ALLOWED!
# ––––––––––––––––––––––––TRANSFORM DATA–––––––––––––––––––––––––#
def transform_data(raw_data):
    """
    Given list of three element tuples, adds in each actor ID as key
    in dictionary and actors who they've acted with as values
    """
    new_data = {"movies" : {}}
    for actor in raw_data:
        if actor[0] not in new_data:
            new_data[actor[0]] = {actor[1]}
        else:
            new_data[actor[0]].add(actor[1])
        # make sure actors is a symmetric relationship
        if actor[1] not in new_data:
            new_data[actor[1]] = {actor[0]}
        else:
            new_data[actor[1]].add(actor[0])
        if actor[-1] not in new_data["movies"]:
            new_data["movies"][actor[-1]] = {actor[0], actor[1]}
        else:
            new_data["movies"][actor[-1]].add(actor[0])
            new_data["movies"][actor[-1]].add(actor[1])
    return new_data
# ––––––––––––––––––––––––ACTED TOGETHER–––––––––––––––––––––––––#
def acted_together(transformed_data, actor_id_1, actor_id_2):
    """
    Returns true if two actors have acted with each other
    """
    if actor_id_1 == actor_id_2:
        return True
    for actor in transformed_data[actor_id_1]:
        if actor_id_2 == actor:
            return True
    return False
# –––––––––––––––––––––ACTORS WITH BACON NUM–––––––––––––––––––––#
def actors_with_bacon_number(transformed_data, n):
    """
    Returns set containing all actors who have n as their bacon
    number
    """
    bacon_num = 4724
    #bacon_list = [key for key in transformed_data
    #             for actor in transformed_data[key]
    #              if key != "movies" if bacon_num in actor]
    bacon_set = {bacon_num}
    visited = {bacon_num}
    for i in range(n):
            # if actor is among actors contained in previous node and not Kevin Bacon
        bacon_list = []
        for actor in bacon_set:
            for costar in transformed_data[actor]:
                print(actor, costar)
                if costar not in visited:
                    print("hi")
                    bacon_list.append(costar)
        #bacon_list = [actor for i in range(len(bacon_list))
        #              for actor in transformed_data[bacon_list[i]]
        #              if actor[0] not in visited and actor[0] != bacon_num]
        visited.update(bacon_list)
        bacon_set = set(bacon_list)
        if bacon_list == []:
            return set()
    return set(bacon_list)
# ––––––––––––––––––––––––BACON PATH–––––––––––––––––––––––––#
def bacon_path(transformed_data, actor_id):
    """
    Finds shortest path connecting Kevin Bacon
    to a given actor
    """
    if actor_id not in transformed_data:
        return None
    bacon_num = 4724
    path = find_path(transformed_data, bacon_num, actor_id)
    return path
# ––––––––––––––––––––––––ACTOR TO ACTOR PATH–––––––––––––––––––––#
def actor_to_actor_path(transformed_data, actor_id_1, actor_id_2):
    """
    Finds shortest path connecting one actor to another
    """
    if actor_id_1 not in transformed_data or actor_id_2 not in transformed_data:
        return None
    if actor_id_1 == actor_id_2:
        return [actor_id_1]
    path = find_path(transformed_data, actor_id_1, actor_id_2)
    return path
# ––––––––––––––––––––––––––MOVIE PATH––––––––––––––––––––––––––––––#
def movie_path(transformed_data, actor_id_1, actor_id_2):
    """
    Returns path of movies that connect two actors
    """
    result = []
    my_actor_path = actor_to_actor_path(transformed_data, actor_id_1, actor_id_2)
    for i in range(len(my_actor_path) - 1):
        result += [tup[-1] for tup in transformed_data[my_actor_path[i]]
                   if my_actor_path[i + 1] in tup]
    return result
# –––––––––––––––––––––––––ACTOR PATH––––––––––––––––––––––––––––––#
def actor_path(transformed_data, actor_id_1, goal_test_function):
    """
    Generalized version of actor_to_actor_path.
    """
    if goal_test_function(actor_id_1):
        return [actor_id_1]
    path = find_path(transformed_data, actor_id_1, 0, goal_test_function)
    return path
#––––––––––––––––––––––––ACTORS CONNECTING FILMS––––––––––––––––––––––––#
def actors_connecting_films(transformed_data, movie1, movie2):
    """
    Returns list of actors connecting two films
    """
    #movie1_actors = {actor for actor in transformed_data for tuple in transformed_data[actor]
    #         if movie1 == tuple[-1]}
    #movie2_actors = {actor for actor in transformed_data for tuple in transformed_data[actor]
    #         if movie2 == tuple[-1]}
    movie1_actors = transformed_data["movies"][movie1]
    movie2_actors = transformed_data["movies"][movie2]
    comp_list = [0] * 2000
    for actors1 in movie1_actors:
        for actors2 in movie2_actors:
            path = find_path(transformed_data, actors1, actors2)
            if len(path) < len(comp_list):
                comp_list = path
    return comp_list
# ––––––––––––––––––––––––HELPER FUNCTIONS–––––––––––––––––––––––––#
def get_next_node(transformed_data, visited_set, current_actor, actor_id):
    """
    Helper function returns list of actors in the next node of adjacency
    dictionary
    """
    # adds actor to bacon list if actor wasn't in previous node and is not
    # equivalent to bacon_id
    next_val = [actor for actor in transformed_data[current_actor]
        if actor not in visited_set and actor != actor_id]
    return next_val

def find_path(transformed_data, actor1, actor2, goal_condition=0):
    """
    Finds shortest path between two actor by implementing a breadth first
    search
    """
    agenda = [(actor1,)]
    visited = {actor1, }
    while agenda:
        current_path = agenda.pop(0)
        terminal_state = current_path[-1]
        for actor in get_next_node(transformed_data, visited, terminal_state, actor1):
            if actor not in visited:
                new_path = current_path + (actor,)
                if goal_condition == 0:
                    if actor2 in new_path:
                        return list(new_path)
                else:
                    if goal_condition(actor):
                        return list(new_path)
                agenda.append(new_path)
                visited.add(actor)

# ––––––––––––––––––TESTING––––––––––––––––––––#
if __name__ == "__main__":
    with open("resources/small.pickle", "rb") as f:
        # small = pickle.load(f)
        # names = pickle.load(f)
        # tiny = pickle.load(f)
        # large = pickle.load(f)
        # movies = pickle.load(f)
        def get_key_list(my_dict, val_list):
            """
            Prints keys of given a values within a dict
            """
            result = [key for i in range(len(val_list)) 
                      for key, value in my_dict.items()
                      if val_list[i] == value]
            print(result)

    # print(actors_with_bacon_number(transform_data(large), 6))

    # print(actors_with_bacon_number(transform_data(tiny), 1))

    # –––––––––––––––ACTED TOGETHER DILLON AND SKARSGARD––––––––––––––––––#
    # print(acted_together(transform_data(small), 1640, 2876))
    # –––––––––––––––ACTED TOGETHER PENDERGAST AND CREVILLAN––––––––––––––#
    # print(acted_together(transform_data(small), 953997, 146634))

    # –––––––––––––––––––––ACTORS WITH BACON NUM 6––––––––––––––––––––––––#
    # print(actors_with_bacon_number(transform_data(large), 6))
    # get_key(names, 1367972)
    # get_key(names, 1338716)
    # get_key(names, 1345461)
    # get_key(names, 1345462)

    # –––––––––––––––––––––BACON PATH TO JOSEPH BEUYS––––––––––––––––––––––#
    # print(bacon_path(transform_data(large), 1307035))
    # get_key_list(names, [4724, 56614, 1844, 18400, 1037929, 1307035])

    # ––––––––––––––––ACTOR PATH FROM TOM LONDON TO TOM HULCE––––––––––––––#
    # print(actor_to_actor_path(transform_data(large), 932309, 3999))
    # get_key_list(names, [932309, 14664, 102327, 41214, 4765, 3999])

    # ––––––––––––––––––MOVIE PATH FROM WAGNER TO RADACIC––––––––––––––––––#
    # print(movie_path(transform_data(large), 9208, 1345461))
    # get_key_list(movies, [11422, 115201, 29938, 256690, 283406])

    # additional code here will be run only when lab.py is invoked directly
    # (not when imported from test.py), so this is a good place to put code
    # used, for example, to generate the results for the online questions.
    pass