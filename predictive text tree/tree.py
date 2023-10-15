"""
6.1010 Spring '23
"""

import doctest
from text_tokenize import tokenize_sentences


class PrefixTree:
    """
    Implements tree that stores an associative array
    """
    def __init__(self):
        self.value = None
        self.children = {}

    def __setitem__(self, key, value):
        """
        Add a key with the given value to the prefix tree,
        or reassign the associated value if it is already present.
        Raise a TypeError if the given key is not a string.
        """
        # current = self
        if not isinstance(key, str):
            raise TypeError
        if len(key) == 0:
            self.value = value
        else:
            if key[0] in self.children:
                next_child = self.children[key[0]]
                next_child[key[1:]] = value
            else:
                self.children[key[0]] = PrefixTree()
                self.children[key[0]][key[1:]] = value

    def __getitem__(self, key):
        """
        Return the value for the specified prefix.
        Raise a KeyError if the given key is not in the prefix tree.
        Raise a TypeError if the given key is not a string.
        """
        if not isinstance(key, str):
            raise TypeError
        if len(key) == 0:
            return self.value
        if key[0] in self.children:
            next_child = self.children[key[0]]
            return next_child[key[1:]]
        else:
            raise KeyError

    def __delitem__(self, key):
        """
        Delete the given key from the prefix tree if it exists.
        Raise a KeyError if the given key is not in the prefix tree.
        Raise a TypeError if the given key is not a string.
        """
        if not isinstance(key, str):
            raise TypeError
        if key not in self:
            raise KeyError
        self[key] = None

    def __contains__(self, key):
        """
        Is key a key in the prefix tree?  Return True or False.
        Raise a TypeError if the given key is not a string.
        """
        # print(key, "key")
        if not isinstance(key, str):
            raise TypeError
        if len(key) == 0:
            if self.value is None:
                return False
            return True
        else:
            if key[0] not in self.children:
                return False
            next_child = self.children[key[0]]
            return key[1:] in next_child

    def __iter__(self):
        """
        Generator of (key, value) pairs for all keys/values in this prefix tree
        and its children. Must be a generator!
        """
        if self.value is not None:
            yield ("", self.value)
        for child, tree in self.children.items():
            for key, val in tree.__iter__():
                yield (child + key, val)


def word_frequencies(text):
    """
    Given a piece of text as a single string, create a prefix tree whose keys
    are the words in the text, and whose values are the number of times the
    associated word appears in the text.
    """
    tree = PrefixTree()
    sentences = tokenize_sentences(text)
    # this is all wrong
    for sentence in sentences:
        for word in sentence.split():
            if word not in tree:
                tree[word] = 1
            else:
                tree[word] = tree[word] + 1
    return tree


def get_starting_node(tree, prefix):
    """
    Gets starting node for a given prefix
    """
    if len(prefix) == 0:
        tuples = list(tree)
        return tuples
    
    if prefix[0] in tree.children:
        next_child = tree.children[prefix[0]]
        return get_starting_node(next_child, prefix[1:])
    else:
        return []

def autocomplete(tree, prefix, max_count=None):
    """
    Return the list of the most-frequently occurring elements that start with
    the given prefix.  Include only the top max_count elements if max_count is
    specified, otherwise return all.

    Raise a TypeError if the given prefix is not a string.
    """
    if not isinstance(prefix, str):
        raise TypeError
    words = get_starting_node(tree, prefix)
    if words == []:
        return []

    word_list = [(prefix + word, freq) for word, freq in words]
    word_list.sort(key=lambda x: x[1], reverse=True)

    if max_count is None:
        return [key for key, value in word_list]
    return [key for key, value in word_list[:max_count]]


def gets_edits(prefix):
    """
    Gets all edits made possible by single-character insertion, deletion,
    replacement and two-character transpose
    """
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    words = set()
    for letter in alphabet:
        for c_index in range(len(prefix)):
            # replace single character
            words.add(prefix[:c_index] + letter + prefix[c_index + 1 :])
            # adds character in the range "a" to "z" at any place in the word
            words.add(prefix[0:c_index] + letter + prefix[c_index:])
            # deletes single character
            words.add(prefix[:c_index] + prefix[c_index + 1 :])
    # switch the positions of any two adjacent characters in the word
    for i in range(len(prefix) - 1):
        words.add(prefix[:i] + prefix[i + 1] + prefix[i] + prefix[i + 2 :])
    return words


def autocorrect(tree, prefix, max_count=None):
    """
    Return the list of the most-frequent words that start with prefix or that
    are valid words that differ from prefix by a small edit.  Include up to
    max_count elements from the autocompletion.  If autocompletion produces
    fewer than max_count elements, include the most-frequently-occurring valid
    edits of the given word as well, up to max_count total elements.
    """
    auto_res = autocomplete(tree, prefix, max_count)
    #print(auto_res)
    #print(max_count, "max")
    #print(len(auto_res), "auto")
    edits = gets_edits(prefix)
    valid_edits = [
        (edit, tree[edit]) for edit in edits if edit in tree and edit not in auto_res
    ]
    valid_edits.sort(key=lambda x: x[1], reverse=True)
    edit_keys = [key for key, value in valid_edits]
    if max_count is None:
        return auto_res + edit_keys
    elif len(auto_res) < max_count:
        extra_vals = max_count - len(auto_res)
        return auto_res + edit_keys[:extra_vals]
    else:
        return auto_res

def recursive_build(tree, pattern, list_vals):
    """
    Recusive helper for word_filter. Adds support for
    some frequently used regex characters.
    """
    if len(pattern) == 0:
        if tree.value != None:
            return [(pattern, tree.value)]
        return []
    elif pattern[0] == "?":
        # add smth
        temp = []
        for key, child in tree.children.items():
            next = tree.children[key]
            curr_list = recursive_build(next, pattern[1:], list_vals)
            new = [(key + first, val) for first, val in curr_list]
            temp.extend(new)
        return temp
    
    elif pattern[0] == "*":
        # add smth
        temp2 = []
        ignore = recursive_build(tree, pattern[1:], list_vals)
        for key, child in tree.children.items():
            next = tree.children[key]
            curr_list = recursive_build(next, pattern, list_vals)
            new = [(key + first, val) for first, val in curr_list]
            temp2.extend(new)
        return temp2 + ignore
    else:
        if pattern[0] not in tree.children:
            return []
        next = tree.children[pattern[0]]
        curr_list = recursive_build(next, pattern[1:], list_vals)
        return [(pattern[0] + first, val) for first, val in curr_list]


def word_filter(tree, pattern):
    """
    Return list of (word, freq) for all words in the given prefix tree that
    match pattern.  pattern is a string, interpreted as explained below:
         * matches any sequence of zero or more characters,
         ? matches any single character,
         otherwise char in pattern char must equal char in word.
    """
    result = set(recursive_build(tree, pattern, []))
    return list(result)
    


# you can include test cases of your own in the block below.
if __name__ == "__main__":
    doctest.testmod()
    #t = PrefixTree()
    #node1 = PrefixTree()
    #node1.value = "b"

    #t["bat"] = 2
    #t["bar"] = 1
    #print(get_starting_node(t, "ba"))
    #print(recursive_build(t, "bar", []))
    with open("/Users/mjdelg/Downloads/98-0.txt", encoding="utf-8") as f:
        text = f.read()
        tree = word_frequencies(text)
        #print(word_filter(tree, "r?c*t"))
