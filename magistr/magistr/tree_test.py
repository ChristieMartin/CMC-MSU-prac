from collections import deque

# Define a simple class for nodes in the tree
class TreeNode:
    def __init__(self, name):
        self.name = name
        self.connectedWords = []

    def add_child(self, child):
        self.connectedWords.append(child)

# Function to find the position of a word in the tree's tier
def find_word_position(root, word):
    queue = deque([(root, 1)])

    current_tier = 1
    position_in_tier = 0
    
    tier_positions = {}

    while queue:
        current_node, tier = queue.popleft()
        
        if tier > current_tier:
            current_tier = tier
            position_in_tier = 0

        position_in_tier += 1
        
        tier_positions[current_node.name] = position_in_tier
        
        if current_node.name == word:
            return tier, tier_positions[word]
        
        for child in current_node.connectedWords:
            queue.append((child, tier + 1))
    
    return None, None

# Example usage:
# Construct the tree as per the image
root = TreeNode("A_1")
A_2 = TreeNode("A_2")
A_5 = TreeNode("A_5")
A_3 = TreeNode("A_3")
A_4 = TreeNode("A_4")
A_6 = TreeNode("A_6")

root.add_child(A_2)
root.add_child(A_5)
A_2.add_child(A_3)
A_2.add_child(A_4)
A_5.add_child(A_6)

# Find the position of 'A_6' in the tree
tier, position = find_word_position(root, "A_2")
print(f"The word 'A_6' is at tier {tier}, position {position}.")
