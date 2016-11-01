/* RemoveRemove Duplicates from Sorted Array II

Follow up for ”Remove Duplicates”: What if duplicates are allowed at most twice?

For example, Given sorted array A = [1,1,1,2,2,3],

Your function should return length = 5, and A is now [1,1,2,2,3]

*/

#include <iostream>
#include <vector>
#include <iterator>             // std::distance
#include <algorithm>            // std::unique

using namespace std;

int removeDuplicates(vector<int>& nums){
    if (nums.size() < 2)
        return nums.size();
    else {
        int index = 2, occur = 1;
        for (int i = 3; i < nums.size(); i++) {
            if (nums[i] != nums[index]) {
                occur = 1;
                nums[index++] = nums[i];
            } else if (occur < 2) {
                occur++;
                nums[index++] = nums[i];
            } /* else  */
        }
        return index;
    }
}

int removeReplicates(vector<int>& nums, int rep) {
    const int N = nums.size();
    if (N < rep)
        return N;
    else {
        int index = 1, occur = 1;
        for (int i = 1; i < N; i++) {

            /* NOW
             + nums[index-1] is the last of preserved elements (the actor)
             + nums[index] is the place to put new actor
             + nums[i] is the potential new element (the candidate)
            
             IF actor != candidate:
             - candidate gets pushed into preserved elements
             - candidate becomes the new actor
             - update the occur to be denoting the actor, i.e. 1 */
            if (nums[index-1] != nums[i]) {
                nums[index] = nums[i];
                occur = 1;
                index++;
            }
            /*
             IF actor == candidate:
             - test whether occur arrives the threshold
             - IF not,
             - - candidate gets pushed
             - - candidate becomes the new actor (trivial)
             - - update the occur with increment
            */
            else if (occur < rep) {
                nums[index] = nums[i];
                occur++;
                index++;
            }
            /* INV: 
               index < i
               // since the total increment times of index is no more than i
               index + 1 <= i
             */
        }
        return index;
    }
}

int main()
{
    // vector<int> nums = {1, 1, 1, 2, 3, 3, 4, 4, 4, 5};
    vector<int> nums = {1, 1, 1, 1, 2, 3, 3, 4, 4, 4, 4, 5};
    // int up = removeReplicates(nums, 2);
    int up = removeReplicates(nums, 3);
    for(int i = 0; i < up; i++)
        cout << nums[i] << " ";
    return 0;
}
