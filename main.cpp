#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <string>

using namespace std;

// a game of live implemenation

/**
 * @param vector<vector<int> > field
 *  The field which should be printed
 * @return
 *  void
 */
void printOut(vector<vector<int> > field)
{
	const string NORMAL_SETTINGS = "\033[0m";
	
	// clear
	cout << "\033[2J";
	for(int x = 0; x < field.size(); x++)
	{
		for(int y = 0; y < field[x].size(); y++)
		{
			// write on position x / y
			if(field[x][y] == 1)
			{
				cout << "\033[" << y+1 << ";" << x+1 << "H" << 0;
			}
		}
	}
	cout << NORMAL_SETTINGS;
	cout << '\n';
}

/**
 * @param vector<vector<int> > field
 *  The field which should be new calculeted
 * @return
 *  Returns the next generatin of vector<vector<int> > field
*/
vector<vector<int> > calc(vector<vector<int> > field)
{
	const int DEAD = 0;
	const int ALIVE = 1;

	const int SIZE = field.size();
	const int BOX_SIZE = (SIZE > 3) ? SIZE : 3;
	vector<vector<int> > newField(SIZE, vector<int>(SIZE));
	
	for(int x = 0; x < field.size(); x++)
	{
		for(int y = 0; y < field.size(); y++)
		{
			int nOfN = 0;
			int cellState = field[x][y];
				
			// first row
			if(y - 1 >= 0)
			{
				if(x + 1 < BOX_SIZE)
				{
					nOfN += field[x+1][y-1];
				}
				
				if(x-1 >= 0)
				{
					nOfN += field[x-1][y-1];
				}

				nOfN += field[x][y-1];
			}
			
			// second row
			if(x + 1 < BOX_SIZE)
			{
				nOfN += field[x+1][y];
			}
			
			if(x-1 >= 0)
			{
				nOfN += field[x-1][y];
			}
			
			// third row
			if((y + 1) < BOX_SIZE)
			{
				if(x + 1 < BOX_SIZE)
				{
					nOfN += field[x+1][y+1];
				}
				
				if(x-1 >= 0)
				{
					nOfN += field[x-1][y+1];
				}

				nOfN += field[x][y+1];
			}				

			if(cellState == DEAD)
			{
				if(nOfN == 3)
				{
					newField[x][y] = ALIVE;
				}
				else
				{
					newField[x][y] = DEAD;
				}
			}
			else
			{
				if(nOfN < 2 || nOfN > 3)
				{
					newField[x][y] = DEAD;
				}
				else
				{
					newField[x][y] = ALIVE;
				}
			}
		}
	}

	return newField;
}

int main()
{
	int SIZE = 0;

	cin >> SIZE;
	vector<vector<int> > field(SIZE, vector<int>(SIZE));

	
	
	// read field configuration from stdin
	for(int x = 0; x < SIZE; x++)
	{
		for(int y = 0; y < SIZE; y++)
		{
			cin >> field[x][y];
		}
	}
	
	while(true)
	{
		printOut(field);
		field = calc(field);
		
		// sleep for 1 second
		this_thread::sleep_for(chrono::milliseconds(500));
		printOut(field);
	}
	
	return 0;
}
