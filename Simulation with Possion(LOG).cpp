#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <ctime>
#include <exception>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <random>
#include <string>
#include <utility>
#include <vector>
#include <map>

// model parameters
const int iFirstGen = 7;           //Number of parents in the first generation
const int i1stGNRP = 4;            //Number of non-related parents from the first generation
const int i1stGNRf = 2;            //Number of non-related parents from the first generation and they are females
const int NumRepeats = 300;
const int iClutchSize = 3;
const double dNestlingSur = 0.62;    //survival rate during the nestling stage
const double a = -0.65;            //parameter 1 for offspring survival and IC a = 0.05*2 because we need to use the reproductive coefficients betwen parents, which is 2 times of offspring IC
const double b = -0.06;              //parameter 2 for offspring survival and IC, the hatchng success when IC = 0.73+1*0.17
const double dMorRateAdu = 0.05;    //when assume uniform adult mortality rate regardless of age;
const double dMorRateJuv = 0.22;    //Juvenile mortality rate
const double dMorRateJuv2 = 0.05;
const double dNestSur = 0.62;



// Obtain seed from system clock
std::chrono::high_resolution_clock::time_point tp =
std::chrono::high_resolution_clock::now();
unsigned seed = static_cast<unsigned>(tp.time_since_epoch().count());


// Create and seed pseudo-random number generator
std::mt19937_64 rng;

enum Sex { female, male };

//Structure
class Individual {
public:
	int iAge = 0;
	Sex Sex_;
	std::pair<int, int> ParentsID;
	size_t iID;
	int iNumOffs;              //how many offspring it produced this round
	double dICadd1;                   //inbreeding coefficients+1
	int iPairYears = 0;                 //how many year they are together as a pair
	int iPairNumOffs;                  //how many offspring this pair has until now
	double dIC;
	std::vector<double> vMatrixCol;

	Individual() {
		Sex_ = Sex::female;
		iAge = 0;
	}

	void set_sex(const Sex& s) {
		Sex_ = s;
	}
	void set_ID(const int& ID) {
		iID = ID;
	}


private:
	int ID;
};

struct IC_track {
	int ID = -1;
	double diag = -1;
	int mother = -1;
	int father = -1;
	bool founder = false;

	IC_track(int x, bool init) : ID(x), founder(init) {                 //if the individual is founder, then diag value of the matrix is 1
		if (founder) diag = 1.0;
	};
	IC_track(int x, int m, int f) : ID(x), founder(false), mother(m), father(f) {

	};
};


double get_IC(std::vector<IC_track>& pop,
	int i, int j,
	std::map<std::pair<int, int>, double>& lookup) {
	if (i > j) std::swap(i, j);
	auto lookup_answ = lookup.find(std::pair<int, int>(i, j));
	if (lookup_answ != lookup.end()) {
		return lookup_answ->second; //return the second item in the lookup, which is the IC
	}

	double answ = 0.0;
	if (i == j) {
		answ = pop[i].diag;
	}
	else if (i < i1stGNRP && j < i1stGNRP) {
		answ = 0.0;   //the cell values on the upper triangle
	}
	else {
		if (j > pop.size()) throw "j out of bounds";
		auto m = pop[j].mother;
		auto f = pop[j].father;
		if (m > f) std::swap(m, f);

		double a_ip = get_IC(pop, m, i, lookup);
		double a_iq = get_IC(pop, f, i, lookup);

		answ = 0.5 * (a_ip + a_iq);
	}
	lookup.insert({ std::pair<int, int>{i, j}, answ });
	return answ;
}

void set_IC(std::vector<IC_track>& pop, int i,
	std::map<std::pair<int, int>, double>& lookup) {
	if (pop[i].diag > -1) return;

	auto m = pop[i].mother;
	auto f = pop[i].father;
	if (m > f) std::swap(m, f);  //swap the value of m and f?

	double a_pq = get_IC(pop, m, f, lookup);
	pop[i].diag = 1 + 0.5 * a_pq;
	return;
}


void update_IC(std::vector<IC_track>& pop,
	std::map<std::pair<int, int>, double>& lookup) {
	for (int i = static_cast<int>(pop.size()) - 1; i >= 0; --i) {
		set_IC(pop, i, lookup);
	}
}

void Set_offs(std::pair<Individual, Individual>& pairs,
	double& IC,
	std::vector<Individual>& voffspring,
	std::vector<Individual>& population,
	size_t& IDPool,
	std::vector< IC_track >& track)
{
	Individual Offs;
	std::bernoulli_distribution givesex(0.5);
	Offs.Sex_ = static_cast<Sex>(givesex(rng));   //give sex
	Offs.ParentsID.first = pairs.first.iID;
	Offs.ParentsID.second = pairs.second.iID;
	Offs.iID = IDPool;
	IDPool++;
	Offs.dIC = IC;
	track.push_back(IC_track(Offs.iID, pairs.first.iID, pairs.second.iID));
	population.push_back(Offs);
	voffspring.push_back(Offs);
}


void Makingpairs(const std::vector<Individual>& males,
	const std::vector<Individual>& females,
	std::vector< std::pair<Individual, Individual>>& pairs,
	std::vector<Individual>& singles)
{
	if (!males.empty() || !females.empty()) {
		size_t iNumPairs = males.size();
		if (males.size() > females.size()) {
			iNumPairs = females.size();
			for (size_t a = iNumPairs; a < males.size(); ++a)
				singles.push_back(males[a]);
		}
		else if (males.size() < females.size()) {
			iNumPairs = males.size();
			for (size_t b = iNumPairs; b < females.size(); ++b)
				singles.push_back(females[b]);
		}

		if (iNumPairs != 0) {
			for (size_t j = 0; j < iNumPairs; ++j) {
				std::pair<Individual, Individual> couple(males[j], females[j]);
				//couple.first.iPairYears = couple.second.iPairYears = 1; *this is not used for now, because it seems not useful. If I want to add this back again, I can go to the RemoveDeath function to update the marriage age
				pairs.push_back(couple);
			}
		}
	}

}

void OffsProduce(std::pair<Individual, Individual>& pairs,
	std::vector<Individual>& voffspring,
	std::vector<Individual>& population,
	size_t& IDPool,
	std::vector< IC_track >& track,
	std::map< std::pair<int, int>, double>& lookup_map)
{

	double IC = 0.5 * get_IC(track, pairs.first.iID, pairs.second.iID, lookup_map);

	std::bernoulli_distribution NestSur(dNestSur);

	if (NestSur(rng) == true) {
		//calculate the number of survived offspring
		if ((exp(a * IC + b)) > 0) {
			double dIdealNum = iClutchSize * (exp(a * IC + b));               //Ideal number of hatchings  Hs = a*IC+b
			std::poisson_distribution<int>NumOffs(dIdealNum * dNestlingSur);   //Number of fledglings = Number of hatching * Survival success of nestling stage
			int iSurvivor = NumOffs(rng);
			if (iSurvivor > 3) iSurvivor = 3;

			if (iSurvivor > 0) {
				//give parents ID, sibling ID, matrix expansion
				for (int i = 0; i < iSurvivor; ++i) {
					Set_offs(pairs, IC, voffspring, population, IDPool, track);
				}
			}
		}
	}
}

void RemoveDeath(std::vector<Individual >& singles,
	std::vector<Individual >& offspring,
	std::vector< std::pair< Individual, Individual> >& pairs,
	std::vector<Individual>& death, std::vector<Individual>& juveniel)
{
	std::binomial_distribution<size_t>MorAduS(singles.size(), dMorRateAdu);
	std::binomial_distribution<size_t>MorJuv(offspring.size(), dMorRateJuv);
	std::binomial_distribution<size_t>MorJuv2(juveniel.size(), dMorRateJuv2);  //for the juveniels who are 2 years old
	std::binomial_distribution<size_t>MorAduP((pairs.size() * 2), dMorRateAdu);

	//the number of  singles, juveniels, dead adults parents,and how many dead in pairs in those dead adults

	if (!singles.empty()) {
		shuffle(singles.begin(), singles.end(), rng);
		size_t DeadAduS = MorAduS(rng);
		if (DeadAduS != 0) {
			for (size_t n = 0; n < DeadAduS; ++n) {
				death.push_back(singles.back());   //move the dead one to death vector
				singles.pop_back();     //remove the dead Single adults
			}
		}
	}

	if (!offspring.empty()) {
		shuffle(offspring.begin(), offspring.end(), rng);
		size_t DeadJuv = MorJuv(rng);
		if (DeadJuv != 0) {
			for (size_t n = 0; n < DeadJuv; ++n) {
				death.push_back(offspring.back()); //move the dead one to death vector
				offspring.pop_back();      //remove the dead offspring
			}
		}
	}

	if (!juveniel.empty()) {
		shuffle(juveniel.begin(), juveniel.end(), rng);
		size_t DeadJuv = MorJuv2(rng);
		if (DeadJuv != 0) {
			for (size_t n = 0; n < DeadJuv; ++n) {
				death.push_back(juveniel.back()); //move dead ones to death vector
				juveniel.pop_back();      //remove the dead juvenile
			}
		}
	}

	if (!pairs.empty()) {
		shuffle(pairs.begin(), pairs.end(), rng);
		size_t  DeadAduP = MorAduP(rng);
		if (DeadAduP != 0) {
			std::binomial_distribution<size_t>MorAduPairDead(DeadAduP, dMorRateAdu);  //the pairs both dead in the same season
			size_t DeadPair = MorAduPairDead(rng);
			if (DeadPair > pairs.size()) DeadPair = pairs.size();    //in case of very very very very very small chance that DeadPair > pairs.size

			//remove the dead parents
			if (DeadPair != 0) {                                       //if there are pairs (both) have to die
				if (DeadPair > pairs.size()) DeadPair = pairs.size();
				for (size_t n = 0; n < DeadPair; ++n) pairs.pop_back();   // remove the dead pair(s)

				size_t RemaintoRemove = DeadAduP - 2 * DeadPair;          //how many parents need to be removed from the remaining pairs
				if (RemaintoRemove > pairs.size()) RemaintoRemove = pairs.size();

				for (size_t n = 0; n < RemaintoRemove; ++n) {  //move the alive one to vSingles, and move the dead completely out of the vPairs
					std::bernoulli_distribution Selectone(0.5);
					Selectone(rng) == 0 ? singles.push_back(pairs.back().first) : singles.push_back(pairs.back().second);
					singles.back().iPairYears = 0;
					singles.back().Sex_ == 0 ? death.push_back(pairs.back().first) : death.push_back(pairs.back().second); //put the death parent to the dead vector
					pairs.pop_back();
				}
			}

			else if (DeadAduP != 0 && DeadPair == 0) {
				if (DeadAduP > pairs.size()) DeadAduP = pairs.size();
				for (size_t n = 0; n < DeadAduP; ++n) {
					std::bernoulli_distribution Selectone(0.5);
					Selectone(rng) == 0 ? singles.push_back(pairs.back().first) : singles.push_back(pairs.back().second);
					singles.back().iPairYears = 0;
					singles.back().Sex_ == 0 ? death.push_back(pairs.back().first) : death.push_back(pairs.back().second); //put the death parent to the dead vector
					pairs.pop_back();
				}
			}
		}
	}
}

void CheckAge(std::vector< std::pair< Individual, Individual> >& pairs,
	std::vector<Individual >& males,
	std::vector<Individual >& females) {

	if (pairs.empty()) return;

	for (int i = pairs.size() - 1; i >= 0; --i) {
		if (pairs[i].first.iAge >= 20 && pairs[i].second.iAge < 20) {
			females.push_back(pairs[i].second);
			pairs[i] = pairs.back();
			pairs.pop_back();
		}
		else if (pairs[i].first.iAge < 20 && pairs[i].second.iAge >= 20) {
			males.push_back(pairs[i].first);
			pairs[i] = pairs.back();
			pairs.pop_back();
		}
		else if (pairs[i].first.iAge >= 20 && pairs[i].second.iAge >= 20) {
			pairs[i] = pairs.back();
			pairs.pop_back();
		}
	}

	for (int i = males.size() - 1; i >= 0; --i) {
		if (males[i].iAge >= 20) {
			males[i] = males.back();
			males.pop_back();
		}
	}

	for (int i = females.size() - 1; i >= 0; --i) {
		if (females[i].iAge >= 20) {
			females[i] = females.back();
			females.pop_back();
		}
	}
}


int main() {
	try
	{
		auto clock_start = std::chrono::system_clock::now();

		// draw random seed and log it

		rng.seed(seed);

		std::ofstream PopuInfo;
		// std::ofstream IC;

		PopuInfo.open("Population Info_check.csv");
		//IC.open("ICoutput.csv");

		PopuInfo << "Repeat" << "," << "Season" << "," << "PopulationSize" << "," << "HaveBorn" << "," << "AverageIC" << "," << "SD" << std::endl;
		// IC << "Repeat" << "," << "Season" << "," << "IC" << std::endl;
		 //Check if file is open, else error
		if (!PopuInfo.is_open())
			throw std::runtime_error("unable to open the file. \n");


		for (int iRepeat = 1; iRepeat < NumRepeats + 1; ++iRepeat) {
			std::vector<Individual> vPopulation;     //vICmatrix includes all individuals count in the ICmatrix
			//vPopulation includes all individuals in the population
			size_t IDPool = 0;
			std::vector<Individual> vFemales, vMales, vSingles, vOffspring, vDeath, vJuvenile;
			std::vector< std::pair< Individual, Individual> > vPairs;
			std::vector<double>vIC;    //used for store IC of breeding individuals in each year

			std::map< std::pair<int, int>, double> lookup_map;
			std::vector< IC_track > track;

			//create the first generation
			for (int i = 0; i < i1stGNRP; ++i) {
				Individual FirstGen;
				FirstGen.set_ID(IDPool);      //give ID from the IDpool in order
				IDPool++;
				FirstGen.dIC = 0.0;
				i < i1stGNRf ? FirstGen.set_sex(Sex::male) : FirstGen.set_sex(Sex::female);  //give sex to the first four non-related parents
				FirstGen.iAge = 1;
				vPopulation.push_back(FirstGen);

				track.push_back(IC_track(FirstGen.iID, true));
			}

			update_IC(track, lookup_map);

			//I need to add three offspring with random sex to the matrix and the 1st Gen population
			vPairs.push_back(std::pair<Individual, Individual>(vPopulation[0], vPopulation[2]));
			vPairs.push_back(std::pair<Individual, Individual>(vPopulation[1], vPopulation[3]));


			double IC = 0.5 * get_IC(track, vPairs[0].first.iID, vPairs[0].second.iID, lookup_map);
			for (int j = 0; j < 3; ++j) {      //the first pair reproduced one offspring as a founder; the second pair reproduced two offspring as founders. 
				Set_offs(vPairs[0], IC, vOffspring, vPopulation, IDPool, track);
			}



			size_t Season = 0;
			double dAvgIC;
			//making breeding pairs
			for (; ; ++Season) {
				shuffle(vMales.begin(), vMales.end(), rng);
				shuffle(vFemales.begin(), vFemales.end(), rng);
				Makingpairs(vMales, vFemales, vPairs, vSingles); //radomly pairing up


				//Update age
				for (size_t n = 0; n < vPairs.size(); ++n) {
					++vPairs[n].first.iAge;
					++vPairs[n].second.iAge;
					++vPairs[n].first.iPairYears;
					++vPairs[n].second.iPairYears;
				}
				for (size_t m = 0; m < vSingles.size(); ++m) ++vSingles[m].iAge;

				//mark parents ID for each new baby and put them to the offspring pool, extend ICmatrix
				for (size_t i = 0; i < vPairs.size(); ++i)
					OffsProduce(vPairs[i], vOffspring, vPopulation, IDPool, track, lookup_map);   //give ID, parents ID, sex

				//remove dead individuals and rearrange the survival ones survived parents and offspring over the season
				RemoveDeath(vSingles, vOffspring, vPairs, vDeath, vJuvenile);

				//clear the mating pool
				vMales.clear(); vFemales.clear();                           //clear mating pool for this season

				//add singles and offspring to the mating pool of the next season
				if (!vSingles.empty()) {
					for (size_t i = 0; i < vSingles.size(); ++i)
						vSingles[i].Sex_ == Sex::female ? vFemales.push_back(vSingles[i]) : vMales.push_back(vSingles[i]);
				}
				if (!vJuvenile.empty()) {
					for (size_t i = 0; i < vJuvenile.size(); ++i)            //add offspring to the mating pool for the next season
						vJuvenile[i].Sex_ == Sex::female ? vFemales.push_back(vJuvenile[i]) : vMales.push_back(vJuvenile[i]);
				}

				vJuvenile.clear();
				vSingles.clear();

				if (!vOffspring.empty()) {
					for (const auto& i : vOffspring) vJuvenile.push_back(i);
				}

				vOffspring.clear();

				//check adult age
				CheckAge(vPairs, vMales, vFemales);

				//#############OUTPUT########################
				//calculate the average IC of breeders in this season
				//first push back all the inviduals who is alive to the vIC vector
				if (!vPairs.empty()) {
					for (const auto& i : vPairs) {
						vIC.push_back(i.first.dIC);
						vIC.push_back(i.second.dIC);
					}
				}
				if (!vMales.empty()) { for (const auto& i : vMales) vIC.push_back(i.dIC); }
				if (!vFemales.empty()) { for (const auto& i : vFemales) vIC.push_back(i.dIC); }

				double sdIC=0.0;
				double dAvgIC=0.0;
				if (!vIC.empty()) {
					double dSum = 0.0;
					for (size_t i = 0; i < vIC.size(); ++i) dSum += vIC[i];
					dAvgIC = dSum / vIC.size();

					double dSumVar = 0.0;
					for (size_t i = 0; i < vIC.size(); ++i) dSumVar += (vIC[i] - dAvgIC) * (vIC[i] - dAvgIC);
					sdIC = sqrt(dSumVar / (vIC.size() - 1));
				}

				//Output population info
				PopuInfo << iRepeat << "," << Season << "," << (vPairs.size() * 2 + vMales.size() + vFemales.size() + vJuvenile.size()) << "," << vPopulation.size() << "," << dAvgIC << "," << sdIC << std::endl;
				//std::cout << "Season" << Season << ", vPairs size:" << vPairs.size() * 2 << ", vMales:" << vMales.size() << ", vFemales: " << vFemales.size() << std::endl;

				//output IC every 8 seasons
				//if (Season % 8 == 0) {
			   //     for (size_t i = 0; i < vIC.size(); ++i)
			   //         if (i % 5 == 0)    IC << iRepeat << "," << Season << "," << vIC[i] << std::endl;
			   // }

				if (vPairs.size() * 2 + vMales.size() + vFemales.size() + vJuvenile.size() > 9000) break;
				if ((vPairs.size() + vMales.size() + vFemales.size() + vJuvenile.size() == 0) || (vPairs.size() == 0 && (vMales.size() == 0 || vFemales.size() == 0) && vJuvenile.size() == 0)) {
					std::cout << "Population extinct!";
					break;
				}

				vIC.clear();

				update_IC(track, lookup_map);
			}

			//output IC for the last generation
		   // for (size_t i = 0; i < vIC.size(); ++i)
		   //     IC << iRepeat << "," << Season << "," << vIC[i] << std::endl;

			std::cout << "End:" << iRepeat << ", Season: " << Season << ", Population size:" << vPopulation.size() << ", " << "Current size: " << vPairs.size() * 2 + vFemales.size() + vMales.size() + vJuvenile.size() << std::endl;
			//std::cout << "Pairsize:" << vPairs.size() << ", Female size" << vFemales.size() << ", Male size " << vMales.size() << ", AvgIC " << dAvgIC << std::endl;

			vPopulation.clear(); vMales.clear(); vFemales.clear(); vPairs.clear();
			vDeath.clear();
		}


		PopuInfo.close();
		//IC.close();
	}

	catch (std::exception& error)
	{
		std::cerr << error.what();
		exit(EXIT_FAILURE);
	}
	return 0;
}
