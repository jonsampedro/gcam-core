﻿#ifndef _BUILDING_GOMPERTZ_FUNCTION_H_
#define _BUILDING_GOMPERTZ_FUNCTION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file building_gompertz_function.h
* \ingroup Objects
* \brief GompertzDemandFunction class header file.
* \author	Jon Sampedro
*/

#include <string>
#include <vector>
#include "functions/include/aproduction_function.h"

class IInput;

/*! 
 * \ingroup Objects
 * \brief A function which defines demand behavior for a building size in terms
 *         of floorspace given the income, population density and base floorspace per capita value
 * \details The total floorspace is estimated as follows:
 *          Per capita floorspace=( unadjust_satiation − land_density_param ∗ log⁡( population/habitable_land))

									∗ exp(−base_floorspace_param ∗ log⁡(base per capita floorspace)

									∗ exp(−income_param ∗log⁡(per capita GDP) ) ) − bias_adjust_param
 *
 * \author Jon Sampedro
 */
class GompertzDemandFunction : public AProductionFunction {
public:
	double calcDemand(InputSet& input, double consumption, const std::string& regionName,
		const std::string& sectorName, const double aShutdownCoef, int period,
		double capitalStock = 0, double alphaZero = 0, double sigma = 0, double IBT = 0,
		const IInput* aParentInput = 0) const;


	static const std::string& getXMLNameStatic();

	// INamed methods

		// IParsable methods

	virtual const std::string& getName() const;

	virtual void toDebugXML(const int aPeriod,
		std::ostream& aOut,
		Tabs* aTabs) const;

	virtual const std::string& getXMLReportingName() const;

	virtual void XMLParse(const xercesc::DOMNode* aNode);

protected:

	DEFINE_DATA(

		DEFINE_SUBCLASS_FAMILY(GompertzDemandFunction),

		//! Current Subregional population.  Note that this is just a
		//! temporary value used during demand calculations
		DEFINE_VARIABLE(SIMPLE, "subregional-population", mCurrentSubregionalPopulation, Value),

		//! Current Subregional income.  Note that this is just a
		//! temporary value used during demand calculations
		DEFINE_VARIABLE(SIMPLE, "subregional-income", mCurrentSubregionalIncome, Value),

		//! The unadjusted satiation level to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "unadjust-satiation", mUnadjustSatiation, Value),

		//! The habitable land to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "habitable-land", mHabitableLand, Value),

		//! The base pcFlsp to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "base-pcFlsp", mBasepcFlsp, Value),

		//! The land density parameter to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "land-density-param", mLandDensityParam, Value),

		//! The base floorspace parameter to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "base-floorspace-param", mBaseFloorspaceParam, Value),

		//! The income parameter to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "income-param", mIncomeParam, Value),

		//! The bias correction parameter to use during calcDemand. Parsed from XML
		DEFINE_VARIABLE(SIMPLE | STATE, "bias-adjust-param", mBiasAdjustParam, Value)

	)

		void copy(const GompertzDemandFunction& aOther);
}



    
  

#endif // _BUILDING_FUNCTION_H_
