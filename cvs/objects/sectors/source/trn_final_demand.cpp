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
 * \file trn_final_demand.cpp
 * \ingroup Objects
 * \brief TrnFinalDemand class source file.
 * \author Jon Sampedro
 */

#include <string>
#include <algorithm>

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/model_time.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "demographics/include/demographic.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/sector_utils.h"
#include "sectors/include/trn_final_demand.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Constructor.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
TrnFinalDemand::TrnFinalDemand():
{
}

/*! \brief Destructor.
*/
TrnFinalDemand::~TrnFinalDemand(){
}

const string& TrnFinalDemand::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& TrnFinalDemand::getXMLNameStatic() {
    const static string XML_NAME = "trn-final-demand";
    return XML_NAME;
}

const string& TrnFinalDemand::getName() const {
    return mName;
}

void TrnFinalDemand::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write the xml for the class members.
    XMLWriteElement( mBaseService[ aPeriod ], "base-service", aOut, aTabs );
    XMLWriteElement(mTrnCoef[ aPeriod ], "coef_trn", aOut, aTabs );
    XMLWriteElement(mSubregIncomeShare[ aPeriod ], "subregional-income-share", aOut, aTabs );
    XMLWriteElement(mSubregPopShare[ aPeriod ], "subregional-population-share", aOut, aTabs );

    toDebugXMLDerived( aPeriod, aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void TrnFinalDemand::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {

}

void TrnFinalDemand::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    if( mBaseService[ 0 ] < 0.0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Zero base service for demand sector " << mName
                << " in region " << aRegionName << "." << endl;
        mBaseService[ 0 ] = 1;
        mPreTechChangeServiceDemand[ 0 ] = 1;
    }
    // Make sure that we have income and price elasticities for each model period.
    // If not we should interpolate between model periods that we do have.
    

void TrnFinalDemand::initCalc( const string& aRegionName,
                                  const GDP* aGDP,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
}

/*! \brief Set the final demand for service into the marketplace after 
* calling the aggregate demand function.
*
* \detail Adding the demand for final services into the marketplace
*  is separted from the actual calculation of final service so that services can
*  be calculated and used without being adding to marketplace.
* \author Sonny Kim, Josh Lurz, Jon Sampedro
* \param string& aRegionName region name.
* \param GDP* aGDP object.
* \param Demographic* aDemographicss.
* \param aPeriod Model aPeriod
* 
*  * \brief A function that estimates total passenger transport demand (pass-km)
 *        given the subregional income and average prices
 * 
 * \details The total passenger transport demand is estimated as follows:
 *       
*       trnDemand = trnCoef * subregionalIncome(^IncElast?) * price(^Prelast?) * subregionalPopulation;
*/
void TrnFinalDemand::setFinalDemand( const string& aRegionName,
                                        const Demographic* aDemographics,
                                        const GDP* aGDP,
                                        const int aPeriod )
{
    // unit conversions to convert from thous ppl to ppl
    const double CONV_POP_THOUS = 1e3;

    
    // Subregional population
    double subregionalPopulation = aDemographics * mSubregPopShare * CONV_POP_THOUS;

    // Subregional income 
    double subregionalIncome = (aGDP * mSubregIncomeShare) / subregionalPopulation;

    // Price
    double price = getPricePaid(aRegionName, aPeriod);


    // Function
    double mServiceDemand = mTrnCoef * subregionalIncome * price * subregionalPopulation;


    // Set the service demand into the marketplace.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, mServiceDemand[ aPeriod ], aPeriod );
}


double TrnFinalDemand::getWeightedEnergyPrice(const string& aRegionName,
    const int aPeriod) const
{
    // TODO: this method is no longer used and should be dropped
    // when the macro module is introduced
    return 0.0;
}

void TrnFinalDemand::accept(IVisitor* aVisitor,
    const int aPeriod) const
{
    aVisitor->startVisitFinalDemand(this, aPeriod);
    aVisitor->endVisitFinalDemand(this, aPeriod);
}