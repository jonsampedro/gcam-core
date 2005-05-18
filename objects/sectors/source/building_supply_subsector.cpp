/*! 
* \file building_supply_subsector.cpp
* \ingroup CIAM
* \brief The building supply subsector
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/building_supply_subsector.h"
#include "technologies/include/building_supply_technology.h"

using namespace std;
using namespace xercesc;

// static initialize.
const string BuildingSupplySubSector::XML_NAME = "buildingsupplysubsector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

BuildingSupplySubSector::BuildingSupplySubSector( const string regionName, const string sectorName ) : Subsector( regionName, sectorName ){

}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplySubSector::getXMLName() const {
	return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& BuildingSupplySubSector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Parses any input variables specific to derived classes
*
*/
bool BuildingSupplySubSector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {

    return false;
}

//! Virtual function which specifies the XML name of the children of this class, the type of technology.
bool BuildingSupplySubSector::isNameOfChild  ( const string& nodename ) const {
    return nodename == BuildingSupplyTechnology::getXMLNameStatic1D();
}

//! Virtual function to generate a child element or construct the appropriate technology.
technology* BuildingSupplySubSector::createChild( const string& nodename ) const {
    return new BuildingSupplyTechnology();
}