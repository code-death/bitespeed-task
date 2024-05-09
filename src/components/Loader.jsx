import Xlviloader from "./xlviloader.jsx";
import React from "react";

const Loader = () => {

    return (
        <div className={'loader-background'}>
            <Xlviloader
                boxColors={["#FFF", "#1a1a1a", "#747bff"]}
                desktopSize={"128px"}
                mobileSize={"100px"}
            />
        </div>
    )
}

export default Loader
