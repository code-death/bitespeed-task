import React, {useEffect, useMemo, useState} from 'react';
import './css/home.css'
import initialRules from '../../constants/builder.json'
import FilterGroup from "./components/FilterGroup.jsx";
import FilterRule from "./components/FilterRule.jsx";
import _ from "lodash";
import {v4 as uuidv4} from 'uuid';
import {Button} from "antd";

const Home = () => {
    const [parsedRules, setParsedRules] = useState([])


    useEffect(() => {
        let data = window.localStorage.getItem('data')
        if(_.isEmpty(data)) {
            setParsedRules(generateArray(initialRules, Object.keys(initialRules)[0]));
        } else {
            setParsedRules(generateArray(JSON.parse(data), Object.keys(data)[0]));
        }
    }, []);

    const generateArray = (ruleGroup, op, ruleIndex) => {
        let nodeArray = [];
        let parentIndex = ruleIndex;

        if (ruleGroup?.OR) {
            ruleGroup.OR.map((rule, index) => {
                nodeArray.push(generateArray({...rule, parentIndex}, 'OR', index))
            })
        } else if (ruleGroup?.AND) {
            ruleGroup.AND.map((rule, index) => {
                nodeArray.push(generateArray({...rule, parentIndex}, 'AND', index))
            })
        } else {
            return {...ruleGroup, op, ruleIndex};
        }

        return nodeArray
    }

    function findSubArrayIndex(mainArray, subArray) {
        for (let i = 0; i < mainArray.length; i++) {
            const item = mainArray[i];

            if (Array.isArray(item) && item.length === subArray.length) {
                let matched = true;
                for (let j = 0; j < item.length; j++) {
                    if (JSON.stringify(item[j]) !== JSON.stringify(subArray[j])) {
                        matched = false;
                        break;
                    }
                }
                if (matched) return i;
            }
        }
        return -1;
    }

    const handleAddRule = (rules, op, parentIndex, depth, parent) => {
        let tempRules = [...parsedRules];

        if (depth === 0) {
            tempRules.push({
                type: "Text",
                value: "",
                op,
                parentIndex
            })
        }

        if (depth === 1) {
            tempRules[parentIndex].push({
                type: "Text",
                value: "",
                op,
                parentIndex
            })
        }

        if (depth === 2) {
            let supIndex = findSubArrayIndex(tempRules, parent);
            tempRules[supIndex][parentIndex].push({
                type: "Text",
                value: "",
                op,
                parentIndex
            })
        }

        setParsedRules(tempRules);
    };

    const handleAddGroup = (rules, op, parentIndex, depth, parent) => {
        let tempRules = [...parsedRules];

        if (depth === 0) {
            tempRules.push([
                {
                    type: "Dropdown",
                    value: "",
                    op: "OR",
                    parentIndex: tempRules.length
                }
            ])
        }

        if (depth === 1) {
            tempRules[parentIndex].push([
                {
                    type: "Dropdown",
                    value: "",
                    op: "OR",
                    parentIndex: tempRules[parentIndex].length
                }
            ])
        }

        setParsedRules(tempRules)
    }

    const handleDeleteRule = (rules, op, parentIndex, nodeNum, parent, ruleIndex) => {
        let tempRules = [...parsedRules];

        if (nodeNum === 0) {
            tempRules.splice(ruleIndex, 1);
        }

        if (nodeNum === 1) {
            tempRules[parentIndex].splice(ruleIndex, 1);
        }

        if (nodeNum === 2) {
            let supIndex = findSubArrayIndex(tempRules, parent);
            tempRules[supIndex][parentIndex].splice(ruleIndex, 1)
        }

        setParsedRules(tempRules);
    };

    const handleDeleteGroup = (rules, op, parentIndex, nodeNum, parent) => {
        let tempRules = [...parsedRules];

        if (nodeNum === 1) {
            let groupIndex = findSubArrayIndex(parent, rules)
            tempRules.splice(groupIndex, 1)
        }

        if(nodeNum === 2) {
            let groupIndex = findSubArrayIndex(parent, rules)
            let supIndex = findSubArrayIndex(tempRules, parent)
            tempRules[supIndex].splice(groupIndex, 1)
        }

        setParsedRules(tempRules);
    };

    const handleChangeOp = (rules, op, parentIndex, depth, parent, ruleIndex, value) => {
        let tempRules = [...parsedRules];

        if(depth === 0) {
            tempRules.forEach(rule => {
                if(!rule.length) {
                    rule.op = value
                }
            })
        }

        if(depth === 1) {
            tempRules[parentIndex].forEach(rule => {
                if(!rule.length) {
                    rule.op = value
                }
            })
        }

        if(depth === 2) {
            let supeIndex = findSubArrayIndex(tempRules, parent);
            tempRules[supeIndex][parentIndex].forEach(rule => {
                if(!rule.length) {
                    rule.op = value
                }
            })
        }

        setParsedRules(tempRules)
    }

    const handleChangeValue = (rules, op, parentIndex, depth, parent, ruleIndex, value) => {
        let tempRules = [...parsedRules];

        if (depth === 0) {
            tempRules[ruleIndex].value = value;
        }

        if(depth === 1) {
            tempRules[parentIndex][ruleIndex].value = value;
        }

        if(depth === 2) {
            let supIndex = findSubArrayIndex(tempRules, parent);
            tempRules[supIndex][parentIndex][ruleIndex].value = value;
        }

        setParsedRules(tempRules)
    }

    const handleSave = () =>{
        window.localStorage.setItem('data', JSON.stringify(parseArrayToObject((parsedRules))))
    }

    const parseArrayToObject = (array) => {
        if (Array.isArray(array)) {
            const obj = {};
            if (array.some(item => item.op === "AND")) {
                obj["AND"] = array.map(item => parseArrayToObject(item));
            } else {
                obj["OR"] = array.map(item => parseArrayToObject(item));
            }
            return obj;
        } else {
            const { type, value } = array;
            return { type, value };
        }
    };

    const generateUi = (rules, op, parentIndex = undefined, depth = 0, parent = undefined) => {
        if (depth < 3) {
            return (
                <FilterGroup
                    nodeNum={depth}
                    handleAddRule={() => handleAddRule(rules, op, parentIndex, depth, parent)}
                    handleAddGroup={() => handleAddGroup(rules, op, parentIndex, depth, parent)}
                    handleDeleteGroup={() => handleDeleteGroup(rules, op, parentIndex, depth, parent)}
                >
                    {
                        !_.isEmpty(rules) && rules.map((rule, index) => {
                            if (rule.length) {
                                return generateUi(rule, rule[0].op, rule[0].parentIndex, depth + 1, rules)
                            } else {
                                return (
                                    <FilterRule
                                        key={index} index={index} operator={op} rule={rule}
                                        handleDeleteRule={() => handleDeleteRule(rules, op, parentIndex, depth, parent, index)}
                                        handleChangeValue={(value) => handleChangeValue(rules, op, parentIndex, depth, parent, index, value)}
                                        handleChangeOp={(value) => handleChangeOp(rules, op, parentIndex, depth, parent, index, value)}
                                    />
                                )
                            }
                        })
                    }
                </FilterGroup>
            )
        }
    }

    return (
        <div className={'home-container'}>
            <div className={'rule-container'}>
                <Button onClick={handleSave} style={{position: 'relative', right: "0"}}>Save Data</Button>
                {!_.isEmpty(parsedRules) && generateUi(parsedRules, parsedRules[0]?.op)}
            </div>
        </div>
    )
}

export default Home
