/// adapded from the ThermoFischer `Hello, world!` example provided by Jim Shofstahl 
/// see URL http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
/// the ThermoFisher library has to be manual downloaded and installed
/// Please read the License document
/// Christian Panse <cp@fgcz.ethz.ch> 
/// 2019-05-29 initial using rDotNet; added class rawDiag 
/// 2019-06-15 created MsBackendRawFileReader project
/// 2019-06-22 adapated to Spectra 0.1.0
/// 2019-06-23 added missing meta data


using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.ExceptionServices;
using System.Collections;
using System.Linq;
using ThermoFisher.CommonCore.Data;
using ThermoFisher.CommonCore.Data.Business;
using ThermoFisher.CommonCore.Data.FilterEnums;
using ThermoFisher.CommonCore.Data.Interfaces;
using ThermoFisher.CommonCore.MassPrecisionEstimator;
using ThermoFisher.CommonCore.RawFileReader;


namespace MsBackendRawFileReader
{
    public static class StringExtension
    {
        public static string CleanRawfileTrailerHeader(this string s)
        {
            return(s.Replace(" ", "")
                .Replace("#", "")
                .Replace("m/z", "mZ")
                .Replace("M/Z", "mZ")
                .Replace("(", "")
                .Replace(".", "")
                .Replace(")", "")
                .Replace(":", "")
                .Replace("-", "")
                .Replace("=", ""));
        }
    }

    public class Rawfile
    {
        private string _rawfile = "";

        private IEnumerable<int> scans;
        private IDictionary<string, string> dictInfo = new Dictionary<string, string>();
        private int masterScanIdx = -1;

        private IRawDataPlus rawFile;

        public Rawfile(string rawfile)
        {
            int deviceNumber = 1;
            _rawfile = rawfile;
            rawFile = RawFileReaderAdapter.FileFactory(_rawfile);
            
            rawFile.SelectInstrument(Device.MS, deviceNumber);

            dictInfo.Add("filename", rawFile.FileName);
            dictInfo.Add("creation date", rawFile.FileHeader.CreationDate.ToString());
            dictInfo.Add("first scan", this.getFirstScanNumber().ToString());
            dictInfo.Add("last scan", this.getLastScanNumber().ToString());
            dictInfo.Add("model", rawFile.GetInstrumentData().Model.ToString());
            dictInfo.Add("name", rawFile.GetInstrumentData().Name.ToString());
            dictInfo.Add("SerialNumber", rawFile.GetInstrumentData().SerialNumber.ToString());
            //dictInfo.Add("mass resolution", rawFile.RunHeaderEx.MassResolution.ToString());


            var trailerData = rawFile.GetTrailerExtraInformation(rawFile.RunHeaderEx.FirstSpectrum);
            foreach (int i in Enumerable.Range(1, trailerData.Labels.ToArray().Length))
            {
                try
                {
                    if ((trailerData.Labels[i] == "Master Scan Number:") ||
                        (trailerData.Labels[i] == "Master Scan Number") || (trailerData.Labels[i] == "Master Index:"))
                        this.masterScanIdx = i;
                }
                catch
                {
                }
            }
        }
        
      
	

        public bool check()
        {
            if (!rawFile.IsOpen || rawFile.IsError)
            {
                return false;
            }

            if (rawFile.IsError)
            {
                return false;
            }

            if (rawFile.InAcquisition)
            {
                return false;
            }

            this.scans = Enumerable.Range(this.getFirstScanNumber(), this.getLastScanNumber());



            return true;
        }

        public string[] GetInfoKeys()
        {
            return dictInfo.Keys.ToArray();
        }

        public string[] GetInfoValues()
        {
            return dictInfo.Values.ToArray();
        }


        public bool IsValidFilter(string filter)
        {
            if (rawFile.GetFilterFromString(filter) == null)
            {
                return false;
            }

            return true;
        }

        public string[] GetAutoFilters()
        {
            return rawFile.GetAutoFilters();
        }

        public string[] GetTrailerExtraHeaderInformationLabel()
        {
            List<string> rv = new List<string>();

            var scanTrailer = rawFile.GetTrailerExtraInformation(rawFile.RunHeaderEx.FirstSpectrum);

            foreach (var field in scanTrailer.Labels)
            {
                rv.Add(field.ToString().CleanRawfileTrailerHeader());

            }

            return rv.ToArray();
        }

        public string[] GetTrailerExtraHeaderInformationValue_(int scanNumber)
        {
            return rawFile.GetTrailerExtraInformation(scanNumber).Values;
        }

        public string[] GetTrailerExtraHeaderInformationValueAsString(int scanNumber)
        {
            List<string> rv = new List<string>();
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

            foreach (var field in scanTrailer.Values)
            {
                rv.Add(field);
            }

            return rv.ToArray();
        }

        public double[] GetTrailerExtraHeaderInformationValue(int scanNumber)
        {
            List<double> rv = new List<double>();
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

            foreach (var field in scanTrailer.Values)
            {
                try
                {
                    rv.Add(double.Parse(field));
                }
                catch
                {
                    rv.Add(-123456.0);

                }
            }

            return rv.ToArray();
        }

        public int getFirstScanNumber()
        {
            return (rawFile.RunHeaderEx.FirstSpectrum);
        }

        public int getLastScanNumber()
        {
            return (rawFile.RunHeaderEx.LastSpectrum);
        }

        public bool IsCentroidScan(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return (scanStatistics.IsCentroidScan);

        }

        public string GetScanFilter(int scanNumber)
        {
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);
            return scanFilter.ToString();
        }


        public string GetTitle(int scanNumber)
        {
            string s = "File: " + Path.GetFileName(_rawfile) + " SpectrumID: scans: " + scanNumber;
            return s;
        }


        public double[] GetPrecursorMzs()
        {
            List<double> rv = new List<double>();
            foreach (var scanNumber in this.scans)
            {
                rv.Add(GetPrecursorMz(scanNumber));
            }

            return rv.ToArray();
        }

        public int[] GetMasterScans()
        {
            List<int> rv = new List<int>();

            foreach (var scanNumber in this.scans)
            {
                try
                {
                    var masterScan =
                        rawFile.GetTrailerExtraInformation(scanNumber).Values.ToArray()[this.masterScanIdx];
                    rv.Add(Convert.ToInt32(masterScan));
                }
                catch
                {
                    rv.Add(-1);
                }
            }

            return rv.ToArray();
        }

        public double GetPrecursorMz(int scanNumber)
        {
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

            if (scanFilter.MSOrder.ToString() == "Ms") return -1.0;

            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);

            try
            {
                var reaction0 = scanEvent.GetReaction(0);

                return reaction0.PrecursorMass;
            }
            catch
            {
                return -1.0;
            }
        }

        public double GetCollisionEnergy(int scanNumber)
        {
            try
            {

                var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
                var reaction0 = scanEvent.GetReaction(0);
                return reaction0.CollisionEnergy;
            }
            catch
            {
                return -1.0;
            }
        }

        public double GetIsolationWidth(int scanNumber)
        {
            try
            {
                var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
                var reaction0 = scanEvent.GetReaction(0);

                return reaction0.IsolationWidth;
            }
            catch
            {
                return -1.0;
            }
        }

        public string GetScanType(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return scanStatistics.ScanType.ToString();
        }

        public double[] GetBasePeakMasses()
        {
            List<double> rv = new List<double>();

            foreach (var scanNumber in this.scans)
            {
                var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                try
                {
                    rv.Add(scanStatistics.BasePeakMass);
                }
                catch{
                    rv.Add(double.Parse(scanStatistics.BasePeakMass.ToString()));
                }
            }

            return rv.ToArray();
        }

        public double[] GetBasePeakIntensities()
        {
            List<double> rv = new List<double>();

            foreach (var scanNumber in this.scans)
            {
                var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                try
                {
                    rv.Add(scanStatistics.BasePeakIntensity);
                }
                catch{
                    rv.Add(double.Parse(scanStatistics.BasePeakIntensity.ToString()));
                }
            }

            return rv.ToArray();
        }
        
        public int[] GetCycleNumbers()
        {
            List<int> rv = new List<int>();

            foreach (var scanNumber in this.scans)
            {
                var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                try
                {
                    rv.Add(scanStatistics.CycleNumber);
                }
                catch{
                    rv.Add(Convert.ToInt32(scanStatistics.CycleNumber.ToString()));
                }
            }

            return rv.ToArray();
        }
        
        public double[] GetTotalIonCounts()
        {
            List<double> rv = new List<double>();

            foreach (var scanNumber in this.scans)
            {
                var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                try
		{
			rv.Add(scanStatistics.TIC);
		}
		catch{
                	rv.Add(double.Parse(scanStatistics.TIC.ToString()));
		}
            }

            return rv.ToArray();
        }

    public double[] GetRtime(){
            List<double> rv = new List<double>();
            foreach (var scanNumber in this.scans)
                    rv.Add(GetRTinSeconds(scanNumber));
            return rv.ToArray();
	}

        public double GetRTinSeconds(int scanNumber) {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000;
        }
        
        public double GetBasepeakMass(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return Math.Round(scanStatistics.BasePeakMass);
        }


        public double GetBasepeakIntensity(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return Math.Round(scanStatistics.BasePeakIntensity);
        }

        public string GetMonoisotopicMz(int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

            try
            {
                var idx = trailerFields
                    .Select((item, index) => new
                    {
                        name = item.Label.ToString().CleanRawfileTrailerHeader(),
                        Position = index
                    })
                    .First(x => x.name.Contains("MonoisotopicmZ")).Position;


                return scanTrailer.Values.ToArray()[idx];
            }
            catch
            {
                return null;
            }
        }

        public int GetPolarity(int scanNumber)
        {
            
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

            if (scanFilter.Polarity.ToString() == "Positive") return 1;
            
            return -1;
        }


        public double[] GetScanFrequency(string filterString = "", int n = 100)
        {
            List<double> rv = new List<double>();

            var filter = rawFile.GetFilterFromString(filterString);

            if (filter == null)
                return null;

            var filteredScanNumbers = rawFile.GetFilteredScanEnumerator(filter).ToArray();

            if (n > filteredScanNumbers.Length)
                n = filteredScanNumbers.Length;
            
            int stepsize = filteredScanNumbers.Length / n;
         
            double ortime = 0.0;
            
            for (int scanNumber = filteredScanNumbers[0]; scanNumber < filteredScanNumbers[filteredScanNumbers.Length-1]; scanNumber+=stepsize)
            {
                var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                var rtime = Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000;
                    
                rv.Add(rtime);
                rv.Add(stepsize / (rtime - ortime));

                ortime = rtime; 
            }
            return rv.ToArray();
        }
        
        public int[] GetMsLevels()
	{
            List<int> rv = new List<int>();

            foreach (var scanNumber in this.scans)
		    rv.Add(GetMsLevel(scanNumber));
		    
	    return rv.ToArray();
	}
        
        public int GetMsLevel(int scanNumber)
        {
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);
            
            if (scanFilter.MSOrder.ToString() == "Ms") return 1;
            else if (scanFilter.MSOrder.ToString() == "Ms2") return 2; 
            else if (scanFilter.MSOrder.ToString() == "Ms3") return 3; 
            else return -1;
        }


        public int[] GetCharges()
        {
            
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            
            var idx_CHARGE = trailerFields
                .Select((item, index) => new
                {
                    name = item.Label.ToString(),
                    Position = index
                })
                .First(x => x.name.Contains("Charge State")).Position;
                

            List<int> rv = new List<int>();
            foreach (var scanNumber in this.scans)
            {
                try
                {
                    rv.Add(Convert.ToInt32(
                        rawFile.GetTrailerExtraInformation(scanNumber).Values.ToArray()[idx_CHARGE]));
                }
                catch
                {
                    rv.Add(-123456);
                }
            }

           return rv.ToArray();
            
        }
        
        public string GetCharge(int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
            
            var idx_CHARGE = trailerFields
                .Select((item, index) => new
                {
                    name = item.Label.ToString(),
                    Position = index
                })
                .First(x => x.name.Contains("Charge State")).Position;
            
            return scanTrailer.Values.ToArray()[idx_CHARGE]; 
        }

	public double[] GetSpectrumNoises(int scanNumber, string scanFilter)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                return centroidStream.Noises.ToArray();
            }else{
                return null;
            }
        }

	public double[] GetSpectrumCharges(int scanNumber, string scanFilter)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                return centroidStream.Charges.ToArray();
            }else{
                return null;
            }
        }


	public double[] GetSpectrumBaselines(int scanNumber, string scanFilter)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                return centroidStream.Baselines.ToArray();
            }else{
                return null;
            }
        }
        
        
        public double[] GetSpectrumResolutions(int scanNumber, string scanFilter)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                return centroidStream.Resolutions.ToArray();
            }else{
                return null;
            }
        }
        
        

        public double[] GetSpectrumIntensities(int scanNumber, string scanFilter)
	{
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            var centroidStream = rawFile.GetCentroidStream(scanNumber, false);

            if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
            {
		return centroidStream.Intensities.ToArray();
	    }else{
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
		return segmentedScan.Intensities.ToArray();
	    }
	}
	

	
	    // TODO(cp): renmae to GetSpectrumMasses
        public double[] GetSpectrumMz(int scanNumber, string scanFilter)
        {

            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            var centroidStream = rawFile.GetCentroidStream(scanNumber, false);

            if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
            {
		return centroidStream.Masses.ToArray();
            }
            else
            {
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
		return segmentedScan.Positions.ToArray();
            }
        }
    }
}
